# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology
# Modified: I. Kavcic, O. Brunt and L. Turner, Met Office

'''This module provides functionality for the PSyclone kernel extraction
functionality for LFRic. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

# TODO #1382: refactoring common functionality between the various driver
# creation implementation should make this file much smaller.
# pylint: disable=too-many-lines

from psyclone.configuration import Config
from psyclone.domain.common import BaseDriverCreator
from psyclone.domain.lfric import LFRicConstants
from psyclone.line_length import FortLineLength
from psyclone.parse import ModuleManager
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (Assignment, Call, FileContainer,
                                  IntrinsicCall, Literal, Reference,
                                  Routine, ExtractNode)
from psyclone.psyir.symbols import (ArrayType, CHARACTER_TYPE,
                                    ContainerSymbol, DataSymbol,
                                    DataTypeSymbol, UnresolvedType,
                                    ImportInterface, INTEGER_TYPE,
                                    RoutineSymbol, UnsupportedFortranType,
                                    AutomaticInterface)
from psyclone.psyir.transformations import ExtractTrans


class LFRicExtractDriverCreator(BaseDriverCreator):
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.

    The driver is created as follows:

    1. The corresponding :py:class:`psyclone.psyGen.Invoke` statement that
       contains the kernel(s) is copied. This way we avoid affecting the tree
       of the caller. We need the invoke since it contains the symbol table.
    2. We remove all halo exchange nodes.
    3. We lower each kernel (child of the invoke) that was requested to
       be extracted, all others are removed. This is required since the kernel
       extraction will not contain the required data for the other kernels to
       be called. The lowering is important to fix the variable names for the
       loop boundaries of the :py:class:`psyclone.domain.lfric.LFRicLoop`: the
       loop start/stop expressions (`loop0_start` etc.) depend on the position
       of the loop in the tree. For example, if there are two kernels, they
       will be using `loop0_start` and `loop1_start`. If only the second is
       extracted, the former second (and now only) loop would be using
       `loop0_start` without lowering, but the kernel extraction would have
       written the values for `loop1_start`.
    4. We create a program for the driver with a new symbol table and start
       adding symbols for the program unit, precision symbols, PSyData read
       module etc to it.
    5. We add all required symbols to the new symbol table. The copied tree
       will still rely on the symbol table in the original PSyIR, so the
       symbols must be declared in the symbol table of the driver program.
       This is done by replacing all references in the extracted region with
       new references, which use new symbols which are declared in the driver
       symbol table.

       a. We first handle all non user-defined type. We can be certain that
          these symbols are already unique (since it's the original kernel
          code).
       b. Then we handle user-defined types. Since we only use basic Fortran
          types, accesses to these types need to be 'flattened': an access
          like ``a%b%c`` will be flattened to ``a_b_c`` to create a valid
          symbol name without needing the user-defined type. We use the
          original access string (``a%b%c``) as tag, since we know this tag
          is unique, and create a new, unique symbol based on ``a_b_c``. This
          takes care if the user should be using this newly generated name
          (e.g. if the user uses ``a%b%c`` and ``a_b_c``, ``a_b_c`` as non
          user defined symbol will be added to the symbol table first. When
          then ``a%b%c`` is flattened, the symbol table will detect that the
          symbol ``a_b_c`` already exists and create ``a_b_c_1`` for the tag
          ``a%b%c``). For known LFRic types, the actual name used in a
          reference will be changed to the name the user expects. For example,
          if field ``f`` is used, the access will be ``f_proxy%data``. The
          kernel extraction does the same and stores the values under the name
          ``f``, so the driver similarly simplifies the name back to the
          original ``f``.
          The :py:class:`psyclone.domain.lfric.KernCallArgList` class will
          have enforced the appropriate basic Fortran type declaration for
          each reference to a user defined variable. For example, if a field
          ``f`` is used, the reference to ``f_proxy%data`` will have a data
          type attribute of a 1D real array (with the correct precision).

    6. We create the code for reading in all of the variables in the input-
       and output-lists. Mostly, no special handling of argument type is
       required (since the generic interface will make sure to call the
       appropriate function). But in case of user-defined types, we need to
       use the original names with '%' when calling the functions for reading
       in data, since this is the name that was used when creating the data
       file. For example, the name of a parameter like
       ``f_proxy%local_stencil`` will be stored in the data file with the
       '%' notation (which is also the tag used for the symbol). So when
       reading the values in the driver, we need to use the original name
       (or tag) with '%', but the values will be stored in a flattened
       variable. For example, the code created might be:
       `call extract_psy_data%ReadVariable('f_proxy%local_stencil',
       fproxy_local_stencil)`

       a. Input variables are read in using functions from the PSyData
          ``ReadKernelData`` module. These function will allocate all array
          variables to the right size based on the data from the input file.
       b. For parameters that are read and written, two variables will be
          declared: the input will be stored in the unmodified variable name,
          and the output values in a variable with ``_post`` appended. For
          example, a field ``f`` as input will be read into ``f`` using the
          name ``f``, and output values will be read into ``f_post`` using
          the name ``f_post``. The symbol table will make sure that the
          ``_post`` name is unique.
       c. Similar to b., output only parameters will be read into a variable
          named with '_post' attached, e.g. output field ``f`` will be stored
          in a variable ``f_post``. Then the array ``f`` is allocated based on
          the shape of ``f_post`` and initialised to 0 (since it's an
          output-only parameter the value doesn't really matter).

    7. The extracted kernels are added to the program. Since in step 5 all
       references have been replaced, the created code will use the correct
       new variable names (which just have been read in). The output variables
       with ``_post`` attached will not be used at all so far.
    8. After the kernel calls are executed, each output variable is compared
       with the value stored in the corresponding ``_post`` variable. For
       example, a variable ``f`` which was modified in the kernel call(s),
       will then be compared with ``f_post``.

    :param precision: a mapping of the various precisions used in LFRic to
        the actual Fortran data type to be used in a stand-alone driver.
    :type precision: Optional[Dict[str, str]]

    :raises InternalError: if the precision argument is specified but
        is not a dictionary.

    '''
    def __init__(self, region_name=None):
        super().__init__()
        # TODO #2069: check if this list can be taken from LFRicConstants
        # TODO #2018: once r_field is defined in the LFRic infrastructure,
        #             it should be added to this list.
        self._region_name = region_name
        self._all_field_types = ["integer_field_type", "field_type",
                                 "r_bl_field", "r_solver_field_type",
                                 "r_tran_field_type"]

    # -------------------------------------------------------------------------
    @staticmethod
    def _make_valid_unit_name(name):
        '''Valid program or routine names are restricted to 63 characters,
        and no special characters like '-' (which is used when adding
        invoke and region numbers).

        :param str name: a proposed unit name.

        :returns: a valid program or routine  name with special characters
            removed and restricted to a length of 63 characters.
        :rtype: str

        '''
        return name.replace("-", "")[:63]

    # -------------------------------------------------------------------------
    @staticmethod
    def _create_output_var_code(name, program, is_input, read_var,
                                postfix, index=None, module_name=None):
        # pylint: disable=too-many-arguments
        '''
        This function creates all code required for an output variable.
        It creates the '_post' variable which stores the correct result
        from the file, which is read in. If the variable is not also an
        input variable, the variable itself will be declared (based on
        the size of the _post variable) and initialised to 0.
        This function also handles array of fields, which need to get
        an index number added.
        If a module_name is specified, this indicates that this variable
        is imported from an external module. The name of the module will
        be appended to the tag used in the extracted kernel file, e.g.
        `dummy_var2@dummy_mod`.

        :param str name: the name of original variable (i.e.
            without _post), which will be looked up as a tag in the symbol
            table. If index is provided, it is incorporated in the tag using
            f"{name}_{index}_data".
        :param program: the PSyIR Routine to which any code must
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param bool is_input: True if this variable is also an input
            parameter.
        :param str read_var: the readvar method to be used including the
            name of the PSyData object (e.g. 'psy_data%ReadVar')
        :param str postfix: the postfix to use for the expected output
            values, which are read from the file.
        :param index: if present, the index to the component of a field vector.
        :type index: Optional[int]
        :param str module_name: if the variable is part of an external module,
            this contains the module name from which it is imported.
            Otherwise, this must either not be specified or an empty string.

        :returns: a 2-tuple containing the output Symbol after the kernel,
             and the expected output read from the file.
        :rtype: Tuple[:py:class:`psyclone.psyir.symbols.Symbol`,
                      :py:class:`psyclone.psyir.symbols.Symbol`]

        '''
        # For each variable that is written, we need to declare a new variable
        # that stores the expected value which is contained in the kernel data
        # file, which has `_post` appended to the name (so `a` is the variable
        # that is written, and `a_post` is the corresponding variable that
        # has the expected results for verification). Since the written
        # variable and the one storing the expected results have the same
        # type, look up the 'original' variable and declare the _POST variable
        symbol_table = program.symbol_table
        if module_name:
            sym = symbol_table.lookup_with_tag(f"{name}@{module_name}")
        else:
            sym = symbol_table.lookup(name)

        # Declare a 'post' variable of the same type and read in its value.
        post_name = sym.name + postfix
        post_sym = symbol_table.new_symbol(post_name,
                                           symbol_type=DataSymbol,
                                           datatype=sym.datatype.copy())
        if isinstance(post_sym.datatype, UnsupportedFortranType):
            post_sym.datatype = post_sym.datatype.copy()
            post_sym.datatype._declaration = \
                post_sym.datatype._declaration.replace(sym.name, post_name)

        if module_name:
            post_tag = f"{name}{postfix}@{module_name}"
        else:
            if index is not None:
                post_tag = f"{name}{postfix}%{index}"
            else:
                # If it is not indexed then `name` will already end in "_data"
                post_tag = f"{name}{postfix}"
        name_lit = Literal(post_tag, CHARACTER_TYPE)

        if sym.is_array and not sym.datatype.is_allocatable:
            # In case of a non-allocatable array (e.g. a constant
            # size array from a module), call the ReadVariable
            # function that does not require an allocatable field
            BaseDriverCreator.add_call(program, read_var+"NonAlloc",
                                       [name_lit, Reference(post_sym)])
        else:
            # In case of an allocatable array, call the ReadVariable
            # function that will also allocate this array.
            BaseDriverCreator.add_call(program, read_var,
                                       [name_lit, Reference(post_sym)])

        # Now if a variable is written to, but not read, the variable
        # is not allocated. So we need to allocate it and set it to 0.
        if not is_input:
            if (isinstance(post_sym.datatype, ArrayType) or
                    (isinstance(post_sym.datatype, UnsupportedFortranType) and
                     isinstance(post_sym.datatype.partial_datatype,
                                ArrayType))):
                alloc = IntrinsicCall.create(
                    IntrinsicCall.Intrinsic.ALLOCATE,
                    [Reference(sym), ("mold", Reference(post_sym))])
                program.addchild(alloc)
            set_zero = Assignment.create(Reference(sym),
                                         Literal("0", INTEGER_TYPE))
            program.addchild(set_zero)
        return (sym, post_sym)

    # -------------------------------------------------------------------------
    def _create_read_in_code(self, program, psy_data, original_symbol_table,
                             read_write_info, postfix):
        # pylint: disable=too-many-arguments, too-many-branches
        # pylint: disable=too-many-locals, too-many-statements
        '''This function creates the code that reads in the NetCDF file
        produced during extraction. For each:

        - variable that is read-only, it will declare the symbol and add code
          that reads in the variable using the PSyData library.
        - variable that is read and written, it will create code to read in the
          variable that is read, and create a new variable with the same name
          and "_post" added which is read in to store the values from the
          NetCDF file after the instrumented region was executed. In the end,
          the variable that was read and written should have the same value
          as the corresponding "_post" variable.
        - variable that is written only, it will create a variable with "_post"
          as postfix that reads in the output data from the NetCDF file. It
          then also declares a variable without postfix (which will be the
          parameter to the function), allocates it based on the shape of
          the corresponding "_post" variable, and initialises it with 0.

        :param program: the PSyIR Routine to which any code must
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param psy_data: the PSyData symbol to be used.
        :type psy_data: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str postfix: a postfix that is added to a variable name to
            create the corresponding variable that stores the output
            value from the kernel data file.

        :returns: all output parameters, i.e. variables that need to be
            verified after executing the kernel. Each entry is a 2-tuple
            containing the symbol of the computed variable, and the symbol
            of the variable that contains the value read from the file.
        :rtype: List[Tuple[:py:class:`psyclone.psyir.symbols.Symbol`,
                           :py:class:`psyclone.psyir.symbols.Symbol`]]

        '''
        symbol_table = program.scope.symbol_table
        read_var = f"{psy_data.name}%ReadVariable"
        mod_man = ModuleManager.get()

        # First handle variables that are read:
        # -------------------------------------
        read_stmts = []
        for module_name, signature in read_write_info.read_list:
            if module_name:
                continue
            orig_sym = original_symbol_table.lookup(signature[0])
            sym = orig_sym.copy()
            sym.interface = AutomaticInterface()
            if symbol_table.lookup(sym.name, otherwise=None) is not None:
                # We can edit the name because we know the copied symbol is
                # not in a symbol table yet
                sym._name = symbol_table.next_available_name(sym.name)
            symbol_table.add(sym)
            name_lit = Literal(str(signature), CHARACTER_TYPE)
            read_stmts.append((name_lit, sym))

        ExtractNode._bring_external_symbols(read_write_info,
                                            program.scope.symbol_table)
        for module_name, signature in read_write_info.read_list:
            if not module_name:
                continue
            mod_info = mod_man.get_module_info(module_name)
            orig_sym = mod_info.get_symbol(signature[0])
            tag = f"{signature[0]}@{module_name}"
            sym = symbol_table.lookup_with_tag(tag)
            name_lit = Literal(tag, CHARACTER_TYPE)
            read_stmts.append((name_lit, sym))

        for name_lit, sym in read_stmts:
            # TODO #2898: the test for array can be removed if
            # `is_allocatable` is supported for non-arrays.
            if sym.is_array and not sym.datatype.is_allocatable:
                # In case of a non-allocatable array (e.g. a constant
                # size array from a module), call the ReadVariable
                # function that does not require an allocatable field
                self.add_call(program, read_var+"NonAlloc",
                              [name_lit, Reference(sym)])
            else:
                # In case of an allocatable array, call the ReadVariable
                # function that will also allocate this array.
                self.add_call(program, read_var,
                              [name_lit, Reference(sym)])

        # Then handle all variables that are written (note that some
        # variables might be read and written)
        # ----------------------------------------------------------
        # Collect all output symbols to later create the tests for
        # correctness. This list stores 2-tuples: first one the
        # variable that stores the output from the kernel, the second
        # one the variable that stores the output values read from the
        # file. The content of these two variables should be identical
        # at the end.
        output_symbols = []

        for module_name, signature in read_write_info.write_list:
            # Find the right symbol for the variable. Note that all variables
            # in the input and output list have been detected as being used
            # when the variable accesses were analysed. Therefore, these
            # variables have References, and will already have been declared
            # in the symbol table (in _add_all_kernel_symbols).
            if module_name:
                orig_sym = mod_man.get_module_info(module_name).get_symbol(
                    signature[0])
                if not orig_sym:
                    # TODO 2120: We likely couldn't parse the module.
                    print(f"Error finding symbol '{signature}' in "
                          f"'{module_name}'.")
                    continue
            else:
                orig_sym = original_symbol_table.lookup(signature[0])
            is_input = read_write_info.is_read(signature)
            sym_tuple = \
                self._create_output_var_code(str(signature), program,
                                             is_input, read_var, postfix,
                                             module_name=module_name)
            output_symbols.append(sym_tuple)

        return output_symbols

    # -------------------------------------------------------------------------
    @staticmethod
    def _import_modules(symbol_table, sched):
        '''This function adds all the import statements required for the
        actual kernel calls. It finds all calls in the schedule and
        checks for calls with an ImportInterface. Any such call will
        add a ContainerSymbol for the module and a RoutineSymbol (pointing
        to the container) to the symbol table.

        :param symbol_table: the symbol table to which the symbols are added.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param sched: the schedule to analyse for module imports.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        for call in sched.walk(Call):
            routine = call.routine.symbol
            if not isinstance(routine.interface, ImportInterface):
                # No import required, can be ignored.
                continue
            if routine.name in symbol_table:
                # Symbol has already been added - ignore
                continue
            # We need to create a new symbol for the module and the routine
            # called (the PSyIR backend will then create a suitable import
            # statement).
            module = ContainerSymbol(routine.interface.container_symbol.name)
            symbol_table.add(module)
            new_routine_sym = RoutineSymbol(routine.name, UnresolvedType(),
                                            interface=ImportInterface(module))
            symbol_table.add(new_routine_sym)

    # -------------------------------------------------------------------------
    @staticmethod
    def _add_precision_symbols(symbol_table):
        '''This function adds an import of the various precision
        symbols used by LFRic from the constants_mod module.

        :param symbol_table: the symbol table to which the precision symbols
            must be added.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        const = LFRicConstants()
        mod_name = const.UTILITIES_MOD_MAP["constants"]["module"]
        constant_mod = ContainerSymbol(mod_name)
        symbol_table.add(constant_mod)

        # r_quad is defined in constants_mod, but not exported. And r_phys
        # does not exist at all in LFRic. So we have to remove them from the
        # lists of precisions to import.  TODO #2018
        api_config = Config.get().api_conf("lfric")
        all_precisions = [name for name in api_config.precision_map
                          if name not in ["r_quad", "r_phys"]]
        for prec_name in all_precisions:
            symbol_table.new_symbol(prec_name,
                                    tag=f"{prec_name}@{mod_name}",
                                    symbol_type=DataSymbol,
                                    datatype=INTEGER_TYPE,
                                    interface=ImportInterface(constant_mod))

    # -------------------------------------------------------------------------
    def _add_command_line_handler(self, program, psy_data_var, module_name,
                                  region_name):
        '''
        This function adds code to handle the command line. For now an
        alternative filename (to the default one that is hard-coded by
        the created driver) can be specified, which allows the driver to
        be used with different files, e.g. several dumps from one run, and/or
        a separate file from each process. It will also add the code to
        open the input file using the read_kernel_data routine from the
        extraction library.

        :param program: The driver PSyIR.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param psy_data_var: the symbol of the PSyDataExtraction type.
        :type psy_data_var: :py:class:`psyclone.psyir.symbols.Symbol`
        :param str module_name: the name of the module, used to create the
            implicit default kernel dump file name.
        :param str region_name: the name of the region, used to create the
            implicit default kernel dump file name.

        '''
        # pylint: disable=too-many-locals
        program_symbol_table = program.symbol_table

        # PSyIR does not support allocatable strings, so create the two
        # variables we need in a loop.
        # TODO #2137: The UnsupportedFortranType could be reused for all
        #             variables once this is fixed.
        for str_name in ["psydata_filename", "psydata_arg"]:
            str_unique_name = \
                program_symbol_table.next_available_name(str_name)
            str_type = UnsupportedFortranType(
                f"character(:), allocatable :: {str_unique_name}")
            sym = DataTypeSymbol(str_unique_name, str_type)
            program_symbol_table.add(sym)
            if str_name == "psydata_filename":
                psydata_filename = str_unique_name
            else:
                psydata_arg = str_unique_name

        psydata_len = \
            program_symbol_table.find_or_create("psydata_len",
                                                symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE).name
        psydata_i = \
            program_symbol_table.find_or_create("psydata_i",
                                                symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE).name
        # We can only parse one statement at a time, so start with the
        # command line handling:
        code = f"""
        do {psydata_i}=1,command_argument_count()
           call get_command_argument({psydata_i}, length={psydata_len})
           allocate(character({psydata_len})::{psydata_arg})
           call get_command_argument({psydata_i}, {psydata_arg}, &
                                     length={psydata_len})
           if ({psydata_arg} == "--update") then
              ! For later to allow marking fields as being updated
           else
              allocate(character({psydata_len})::{psydata_filename})
              {psydata_filename} = {psydata_arg}
           endif
           deallocate({psydata_arg})
        enddo
        """
        command_line = \
            FortranReader().psyir_from_statement(code, program_symbol_table)
        program.children.insert(0, command_line)

        # Now add the handling of the filename parameter
        code = f"""
        if (allocated({psydata_filename})) then
           call {psy_data_var.name}%OpenReadFileName({psydata_filename})
        else
           call {psy_data_var.name}%OpenReadModuleRegion('{module_name}', &
                                                         '{region_name}')
        endif
        """
        filename_test = \
            FortranReader().psyir_from_statement(code, program_symbol_table)
        program.children.insert(1, filename_test)

    # -------------------------------------------------------------------------
    def create(self, nodes, read_write_info, prefix, postfix, region_name):
        # pylint: disable=too-many-arguments
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernels specified in the 'nodes'
        PSyIR tree with the parameters from the file. The `nodes` are
        consecutive nodes from the PSyIR tree.
        It returns the file container which contains the driver.

        :param nodes: a list of nodes.
        :type nodes: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols ``extract_psydata``.
        :param str postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param Tuple[str,str] region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.

        :returns: the program PSyIR for a stand-alone driver.
        :rtype: :py:class:`psyclone.psyir.psyir.nodes.FileContainer`

        '''
        # pylint: disable=too-many-locals

        # Since this is a 'public' method of an entirely separate class,
        # we check that the list of nodes is what it expects. This is done
        # by invoking the validate function of the basic extract function.
        extract_trans = ExtractTrans()
        # We need to provide the prefix to the validation function:
        extract_trans.validate(nodes, options={"prefix": prefix})

        module_name, local_name = region_name
        unit_name = self._make_valid_unit_name(f"{module_name}_{local_name}")

        # First create the file container, which will only store the program:
        file_container = FileContainer(unit_name)

        # Create the program and add it to the file container:
        program = Routine.create(unit_name, is_program=True)
        program_symbol_table = program.symbol_table
        file_container.addchild(program)

        if prefix:
            prefix = prefix + "_"

        psy_data_mod = ContainerSymbol("read_kernel_data_mod")
        program_symbol_table.add(psy_data_mod)
        psy_data_type = DataTypeSymbol("ReadKernelDataType", UnresolvedType(),
                                       interface=ImportInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)

        # The validation of the extract transform guarantees that all nodes
        # in the node list have the same parent.
        invoke_sched = nodes[0].ancestor(InvokeSchedule)
        schedule_copy = Routine.create("name")
        schedule_copy.children.extend([n.copy() for n in nodes[0].children])

        original_symbol_table = invoke_sched.symbol_table

        # Find all imported routines and add them to the symbol table
        # of the driver, so the driver will have the correct import
        # statements.
        self._import_modules(program.scope.symbol_table, schedule_copy)
        self._add_precision_symbols(program.scope.symbol_table)
        # ExtractNode._bring_external_symbols(read_write_info,
        #                                     program.scope.symbol_table)

        root_name = prefix + "psy_data"
        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        self._add_command_line_handler(program, psy_data, module_name,
                                       local_name)
        output_symbols = self._create_read_in_code(program, psy_data,
                                                   original_symbol_table,
                                                   read_write_info, postfix)
        # Move the nodes making up the extracted region into the Schedule
        # of the driver program
        all_children = schedule_copy.pop_all_children()
        for child in all_children:
            program.addchild(child)

        BaseDriverCreator.add_result_tests(program, output_symbols)

        for symbol in program_symbol_table.datasymbols:

            if isinstance(symbol.datatype, UnsupportedFortranType):
                symbol.datatype = symbol.datatype.copy()

                newt = symbol.datatype._declaration
                newt = newt.replace('pointer', 'allocatable')
                newt = newt.replace('=> null()', '')
                symbol.datatype._declaration = newt

        return file_container

    # -------------------------------------------------------------------------
    @staticmethod
    def collect_all_required_modules(file_container):
        '''Collects recursively all modules used in the file container.
        It returns a dictionary, with the keys being all the (directly or
        indirectly) used modules.

        :param file_container: the FileContainer for which to collect all
            used modules.
        :type file_container:
            :py:class:`psyclone.psyir.psyir.nodes.FileContainer`

        :returns: a dictionary, with the required module names as key, and
            as value a set of all modules required by the key module.
        :rtype: Dict[str, Set[str]]

        '''
        all_mods = set()
        for container in file_container.children:
            sym_tab = container.symbol_table
            # Add all imported modules (i.e. all container symbols)
            all_mods.update(symbol.name for symbol in sym_tab.symbols
                            if isinstance(symbol, ContainerSymbol))

        mod_manager = ModuleManager.get()
        return mod_manager.get_all_dependencies_recursively(all_mods)

    # -------------------------------------------------------------------------
    def get_driver_as_string(self, nodes, read_write_info, prefix, postfix,
                             region_name, writer=FortranWriter()):
        # pylint: disable=too-many-arguments, too-many-locals
        '''This function uses the `create()` function to get the PSyIR of a
        stand-alone driver, and then uses the provided language writer
        to create a string representation in the selected language
        (defaults to Fortran).
        All required modules will be inlined in the correct order, i.e. each
        module will only depend on modules inlined earlier, which will allow
        compilation of the driver. No other dependencies (except system
        dependencies like NetCDF) are required for compilation.

        :param nodes: a list of nodes.
        :type nodes: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param Tuple[str,str] region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.
        :type language_writer:
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        :returns: the driver in the selected language.
        :rtype: str

        '''
        file_container = self.create(nodes, read_write_info, prefix,
                                     postfix, region_name)

        module_dependencies = self.collect_all_required_modules(file_container)
        # Sort the modules by dependencies, i.e. start with modules
        # that have no dependency. This is required for compilation, the
        # compiler must have found any dependent modules before it can
        # compile a module.
        mod_manager = ModuleManager.get()
        sorted_modules = mod_manager.sort_modules(module_dependencies)

        # Inline all required modules into the driver source file so that
        # it is stand-alone.
        out = []

        for module in sorted_modules:
            # Note that all modules in `sorted_modules` are known to be in
            # the module manager, so we can always get the module info here.
            mod_info = mod_manager.get_module_info(module)
            out.append(mod_info.get_source_code())

        out.append(writer(file_container))

        return "\n".join(out)

    # -------------------------------------------------------------------------
    def write_driver(self, nodes, read_write_info, prefix, postfix,
                     region_name, writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses the ``get_driver_as_string()`` function to get a
        a stand-alone driver, and then writes this source code to a file. The
        file name is derived from the region name:
        "driver-"+module_name+"_"+region_name+".F90"

        :param nodes: a list of nodes containing the body of the driver
            routine.
        :type nodes: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param Tuple[str,str] region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.
        :param writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.
        :type writer:
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        '''
        if self._region_name is not None:
            region_name = self._region_name
        code = self.get_driver_as_string(nodes, read_write_info, prefix,
                                         postfix, region_name, writer=writer)
        fll = FortLineLength()
        code = fll.process(code)
        module_name, local_name = region_name
        with open(f"driver-{module_name}-{local_name}.F90", "w",
                  encoding='utf-8') as out:
            out.write(code)
