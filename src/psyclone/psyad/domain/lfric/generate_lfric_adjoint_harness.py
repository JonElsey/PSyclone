# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

''' Provides LFRic-specific PSyclone adjoint test-harness functionality. '''

from fparser import api as fpapi
from psyclone.domain.lfric import psyir
from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicBuiltinFunctorFactory, LFRicKernelFunctor)
from psyclone.domain.lfric.algorithm.lfric_alg import LFRicAlg
from psyclone.errors import InternalError
from psyclone.psyad.domain.common.adjoint_utils import (create_adjoint_name,
                                                        create_real_comparison,
                                                        find_container)
from psyclone.psyir.nodes import (Call, Reference, ArrayReference, Assignment,
                                  Literal, BinaryOperation, Routine)
from psyclone.psyir.symbols import (ImportInterface, ContainerSymbol,
                                    ScalarType, ArrayType, RoutineSymbol,
                                    DataTypeSymbol, DataSymbol, DeferredType)


def _compute_lfric_inner_products(prog, scalars, field_sums, sum_sym):
    '''
    Adds PSyIR to a supplied Routine to compute the sum of the inner products
    of the supplied scalars and fields.

    :param prog: the Routine to which to add PSyIR.
    :type prog: :py:class:`psyclone.psyir.nodes.Routine`
    :param scalars: list of pairs of scalars to multiply and sum.
    :type scalars: List[Tuple[:py:class:`psyclone.psyir.symbols.DataSymbol`,
                              :py:class:`psyclone.psyir.symbols.DataSymbol`]]
    :param field_sums: the results of all of the inner products of the \
                       various field arguments.
    :type field_sums: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param sum_sym: the symbol into which to accumulate the final sum.
    :type sum_sym: :py:class:`psyclone.psyir.symbols.DataSymbol`

    '''
    table = prog.symbol_table
    idef_sym = psyir.add_lfric_precision_symbol(table, "i_def")
    idef_type = ScalarType(ScalarType.Intrinsic.REAL, idef_sym)

    # Initialise the sum to zero: sum = 0.0
    prog.addchild(Assignment.create(Reference(sum_sym),
                                    Literal("0.0", sum_sym.datatype)))
    for scalar in scalars:
        # Compute the product of the pair of scalars: scalar[0]*scalar[1]
        prod = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                      Reference(scalar[0]),
                                      Reference(scalar[1]))
        # Add this product to the sum:
        #     sum = sum + scalar[0]*scalar[1]
        prog.addchild(
                Assignment.create(
                    Reference(sum_sym),
                    BinaryOperation.create(BinaryOperation.Operator.ADD,
                                           Reference(sum_sym), prod)))
    for sym in field_sums:
        if sym.is_scalar:
            # Add this result of a field inner product to the sum:
            #     sum = sum + field_sum
            prog.addchild(
                    Assignment.create(
                        Reference(sum_sym),
                        BinaryOperation.create(BinaryOperation.Operator.ADD,
                                               Reference(sum_sym),
                                               Reference(sym))))
        else:
            # For a field vector we have an array of inner-product values that
            # must be summed.
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                # sum = sum + field_sum(dim)
                add_op = BinaryOperation.create(
                    BinaryOperation.Operator.ADD,
                    Reference(sum_sym),
                    ArrayReference.create(sym,
                                          [Literal(str(dim), idef_type)]))
                prog.addchild(Assignment.create(Reference(sum_sym), add_op))


def _compute_field_inner_products(routine, field_pairs):
    '''
    Constructs the assignments and kernel functors needed to compute the
    inner products of the supplied list of fields.

    :param routine: the routine to which to add the assignments.
    :type routine: :py:class:`psyclone.psyir.nodes.Routine`
    :param field_pairs: the pairs of fields for which we need to compute the \
                        inner products.
    :type field_pairs: List[ \
               Tuple[:py:class:`psyclone.psyir.symbols.DataSymbol`, \
                     :py:class:`psyclone.psyir.symbols.DataSymbol`]]

    :returns: the symbols containing the inner products and the list of \
              kernel functors that computes them.
    :rtype: Tuple[List[:py:class:`psyclone.psyir.symbols.DataSymbol`], \
              List[:py:class:`psyclone.domain.lfric.algorithm.KernelFunctor`]]

    :raises TypeError: if any of the supplied symbols are not DataSymbols.
    :raises InternalError: if any of the supplied pairs of symbols do not \
                           have matching types.
    :raises InternalError: if any of the supplied symbols are not of the \
                           correct type for an LFRic field.
    '''
    table = routine.symbol_table
    rdef_sym = psyir.add_lfric_precision_symbol(table, "r_def")
    rdef_type = ScalarType(ScalarType.Intrinsic.REAL, rdef_sym)
    idef_sym = psyir.add_lfric_precision_symbol(table, "i_def")
    idef_type = ScalarType(ScalarType.Intrinsic.REAL, idef_sym)

    builtin_factory = LFRicBuiltinFunctorFactory.get()

    field_ip_symbols = []
    kernel_list = []
    for sym1, sym2 in field_pairs:

        if not (isinstance(sym1, DataSymbol) and isinstance(sym2, DataSymbol)):
            raise TypeError(
                f"Each pair of fields/field-vectors must be supplied as "
                f"DataSymbols but got: {type(sym1)}, {type(sym2)}")

        if sym1.datatype != sym2.datatype:
            raise InternalError(
                f"Cannot compute the inner product of fields '{sym1.name}' "
                f"and '{sym2.name}' because they are of different types: "
                f"{sym1.datatype} and {sym2.datatype}")

        if sym1 is sym2:
            inner_prod_name = f"{sym1.name}_inner_prod"
        else:
            inner_prod_name = f"{sym1.name}_{sym2.name}_inner_prod"

        if isinstance(sym1.datatype, DataTypeSymbol):
            # Create and initialise a variable to hold the result of the
            # inner product of this pair of fields.
            ip_sym = table.new_symbol(inner_prod_name,
                                      symbol_type=DataSymbol,
                                      datatype=rdef_type)
            # name_inner_prod = 0.0
            routine.addchild(Assignment.create(Reference(ip_sym),
                                               Literal("0.0", rdef_type)))
            if sym2 is sym1:
                # Inner product of field with itself.
                kernel_list.append(
                    builtin_factory.create("x_innerproduct_x", table,
                                           [Reference(ip_sym),
                                            Reference(sym1)]))
            else:
                # Inner product of two different fields.
                kernel_list.append(
                    builtin_factory.create(
                        "x_innerproduct_y", table,
                        [Reference(ip_sym), Reference(sym1), Reference(sym2)]))

        elif isinstance(sym1.datatype, ArrayType):
            # This is a pair of field vectors. We compute the inner product of
            # each component and store the results in an array of the same
            # length as the field vector.

            # Create the array in which the results will be stored.
            dtype = ArrayType(rdef_type, sym1.datatype.shape)
            ip_sym = table.new_symbol(inner_prod_name,
                                      symbol_type=DataSymbol,
                                      datatype=dtype)
            for dim in range(int(sym1.datatype.shape[0].lower.value),
                             int(sym1.datatype.shape[0].upper.value)+1):
                lit = Literal(str(dim), idef_type)
                # Zero the inner product for this component pair.
                routine.addchild(Assignment.create(
                    ArrayReference.create(ip_sym,
                                          [lit]), Literal("0.0", rdef_type)))
                if sym2 is sym1:
                    # Inner product of field with itself.
                    kernel_list.append(
                        builtin_factory.create(
                            "x_innerproduct_x", table,
                            [ArrayReference.create(ip_sym, [lit.copy()]),
                             ArrayReference.create(sym1, [lit.copy()])]))
                else:
                    # Inner product of two different fields.
                    kernel_list.append(
                        builtin_factory.create(
                            "x_innerproduct_y", table,
                            [ArrayReference.create(ip_sym, [lit.copy()]),
                             ArrayReference.create(sym1, [lit.copy()]),
                             ArrayReference.create(sym2, [lit.copy()])]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym1.datatype} for field '{sym1.name}'")

        # Store the list of symbols holding the various inner products.
        field_ip_symbols.append(ip_sym)

    return field_ip_symbols, kernel_list


def _init_fields_random(fields, input_symbols, table):
    '''
    Creates a suitable kernel functor for each field that requires
    initialising with pseudo-random data.

    :param fields: those fields requiring initialisation.
    :type fields: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param input_symbols: map from field name to corresponding field copy.
    :type input_symbols: Dict[str, \
        :py:class:`psyclone.psyir.symbols.DataTypeSymbol` | \
        :py:class:`psyclone.psyir.symbols.ArrayType`]
    :param table: the symbol table to which to add new symbols.
    :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    :returns: the required kernel calls.
    :rtype: List[:py:class:`psyclone.domain.common.algorithm.Functor`]

    '''
    idef_sym = psyir.add_lfric_precision_symbol(table, "i_def")
    idef_type = ScalarType(ScalarType.Intrinsic.REAL, idef_sym)
    # We use the setval_random builtin to initialise all fields.
    kernel_list = []
    builtin_factory = LFRicBuiltinFunctorFactory.get()
    for sym in fields:
        input_sym = input_symbols[sym.name]
        if isinstance(sym.datatype, DataTypeSymbol):
            # Initialise the field with pseudo-random numbers.
            kernel_list.append(
                builtin_factory.create("setval_random", table,
                                       [Reference(sym)]))
            # Keep a copy of the values in the associated 'input' field.
            kernel_list.append(builtin_factory.create(
                "setval_x", table, [Reference(input_sym), Reference(sym)]))
        elif isinstance(sym.datatype, ArrayType):
            # Initialise each member of the field vector with pseudo-random
            # numbers.
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                lit = Literal(str(dim), idef_type)
                # Initialise this component with pseudo-random numbers.
                kernel_list.append(
                    builtin_factory.create("setval_random", table,
                                           [ArrayReference.create(sym,
                                                                  [lit])]))
                # Keep a copy of the values in the associated 'input' field.
                kernel_list.append(
                    builtin_factory.create(
                        "setval_x", table,
                        [ArrayReference.create(input_sym, [lit.copy()]),
                         ArrayReference.create(sym, [lit.copy()])]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")

    # Return the list of kernel functors.
    return kernel_list


def generate_lfric_adjoint_harness(tl_psyir):
    '''
    Constructs and returns the PSyIR for a Container and Routine that
    implements a test harness for the adjoint of the supplied tangent-linear
    kernel.

    :param tl_psyir: the PSyIR of an LFRic module defining a \
                     tangent-linear kernel.
    :type tl_psyir: :py:class:`psyclone.psyir.nodes.Container`

    :returns: PSyIR of an Algorithm that tests the adjoint of the supplied \
              LFRic TL kernel.
    :rtype: :py:class:`psyclone.psyir.nodes.Container`

    :raises ValueError: if the supplied PSyIR does not have a Container (that \
        is *not* a FileContainer).
    :raises ValueError: if the name of the Container (module) in the supplied \
        PSyIR does not follow the LFRic naming convention of ending in '_mod'.

    '''
    tl_container = find_container(tl_psyir)
    if not tl_container:
        raise ValueError(
            f"Test harness code can only be generated if the supplied TL "
            f"kernel is within a module (Container) but the supplied PSyIR "
            f"does not have a Container node:\n{tl_psyir.view(colour=False)}")

    lfalg = LFRicAlg()
    container = lfalg.create_alg_routine("adjoint_test")
    routine = container.walk(Routine)[0]
    table = routine.symbol_table

    # Parse the kernel metadata . This still uses fparser1 as that's what
    # the meta-data handling is currently based upon. We therefore have to
    # convert back from PSyIR to Fortran for the moment.
    # TODO #1806 - replace this with the new PSyIR-based metadata handling.
    # pylint: disable=import-outside-toplevel
    from psyclone.psyir.backend.fortran import FortranWriter
    writer = FortranWriter()
    tl_source = writer(tl_container)
    parse_tree = fpapi.parse(tl_source)

    # Get the name of the module that contains the kernel and create a
    # ContainerSymbol for it.
    kernel_mod_name = tl_container.name.lower()
    if not kernel_mod_name.endswith("_mod"):
        raise ValueError(
            f"The supplied LFRic TL kernel is contained within a module named "
            f"'{kernel_mod_name}'. This does not end in '_mod' and as such "
            f"does not comply with the LFRic naming convention.")

    kernel_mod = table.new_symbol(kernel_mod_name, symbol_type=ContainerSymbol)
    # Assume the LFRic naming convention is followed in order to infer the name
    # of the TL kernel. (If this convention isn't followed in the supplied code
    # then the call to `kernel_from_metadata` below will raise an appropriate
    # exception.)
    kernel_name = kernel_mod_name.replace("_mod", "_type")

    adj_mod = table.new_symbol(create_adjoint_name(kernel_mod_name),
                               symbol_type=ContainerSymbol)
    kernel_routine = table.new_symbol(kernel_name,
                                      symbol_type=DataTypeSymbol,
                                      datatype=DeferredType(),
                                      interface=ImportInterface(kernel_mod))
    adj_routine = table.new_symbol(create_adjoint_name(kernel_name),
                                   symbol_type=DataTypeSymbol,
                                   datatype=DeferredType(),
                                   interface=ImportInterface(adj_mod))

    # Construct a DynKern using the metadata and then use it to construct
    # the kernel argument list.
    # TODO #1806 - once we have the new PSyIR-based metadata handling then
    # we can pass PSyIR to this routine rather than an fparser1 parse tree.
    kern = lfalg.kernel_from_metadata(parse_tree, kernel_name)
    kern_args = lfalg.construct_kernel_args(routine, kern)

    # Create symbols that will store copies of the inputs to the TL kernel.
    # Currently we only support scalar and field arguments.
    # TODO #1864 - add support for operators.
    input_symbols = {}
    for sym in kern_args.scalars + [fsym for fsym, _ in kern_args.fields]:
        input_sym = table.new_symbol(sym.name+"_input", symbol_type=DataSymbol,
                                     datatype=DeferredType())
        input_sym.copy_properties(sym)
        input_symbols[sym.name] = input_sym

    # Initialise all input field objects.
    for sym, space in kern_args.fields:
        lfalg.initialise_field(routine, input_symbols[sym.name], space)

    # Initialise argument values and keep copies. For scalars we use the
    # Fortran 'random_number' intrinsic directly.
    # TODO #1345 - this is Fortran specific.
    random_num = RoutineSymbol("random_number")
    for sym in kern_args.scalars:
        routine.addchild(Call.create(random_num, [Reference(sym)]))
        input_sym = table.lookup(sym.name+"_input")
        routine.addchild(Assignment.create(Reference(input_sym),
                                           Reference(sym)))

    kernel_list = _init_fields_random([fld for fld, _ in kern_args.fields],
                                      input_symbols, table)

    # Initialise all operator arguments.
    if kern_args.operators:
        raise NotImplementedError(
            f"Kernel {kernel_name} has one or more operator arguments. Test "
            f"harness creation for such a kernel is not yet supported "
            f"(Issue #1864).")

    # Finally, add the kernel itself to the list for the invoke().
    arg_nodes = []
    for arg in kern_args.arglist:
        arg_nodes.append(Reference(table.lookup(arg)))
    kern = LFRicKernelFunctor.create(kernel_routine, arg_nodes)
    kernel_list.append(kern)

    # Compute the inner products of the results of the TL kernel.
    fld_pairs = []
    for sym, _ in kern_args.fields:
        fld_pairs.append((sym, sym))
    field_ip_symbols, ip_kernels = _compute_field_inner_products(routine,
                                                                 fld_pairs)
    kernel_list.extend(ip_kernels)

    # Create the 'call invoke(...)' for the list of kernels.
    invoke_sym = table.new_symbol("invoke", symbol_type=RoutineSymbol)
    inv_call = LFRicAlgorithmInvokeCall.create(invoke_sym,
                                               kernel_list, 0)
    inv_call.preceding_comment = (
        "Initialise arguments and call the tangent-linear kernel.")
    routine.addchild(inv_call)

    rdef_sym = psyir.add_lfric_precision_symbol(table, "r_def")
    rdef_type = ScalarType(ScalarType.Intrinsic.REAL, rdef_sym)

    # Compute the first inner products.
    inner1_sym = table.new_symbol("inner1", symbol_type=DataSymbol,
                                  datatype=rdef_type)

    scalars = zip(kern_args.scalars, kern_args.scalars)
    _compute_lfric_inner_products(routine, scalars, field_ip_symbols,
                                  inner1_sym)

    # Construct the functor for the adjoint kernel. We pass it the same
    # arguments as the TL kernel.
    adj_kern = LFRicKernelFunctor.create(adj_routine,
                                         [arg.copy() for arg in arg_nodes])
    # Compute the inner product of its outputs
    # with the original inputs.
    fld_pairs = []
    for sym, _ in kern_args.fields:
        fld_pairs.append((sym, input_symbols[sym.name]))
    field_ip_symbols, kernel_list = _compute_field_inner_products(routine,
                                                                  fld_pairs)
    kernel_list = [adj_kern] + kernel_list

    # Create the 'call invoke(...)' for the list of kernels.
    routine.addchild(LFRicAlgorithmInvokeCall.create(
        invoke_sym, kernel_list, 1))

    # Sum up the second set of inner products
    inner2_sym = table.new_symbol("inner2", symbol_type=DataSymbol,
                                  datatype=rdef_type)
    scalars = zip(kern_args.scalars,
                  [input_symbols[sym.name] for sym in kern_args.scalars])
    _compute_lfric_inner_products(routine, scalars, field_ip_symbols,
                                  inner2_sym)

    # Finally, compare the two inner products.
    stmts = create_real_comparison(table, kern, inner1_sym, inner2_sym)
    for stmt in stmts:
        routine.addchild(stmt)

    return container
