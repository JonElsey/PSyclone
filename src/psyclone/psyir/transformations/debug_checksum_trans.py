# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors  A. B. G. Chalk, STFC Daresbury Lab
#          J. Elsey, CEMAC, University of Leeds

'''This module contains the DebugChecksumTrans class.'''

from typing import Union, List

from psyclone.core import VariablesAccessInfo
from psyclone.psyir.nodes import Assignment, Node, Reference, Routine
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, \
        PreprocessorInterface, ContainerSymbol
from psyclone.psyir.symbols.datatypes import UnsupportedFortranType, UnresolvedType
from psyclone.psyir.symbols.interfaces import ImportInterface



        
        
class DebugChecksumTrans(RegionTrans):
    '''
    Creates a set of checksums (written via print) for all written to arrays
    inside the provided region.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.transformations import DebugChecksumTrans

    >>> psyir = FortranReader().psyir_from_source("""
    ...     subroutine mysubroutine()
    ...     integer, dimension(10,10) :: A
    ...     integer :: i
    ...     integer :: j
    ...     do i = 1, 10
    ...       do j = 1, 10
    ...         A(i,j) = A(i,k) + i-j
    ...       end do
    ...     end do
    ...     end subroutine
    ...     """)
    ... loop = psyir.children[0].children[0]
    ... DebugChecksumTrans().apply(loop)
    ... print(FortranWriter()(psyir))
    subroutine mysubroutine()
      integer, dimension(10,10) :: a
      integer :: i
      integer :: j
      integer :: PSYCLONE_INTERNAL_line_
    <BLANKLINE>
      do i = 1, 10, 1
        do j = 1, 10, 1
          a(i,j) = a(i,j) + i - j
        enddo
      enddo
      PSYCLONE_INTERNAL_line_ = __LINE__
      PRINT *, "checksums from mysubroutine at line:", PSYCLONE_INTERNAL_line_\
+ 1
      PRINT *, "a checksum", SUM(a)
    <BLANKLINE>
    end subroutine mysubroutine
    <BLANKLINE>

    '''

    def apply(self, node: Union[Node, List[Node]], options=None, 
              ukca=False) -> None:
        '''
            Applies the checksum transformation to the provided node(s).

            :param nodes: The node or list of nodes to apply the
                          transformation to.
            :param options: a dictionary with options for transformations.
            :type options: Optional[Dict[str, Any]]
            :param ukca: Boolean switch to determine whether to use PRINT (default)
                         or a combination of WRITE and call umPrint (UKCA/UM)

        '''
        self.validate(node)
        node_list = self.get_node_list(node)

        # Find all the writes.
        vai = VariablesAccessInfo(node_list)
        writes = []
        for sig in vai.all_data_accesses:
            if vai.is_written(sig) and vai[sig].is_array():
              try:
                  sym = vai[sig].all_accesses[0].node.symbol
                  writes.append(sym)
              except Exception as e:
                  print(Exception)
                  continue
        # For each write, add a checksum after.
        checksum_nodes = []
        freader = FortranReader()
        for sym in writes:
            # Skip checksums for character arrays
            if not hasattr(sym, 'datatype'):
                print(f'symbol {sym} has no datatype, continuing...')
                continue
            if isinstance(sym.datatype, UnresolvedType):
                print(f'Symbol {sym} is an UnresolvedType, skipping')
            try:
                if isinstance(sym.datatype, UnsupportedFortranType):
                    print(f'Skipping {sym.name} as it is an unsupported datatype')
                    continue
            except AttributeError:
                print(f'Cannot access datatype attribute of {sym} - continuing')
                continue
            
            # And additionally for arrays of BOOLEAN type
            # this assumes that it is an array, but I think it should be fine
            # as there is the .is_array() conditional above
            if sym.datatype.datatype == None:
                continue
            try:
              if 'BOOLEAN' in sym.datatype.datatype.intrinsic.name:
                print(f'Skipping {sym.name} as it is of BOOLEAN type')
                continue
            except:  # not a suitable datatype for a SUM either if this fails
              continue
            sym_name = sym.name
            print(sym_name)
            print(sym.datatype)
            #breakpoint()
          
            routine = node_list[0].ancestor(Routine)
            srcname = routine.name


            if ukca:
                self.check_for_umprintmgr(routine)
                checksum = freader.psyir_from_statement(
                    f"write (umMessage, 'A40') '{sym_name} checksum', SUM({sym_name})",
                    node_list[0].ancestor(Routine).symbol_table)
                checksum_call = freader.psyir_from_statement(
                  f"call umprint(umMessage, src='{srcname}')"
                )
                # Remove the comment about this being a code block.
                checksum_call.preceding_comment = ""
                checksum_nodes.append(checksum_call)
            else:
                checksum = freader.psyir_from_statement(
                        f"print *, '{sym_name} checksum', SUM({sym_name})",
                        routine.symbol_table)
              
            checksum.preceding_comment = ""
            checksum_nodes.append(checksum)

        # Find the last node in the region
        depth = 10000000
        position = -1
        for node in node_list:
            if node.depth < depth:
                depth = node.depth
                position = node.position
            elif node.depth == depth and node.position > position:
                position = node.position

        parent = node.parent
        for node in checksum_nodes:
            parent.addchild(node, position+1)

        internal_line = \
            node_list[0].ancestor(Routine).symbol_table.find_or_create(
                "PSYCLONE_INTERNAL_line_", symbol_type=DataSymbol,
                datatype=INTEGER_TYPE,
                )
        line = node_list[0].ancestor(Routine).symbol_table.find_or_create(
                "__LINE__", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
                interface=PreprocessorInterface())
        # Tell us where we are to output the checksums.
        explanation_statement = freader.psyir_from_statement(
                f'print *, "checksums from '
                f'{node_list[0].ancestor(Routine).name} at line:"'
                f', PSYCLONE_INTERNAL_line_ + 1',
                node_list[0].ancestor(Routine).symbol_table
                )
        # Remove the comment about this being a code block.
        explanation_statement.preceding_comment = ""
        assign = Assignment.create(Reference(internal_line), Reference(line))
        parent.addchild(explanation_statement, position+1)
        parent.addchild(assign, position+1)

    def check_for_umprintmgr(self, routine):
        ''' Check for "use umPrintMgr, ONLY: umPrint, umMessage, and insert it if 
            not present.
        '''
        
        # Get the symbol table, and check the dict keys for the 
        # module we want, in this case "umprintmgr". If it isn't 
        # present, add it, then add the relevant subroutines
        # that we want to import
        # We could also use wildcard_import=True, but this isn't 
        # good practice
        table = routine.symbol_table
        if 'umprintmgr' not in table.symbols_dict.keys():   
            umprintmgr = table.new_symbol(root_name='umprintmgr',
                                symbol_type=ContainerSymbol,
                                wildcard_import=False
                                )
            ummessage = table.new_symbol(root_name='ummessage',
                                symbol_type=DataSymbol,
                                datatype=UnresolvedType(),
                                )
            umprint = table.new_symbol(root_name='umprint',
                        symbol_type=DataSymbol,
                        datatype=UnresolvedType(),
                        )
            
            # set the Interface of these new Symbols to our umprintmgr
            # symbol - this tells PSyclone that these are linked, and that 
            # these are the routines that we import from that module
            ummessage.interface = ImportInterface(umprintmgr)
            umprint.interface = ImportInterface(umprintmgr)