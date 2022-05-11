# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Author: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the StructureReference node. '''

from __future__ import absolute_import
import six

from psyclone.core import Signature
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.array_member import ArrayMember
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.array_of_structures_member import \
    ArrayOfStructuresMember
from psyclone.psyir.nodes.structure_member import StructureMember
from psyclone.psyir.symbols import DataSymbol, DataTypeSymbol, StructureType, \
    DeferredType, UnknownType
from psyclone.errors import InternalError


class StructureReference(Reference):
    '''
    Node representing a reference to a component of a structure. As such
    it must have a single child representing the component being accessed.

    '''
    # Textual description of the node.
    _children_valid_format = "Member"
    _text_name = "StructureReference"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Member)
        return False

    @staticmethod
    def create(symbol, members, parent=None):
        '''
        Create a StructureReference instance given a symbol and a
        list of components. e.g. for "field%bundle(2)%flag" this
        list would be [("bundle", [Literal("2", INTEGER4_TYPE)]), "flag"].

        :param symbol: the symbol that this reference is to.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param members: the component(s) of the structure that make up \
            the full access. Any components that are array accesses must \
            provide the name of the array and a list of DataNodes describing \
            which part of it is accessed.
        :type members: list of str or 2-tuples containing (str, \
            list of nodes describing array access)
        :param parent: the parent of this node in the PSyIR.
        :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a StructureReference instance.
        :rtype: :py:class:`psyclone.psyir.nodes.StructureReference`

        :raises TypeError: if the supplied symbol is not a DataSymbol.

        '''
        if not isinstance(symbol, DataSymbol):
            raise TypeError(
                "The 'symbol' argument to StructureReference.create() "
                "should be a DataSymbol but found '{0}'.".format(
                    type(symbol).__name__))

        return StructureReference._create(symbol, symbol.datatype, members,
                                          parent=parent)

    @classmethod
    def _create(cls, symbol, symbol_type, members, parent=None):
        '''
        Create an instance of `cls` given a symbol, a type and a
        list of components. e.g. for "field%bundle(2)%flag" this list
        would be [("bundle", [Literal("2", INTEGER4_TYPE)]), "flag"].

        This 'internal' method is used by both ArrayOfStructuresReference
        *and* this class which is why it is a class method with the symbol
        type as a separate argument.

        :param symbol: the symbol that this reference is to.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param symbol_type: the type of the symbol being referenced.
        :type symbol_type: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        :param members: the component(s) of the structure that are being \
            accessed. Any components that are array references must \
            provide the name of the array and a list of DataNodes describing \
            which part of it is accessed.
        :type members: list of str or 2-tuples containing (str, \
            list of nodes describing array access)
        :param parent: the parent of this node in the PSyIR.
        :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a StructureReference instance.
        :rtype: :py:class:`psyclone.psyir.nodes.StructureReference`

        :raises TypeError: if the arguments to the create method are not of \
            the expected type.
        :raises ValueError: if no members are provided (since this would then \
            be a Reference as opposed to a StructureReference).
        :raises ValueError: if more than one member contains a Range, e.g. \
            'a(:)%b%c(:)'.
        :raises TypeError: if any of the structures being referenced\
            do not have full type information available.

        '''
        if not isinstance(symbol_type, (StructureType, DataTypeSymbol,
                                        DeferredType, UnknownType)):
            raise TypeError(
                "A StructureReference must refer to a symbol that is (or "
                "could be) a structure, however symbol '{0}' has type "
                "'{1}'.".format(symbol.name, symbol_type))
        if not isinstance(members, list):
            raise TypeError(
                "The 'members' argument to StructureReference._create() "
                "must be a list but found '{0}'."
                "".format(type(members).__name__))
        if not members:
            raise ValueError(
                "A StructureReference must include one or more structure "
                "'members' that are being accessed but got an empty list for "
                "symbol '{0}'".format(symbol.name))

        # Create the base reference to the symbol that is a structure
        ref = cls(symbol, parent=parent)

        # Bottom-up creation of full reference. The last element in the members
        # list must be either an ArrayMember or a Member.
        member_with_range = None
        if isinstance(members[-1], tuple):
            # An access to one or more array elements
            subref = ArrayMember.create(members[-1][0], members[-1][1])

            if any(isinstance(idx_expr, Range) for idx_expr in members[-1][1]):
                member_with_range = members[-1][0]

        elif isinstance(members[-1], six.string_types):
            # A member access
            subref = Member(members[-1])
        else:
            raise TypeError(
                "The list of 'members' passed to StructureType._create() "
                "must consist of either 'str' or 2-tuple entries but "
                "found '{0}' in the last entry while attempting to create "
                "reference to symbol '{1}'".format(type(members[-1]).__name__,
                                                   symbol.name))

        # Now do the remaining entries in the members list. Since we know that
        # each of these forms part of a structure they must be either a
        # StructureMember or an ArrayOfStructuresMember.
        child_member = subref

        for component in reversed(members[:-1]):
            if isinstance(component, tuple):
                # This is an array access so we have an ArrayOfStructuresMember
                if any(isinstance(idx_expr, Range) for idx_expr in
                       component[1]):
                    if member_with_range:
                        raise ValueError(
                            f"Only one of the list of 'members' passed to "
                            f"StructureType._create() may contain a Range but "
                            f"both '{component[0]}' and '{member_with_range}' "
                            f"have Ranges.")
                    member_with_range = component[0]
                subref = ArrayOfStructuresMember.create(
                    component[0], component[1], subref)
            elif isinstance(component, six.string_types):
                # No array access so just a StructureMember
                subref = StructureMember.create(component, subref)
            else:
                raise TypeError(
                    "The list of 'members' passed to StructureType._create() "
                    "must consist of either 'str' or 2-tuple entries but "
                    "found '{0}' while attempting to create reference to "
                    "symbol '{1}'".format(type(component).__name__,
                                          symbol.name))
            child_member = subref
        # Finally, add this chain to the top-level reference
        ref.addchild(child_member)
        return ref

    def __str__(self):
        result = super(StructureReference, self).__str__()
        for entity in self._children:
            result += "\n" + str(entity)
        return result

    @property
    def member(self):
        '''
        :returns: the member of the structure that this reference is to.
        :rtype: :py:class:`psyclone.psyir.nodes.Member`

        :raises InternalError: if the first child of this node is not an \
                               instance of Member.
        '''
        if not self.children or not isinstance(self.children[0], Member):
            raise InternalError(
                "{0} malformed or incomplete. It must have a single child "
                "that must be a (sub-class of) Member, but found: {1}".format(
                    type(self).__name__, self.children))
        return self.children[0]

    def rank_of_subsection(self):
        '''Check that the supplied candidate array reference uses supported
        array notation syntax and return the rank of the sub-section
        of the array that uses array notation. e.g. for a reference
        "a(:, 2, :)" the rank of the sub-section is 2.

        :param node: the reference to check.
        :type node: :py:class:`psyclone.psyir.nodes.ArrayReference` or \
            :py:class:`psyclone.psyir.nodes.ArrayMember` or \
            :py:class:`psyclone.psyir.nodes.StructureReference`

        :returns: rank of the sub-section of the array.
        :rtype: int

        :raises InternalError: if no ArrayMixin node with at least one \
                               Range in its indices is found.
        :raises InternalError: if two or more part references in a \
                               structure reference contain ranges.
        :raises NotImplementedError: if the supplied node is not of a \
                                     supported type.
        :raises NotImplementedError: if any ranges are encountered that are \
                                     not for the full extent of the dimension.
        '''
        from psyclone.errors import InternalError
        ranks = 0
        if isinstance(self, ArrayMixin):
            ranks += ArrayMixin.rank_of_subsection(self)
        if isinstance(self.member, ArrayMixin):
            ranks += self.member.rank_of_subsection()

        #non_zero_ranks = [rank for rank in ranks if rank > 0]

        if False: #len(non_zero_ranks) > 1:
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.backend.fortran import FortranWriter
            lang_writer = FortranWriter()
            raise InternalError(
                f"Found a structure reference containing two or more part "
                f"references that have ranges: '{lang_writer(self)}'. "
                f"This is not valid PSyIR.")

        if False: #not non_zero_ranks:
            raise InternalError(
                "No array access found in node '{0}'".format(self.name))
        return ranks #[0]

    def get_signature_and_indices(self):
        ''':returns: the Signature of this structure reference, and \
            a list of the indices used for each component (empty list \
            if an access is not an array).
        :rtype: tuple(:py:class:`psyclone.core.Signature`, list of \
            list of indices)

        '''
        # Get the signature of self:
        my_sig, my_index = \
            super(StructureReference, self).get_signature_and_indices()
        # Then the sub-signature of the member, and indices used:
        sub_sig, indices = self.children[0].get_signature_and_indices()
        # Combine signature and indices
        return (Signature(my_sig, sub_sig), my_index + indices)


# For AutoAPI documentation generation
__all__ = ['StructureReference']
