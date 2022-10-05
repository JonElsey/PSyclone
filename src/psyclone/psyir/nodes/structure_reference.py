# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Author: A. R. Porter and N. Nobre, STFC Daresbury Lab
# Author: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the StructureReference node. '''

from psyclone.core import Signature
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.array_member import ArrayMember
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.array_of_structures_member import \
    ArrayOfStructuresMember
from psyclone.psyir.nodes.structure_member import StructureMember
from psyclone.psyir.symbols import (DataSymbol, DataTypeSymbol, StructureType,
                                    ArrayType, DeferredType, ScalarType,
                                    UnknownType)
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
                f"The 'symbol' argument to StructureReference.create() "
                f"should be a DataSymbol but found '{type(symbol).__name__}'.")

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
        :raises NotImplementedError: if any of the structures being referenced\
            do not have full type information available.

        '''
        if not isinstance(symbol_type, (StructureType, DataTypeSymbol,
                                        DeferredType, UnknownType)):
            raise TypeError(
                f"A StructureReference must refer to a symbol that is (or "
                f"could be) a structure, however symbol '{symbol.name}' has "
                f"type '{symbol_type}'.")
        if not isinstance(members, list):
            raise TypeError(
                f"The 'members' argument to StructureReference._create() "
                f"must be a list but found '{type(members).__name__}'.")
        if not members:
            raise ValueError(
                f"A StructureReference must include one or more structure "
                f"'members' that are being accessed but got an empty list for "
                f"symbol '{symbol.name}'")

        # Create the base reference to the symbol that is a structure
        ref = cls(symbol, parent=parent)

        # Bottom-up creation of full reference. The last element in the members
        # list must be either an ArrayMember or a Member.
        if isinstance(members[-1], tuple):
            # An access to one or more array elements
            subref = ArrayMember.create(members[-1][0], members[-1][1])
        elif isinstance(members[-1], str):
            # A member access
            subref = Member(members[-1])
        else:
            raise TypeError(
                f"The list of 'members' passed to StructureType._create() "
                f"must consist of either 'str' or 2-tuple entries but found "
                f"'{type(members[-1]).__name__}' in the last entry while "
                f"attempting to create reference to symbol '{symbol.name}'")

        # Now do the remaining entries in the members list. Since we know that
        # each of these forms part of a structure they must be either a
        # StructureMember or an ArrayOfStructuresMember.
        child_member = subref

        for component in reversed(members[:-1]):
            if isinstance(component, tuple):
                # This is an array access so we have an ArrayOfStructuresMember
                subref = ArrayOfStructuresMember.create(
                    component[0], component[1], subref)
            elif isinstance(component, str):
                # No array access so just a StructureMember
                subref = StructureMember.create(component, subref)
            else:
                raise TypeError(
                    f"The list of 'members' passed to StructureType._create() "
                    f"must consist of either 'str' or 2-tuple entries but "
                    f"found '{type(component).__name__}' while attempting to "
                    f"create reference to symbol '{symbol.name}'")
            child_member = subref
        # Finally, add this chain to the top-level reference
        ref.addchild(child_member)
        return ref

    def __str__(self):
        result = super().__str__()
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
                f"{type(self).__name__} malformed or incomplete. It must have "
                f"a single child that must be a (sub-class of) Member, but "
                f"found: {self.children}")
        return self.children[0]

    def get_signature_and_indices(self):
        ''':returns: the Signature of this structure reference, and \
            a list of the indices used for each component (empty list \
            if an access is not an array).
        :rtype: tuple(:py:class:`psyclone.core.Signature`, list of \
            list of indices)

        '''
        # Get the signature of self:
        my_sig, my_index = super().get_signature_and_indices()
        # Then the sub-signature of the member, and indices used:
        sub_sig, indices = self.children[0].get_signature_and_indices()
        # Combine signature and indices
        return (Signature(my_sig, sub_sig), my_index + indices)

    @property
    def datatype(self):
        '''
        Walks down the list of members making up this reference to determine
        the type that it refers to.

        In order to minimise code duplication, this method also supports
        ArrayOfStructuresReference by simply allowing for the case where
        the starting reference is to an Array.

        :returns: the datatype of this reference.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`

        :raises NotImplementedError: if the structure reference represents \
                                     an array of arrays.
        '''
        dtype = self.symbol.datatype

        if isinstance(dtype, ArrayType):
            dtype = dtype.intrinsic

        if isinstance(dtype, DataTypeSymbol):
            dtype = dtype.datatype

        if isinstance(dtype, (DeferredType, UnknownType)):
            # We don't know the type of the symbol that defines the type
            # of this structure.
            return DeferredType()

        # We do have the definition of this structure - walk down it.
        cursor = self
        cursor_type = dtype

        # The next four lines are required when this method is called for an
        # ArrayOfStructuresReference.
        if isinstance(cursor, ArrayMixin):
            # pylint: disable=protected-access
            shape = cursor._get_effective_shape()
        else:
            shape = []

        # Walk down the structure, collecting information on any array slices
        # as we go.
        while hasattr(cursor, "member"):
            cursor = cursor.member
            cursor_type = cursor_type.components[cursor.name].datatype
            if isinstance(cursor_type, (UnknownType, DeferredType)):
                return DeferredType()
            if isinstance(cursor, ArrayMixin):
                # pylint: disable=protected-access
                shape.extend(cursor._get_effective_shape())

        # We've reached the ultimate member of the structure access.
        if shape:
            if isinstance(cursor_type, ArrayType):
                # It's of array type but does it represent a single element,
                # a slice or a whole array? (We use `children` rather than
                # `indices` so as to avoid having to check that `cursor` is
                # an `ArrayMember`.)
                if cursor.children:
                    # It has indices so could be a single element or a slice.
                    # pylint: disable=protected-access
                    cursor_shape = cursor._get_effective_shape()
                else:
                    # No indices so it is an access to a whole array.
                    cursor_shape = cursor_type.shape
                if cursor_shape and shape != cursor_shape:
                    # This ultimate access is an array but we've already
                    # encountered one or more slices earlier in the access
                    # expression.
                    # TODO #1887. Allow the writer to be used in error messages
                    # to be set in the Config object?
                    # pylint: disable=import-outside-toplevel
                    from psyclone.psyir.backend.fortran import FortranWriter
                    fwriter = FortranWriter()
                    raise NotImplementedError(
                        f"Array of arrays not supported: the ultimate member "
                        f"'{cursor.name}' of the StructureAccess represents "
                        f"an array but other array notation is present in the "
                        f"full access expression: '{fwriter(self)}'")
                return ArrayType(cursor_type.intrinsic, shape)

            return ArrayType(cursor_type, shape)

        # We don't have an explicit array access (because `shape` is Falsey)
        # but is the ultimate member itself an array?
        if isinstance(cursor_type, ArrayType):
            if not cursor.children:
                # It is and there are no index expressions so we return the
                # ArrayType.
                return cursor_type
            # We have an access to a single element of the array.
            # Currently arrays of scalars are handled in a
            # different way to all other types of array. Issue #1857 will
            # fix this anomaly.
            if isinstance(cursor_type.intrinsic, ScalarType.Intrinsic):
                return ScalarType(cursor_type.intrinsic, cursor_type.precision)
            return cursor_type.intrinsic
        return cursor_type


# For AutoAPI documentation generation
__all__ = ['StructureReference']
