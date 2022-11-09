# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the FieldVectorArg class.

'''
import pytest

from psyclone.domain.lfric.kernel.field_vector_arg import FieldVectorArg


def test_init_noargs():
    '''Test that a FieldVectorArg instance can be created successfully when no
    arguments are provided.

    '''
    field_vector_arg = FieldVectorArg()
    assert isinstance(field_vector_arg, FieldVectorArg)
    assert field_vector_arg.form == "GH_FIELD"
    assert field_vector_arg._datatype is None
    assert field_vector_arg._access is None
    assert field_vector_arg._function_space is None
    assert field_vector_arg._vector_length is None


def test_init_invalid():
    '''Test that appropriate exceptions are raised if invalid initial
    values are provided when constructing an instance of the FieldVectorArg
    class. Only test one of the arguments for the base class.

    '''
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg(datatype="invalid")
    assert ("The datatype descriptor metadata for a field should be one of "
            "['gh_real', 'gh_integer'], but found 'invalid'."
            in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = FieldVectorArg(vector_length=1)
    assert ("The vector size should be a string but found int."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg(vector_length="invalid")
    assert ("The vector size should be a string containing an integer, "
            "but found 'invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg(vector_length="0")
    assert ("The vector size should be an integer greater than 1 but found 0."
            in str(info.value))


def test_init_args():
    '''Test that valid initial values provided when constructing an
    instance of FieldVectorArg are stored as expected.

    '''
    field_vector_arg = FieldVectorArg("GH_REAL", "GH_READ", "W0", "2")
    assert field_vector_arg.form == "GH_FIELD"
    assert field_vector_arg._datatype == "GH_REAL"
    assert field_vector_arg._access == "GH_READ"
    assert field_vector_arg._function_space == "W0"
    assert field_vector_arg._vector_length == "2"


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg.create_from_fortran_string("not valid")
    assert ("Expected kernel metadata to be a Fortran Part_Ref, with "
            "the form 'arg_type(...)' but found 'not valid'."
            in str(info.value))

    fortran_string = "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)"
    field_arg = FieldVectorArg.create_from_fortran_string(fortran_string)
    assert field_arg.form == "GH_FIELD"
    assert field_arg._datatype == "GH_REAL"
    assert field_arg._access == "GH_READ"
    assert field_arg._function_space == "W0"
    assert field_arg._vector_length == "3"


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(TypeError) as info:
        _ = FieldVectorArg.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    fparser2_tree = FieldVectorArg.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)")
    with pytest.raises(TypeError) as info:
        _ = FieldVectorArg.create_from_fparser2(fparser2_tree)
    assert ("The vector length metadata should be in the form "
            "'form*vector_length' but found 'GH_FIELD'." in str(info.value))

    fparser2_tree = FieldVectorArg.create_fparser2(
        "arg_type(GH_FEELED*3, GH_REAL, GH_READ, W0)")
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg.create_from_fparser2(fparser2_tree)
    assert ("FieldVectors should have GH_FIELD in their first metadata "
            "argument, but found 'GH_FEELED'." in str(info.value))

    fparser2_tree = FieldVectorArg.create_fparser2(
        "arg_type(GH_FIELD*3, GH_UNREAL, GH_READ, W0)")
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg.create_from_fparser2(fparser2_tree)
    assert ("At argument index '1' for metadata 'arg_type(GH_FIELD * 3, "
            "GH_UNREAL, GH_READ, W0)'. The datatype descriptor metadata for "
            "a field should be one of ['gh_real', 'gh_integer'], but found "
            "'GH_UNREAL'." in str(info.value))

    fparser2_tree = FieldVectorArg.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_ERROR, W0)")
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg.create_from_fparser2(fparser2_tree)
    assert ("At argument index '2' for metadata 'arg_type(GH_FIELD * 3, "
            "GH_REAL, GH_ERROR, W0)'. The access descriptor metadata for a "
            "field should be one of ['gh_read', 'gh_write', 'gh_inc', "
            "'gh_readinc'], but found 'GH_ERROR'." in str(info.value))

    fparser2_tree = FieldVectorArg.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_READ, DOUBLE_U_ZERO)")
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg.create_from_fparser2(fparser2_tree)
    assert ("At argument index '3' for metadata 'arg_type(GH_FIELD * 3, "
            "GH_REAL, GH_READ, DOUBLE_U_ZERO)'. The function space metadata "
            "should be one of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', "
            "'w0', 'w1', 'w2', 'w2trace', 'w2h', 'w2htrace', 'any_w2', "
            "'wchi', 'any_space_1', 'any_space_2', 'any_space_3', "
            "'any_space_4', 'any_space_5', 'any_space_6', 'any_space_7', "
            "'any_space_8', 'any_space_9', 'any_space_10', "
            "'any_discontinuous_space_1', 'any_discontinuous_space_2', "
            "'any_discontinuous_space_3', 'any_discontinuous_space_4', "
            "'any_discontinuous_space_5', 'any_discontinuous_space_6', "
            "'any_discontinuous_space_7', 'any_discontinuous_space_8', "
            "'any_discontinuous_space_9', 'any_discontinuous_space_10'], "
            "but found 'DOUBLE_U_ZERO'." in str(info.value))

    fparser2_tree = FieldVectorArg.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)")
    field_vector_arg = FieldVectorArg.create_from_fparser2(fparser2_tree)
    assert field_vector_arg.form == "GH_FIELD"
    assert field_vector_arg._datatype == "GH_REAL"
    assert field_vector_arg._access == "GH_READ"
    assert field_vector_arg._function_space == "W0"
    assert field_vector_arg._vector_length == "3"


def test_fortran_string():
    '''Test that the fortran_string method works as expected, including
    raise an exception if all of the required properties have not been
    set '''
    fortran_string = "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)"
    field_vector_arg = FieldVectorArg.create_from_fortran_string(
        fortran_string)
    result = field_vector_arg.fortran_string()
    assert result == fortran_string

    field_vector_arg = FieldVectorArg()
    with pytest.raises(ValueError) as info:
        _ = field_vector_arg.fortran_string()
    assert ("Values for datatype, access, function_space and vector_length "
            "must be provided before calling the fortran_string method, but "
            "found 'None', 'None', 'None' and 'None', respectively."
            in str(info.value))


def test_setter_getter():
    '''Test that the setters and getters work as expected, including
    raising exceptions if values are invalid. Only test some of the
    base class values.

    '''
    field_arg = FieldVectorArg()
    assert field_arg.form == "GH_FIELD"

    assert field_arg.datatype is None
    with pytest.raises(ValueError) as info:
        field_arg.datatype = "invalid"
    assert ("The datatype descriptor metadata for a field should be one of "
            "['gh_real', 'gh_integer'], but found 'invalid'."
            in str(info.value))

    field_arg.datatype = "gh_integer"
    assert field_arg.datatype == "gh_integer"
    field_arg.datatype = "GH_INTEGER"
    assert field_arg.datatype == "GH_INTEGER"

    assert field_arg.vector_length is None
    with pytest.raises(ValueError) as info:
        field_arg.vector_length = "invalid"
    assert ("The vector size should be a string containing an integer, "
            "but found 'invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        field_arg.vector_length = "1"
    assert ("The vector size should be an integer greater than 1 but found 1."
            in str(info.value))

    field_arg.vector_length = "3"
    assert field_arg.vector_length == "3"
