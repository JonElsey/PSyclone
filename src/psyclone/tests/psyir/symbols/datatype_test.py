# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Authors R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.datatype module '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import DataType, DeferredType, ScalarType, \
    ArrayType, DataSymbol
from psyclone.errors import InternalError


# Abstract DataType class

def test_datatype():
    '''Test that the DataType class can't be created.'''
    # pylint: disable=abstract-class-instantiated
    with pytest.raises(TypeError) as excinfo:
        _ = DataType()
    assert ("Can't instantiate abstract class DataType with abstract methods "
            "__str__" in str(excinfo.value))


# DeferredType class

def test_deferredtype():
    '''Test that the DeferredType class can be created successfully.'''
    assert isinstance(DeferredType(), DeferredType)


def test_deferredtype_str():
    '''Test that the DeferredType class str method works as expected.'''
    data_type = DeferredType()
    assert str(data_type) == "DeferredType"


# ScalarType class

@pytest.mark.parametrize("precision", [ScalarType.Precision.SINGLE,
                                       ScalarType.Precision.DOUBLE,
                                       ScalarType.Precision.UNDEFINED])
@pytest.mark.parametrize("intrinsic", [ScalarType.Intrinsic.INTEGER,
                                       ScalarType.Intrinsic.REAL,
                                       ScalarType.Intrinsic.BOOLEAN,
                                       ScalarType.Intrinsic.CHARACTER])
def test_scalartype_enum_precision(intrinsic, precision):
    '''Test that the ScalarType class can be created successfully for all
    supported ScalarType intrinsics and all suported enumerated precisions.

    '''
    scalar_type = ScalarType(intrinsic, precision)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision == precision


@pytest.mark.parametrize("precision", [1, 8, 16])
@pytest.mark.parametrize("intrinsic", [ScalarType.Intrinsic.INTEGER,
                                       ScalarType.Intrinsic.REAL,
                                       ScalarType.Intrinsic.BOOLEAN,
                                       ScalarType.Intrinsic.CHARACTER])
def test_scalartype_int_precision(intrinsic, precision):
    '''Test that the ScalarType class can be created successfully for all
    supported ScalarType intrinsics and a set of valid integer precisions.

    '''
    scalar_type = ScalarType(intrinsic, precision)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision == precision


@pytest.mark.parametrize("intrinsic", [ScalarType.Intrinsic.INTEGER,
                                       ScalarType.Intrinsic.REAL,
                                       ScalarType.Intrinsic.BOOLEAN,
                                       ScalarType.Intrinsic.CHARACTER])
def test_scalartype_datasymbol_precision(intrinsic):
    '''Test that the ScalarType class can be created successfully for all
    supported ScalarType intrinsics and the precision specified by another
    symbol.

    '''
    # Create an r_def precision symbol with a constant value of 8
    data_type = ScalarType(ScalarType.Intrinsic.INTEGER,
                           ScalarType.Precision.UNDEFINED)
    precision_symbol = DataSymbol("r_def", data_type, constant_value=8)
    # Set the precision of our ScalarType to be the precision symbol
    scalar_type = ScalarType(intrinsic, precision_symbol)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision is precision_symbol


def test_scalartype_invalid_intrinsic_type():
    '''Test that the ScalarType class raises an exception when an invalid
    intrinsic type is provided.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = ScalarType(None, None)
    assert ("ScalarType expected 'intrinsic' argument to be of type "
            "ScalarType.Intrinsic but found 'NoneType'." in str(excinfo.value))


def test_scalartype_invalid_precision_type():
    '''Test that the ScalarType class raises an exception when an invalid
    precision type is provided.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = ScalarType(ScalarType.Intrinsic.INTEGER, None)
    assert ("ScalarType expected 'precision' argument to be of type int, "
            "ScalarType.Precision or DataSymbol, but found 'NoneType'."
            in str(excinfo.value))


def test_scalartype_invalid_precision_int_value():
    '''Test that the ScalarType class raises an exception when an invalid
    integer precision value is provided.

    '''
    with pytest.raises(ValueError) as excinfo:
        _ = ScalarType(ScalarType.Intrinsic.INTEGER, 0)
    assert ("The precision of a DataSymbol when specified as an integer "
            "number of bytes must be > 0 but found '0'."
            in str(excinfo.value))


def test_scalartype_invalid_precision_datasymbol():
    '''Test that the ScalarType class raises an exception when an invalid
    precision symbol is provided (it must be a scalar integer or
    deferred).

    '''
    # Create an r_def precision symbol with a constant value of 8
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    precision_symbol = DataSymbol("r_def", data_type)
    with pytest.raises(ValueError) as excinfo:
        _ = ScalarType(ScalarType.Intrinsic.REAL, precision_symbol)
    assert ("A DataSymbol representing the precision of another DataSymbol "
            "must be of either 'deferred' or scalar, integer type but got: "
            "r_def: <Scalar<REAL, 4>, Local>"
            in str(excinfo.value))


def test_scalartype_str():
    '''Test that the ScalarType class str method works as expected.'''
    data_type = ScalarType(ScalarType.Intrinsic.BOOLEAN,
                           ScalarType.Precision.UNDEFINED)
    assert str(data_type) == "Scalar<BOOLEAN, UNDEFINED>"


def test_scalartype_immutable():
    '''Test that the scalartype attributes can't be modified'''
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    with pytest.raises(AttributeError):
        data_type.intrinsic = ScalarType.Intrinsic.INTEGER
    with pytest.raises(AttributeError):
        data_type.precision = 8


# ArrayType class
def test_arraytype():
    '''Test that the ArrayType class __init__ works as expected.'''
    datatype = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    shape = [10, 10]
    array_type = ArrayType(datatype, shape)
    assert isinstance(array_type, ArrayType)
    assert array_type.shape == shape
    assert array_type._datatype == datatype


def test_arraytype_invalid_datatype():
    '''Test that the ArrayType class raises an exception when the datatype
    argument is the wrong type.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(None, None)
    assert ("ArrayType expected 'datatype' argument to be of type "
            "DataType but found 'NoneType'." in str(excinfo.value))


def test_arraytype_invalid_shape():
    '''Test that the ArrayType class raises an exception when the shape
    argument is the wrong type.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, None)
    assert ("ArrayType expected 'shape' argument to be of type list but "
            "found 'NoneType'." in str(excinfo.value))


def test_arraytype_invalid_shape_dimension_1():
    '''Test that the ArrayType class raises an exception when one of the
    dimensions of the shape list argument is a datasymbol but is not a
    scalar integer.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    symbol = DataSymbol("fred", scalar_type)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [symbol])
    assert ("DataSymbols that are part of another symbol shape can only be "
            "scalar integers, but found 'fred: <Scalar<REAL, 4>, Local>'."
            in str(excinfo.value))


def test_arraytype_invalid_shape_dimension_2():
    '''Test that the ArrayType class raises an exception when one of the
    dimensions of the shape list argument is not a datasymbol and is
    not an integer or an ArrayType.Extent type.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [None])
    assert ("DataSymbol shape list elements can only be 'DataSymbol', "
            "'integer' or ArrayType.Extent, but found 'NoneType'."
            in str(excinfo.value))


def test_arraytype_str():
    '''Test that the ArrayType class str method works as expected.'''
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER,
                             ScalarType.Precision.UNDEFINED)
    data_symbol = DataSymbol("var", scalar_type)
    data_type = ArrayType(scalar_type, [10, data_symbol,
                                        ArrayType.Extent.DEFERRED,
                                        ArrayType.Extent.ATTRIBUTE])
    assert (str(data_type) == "Array<Scalar<INTEGER, UNDEFINED>,"
            " shape=[10, var, 'DEFERRED', 'ATTRIBUTE']>")


def test_arraytype_str_invalid():
    '''Test that the ArrayType class str method raises an exception if an
    unsupported dimension type is found.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    array_type = ArrayType(scalar_type, [10])
    # Make on of the array dimensions an unsupported type
    array_type._shape = [None]
    with pytest.raises(InternalError) as excinfo:
        _ = str(array_type)
    assert ("PSyclone internal error: ArrayType shape list elements can only "
            "be 'DataSymbol', 'int' or 'ArrayType.Extent', but found "
            "'NoneType'." in str(excinfo.value))


def test_arraytype_immutable():
    '''Test that the scalartype attributes can't be modified'''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    data_type = ArrayType(scalar_type, [10, 10])
    with pytest.raises(AttributeError):
        data_type.intrinsic = ScalarType.Intrinsic.INTEGER
    with pytest.raises(AttributeError):
        data_type.precision = 8
    with pytest.raises(AttributeError):
        data_type.shape = []
