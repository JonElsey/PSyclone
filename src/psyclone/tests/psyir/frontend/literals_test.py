# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab


''' Performs py.test tests on the support for literals in the fparser2
    PSyIR front-end '''

from __future__ import absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.frontend import fparser2
from psyclone.psyir.frontend.fparser2 import Fparser2Reader, \
    get_literal_precision
from psyclone.psyir.symbols import ScalarType, DataSymbol, DeferredType
from psyclone.psyir.nodes import Node, Literal, CodeBlock, Schedule
from psyclone.errors import InternalError


@pytest.mark.parametrize("code, dtype",
                         [("'hello'", ScalarType.Intrinsic.CHARACTER),
                          ("1", ScalarType.Intrinsic.INTEGER),
                          ("1.0", ScalarType.Intrinsic.REAL),
                          (".tRue.", ScalarType.Intrinsic.BOOLEAN),
                          (".false.", ScalarType.Intrinsic.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_handling_literal(code, dtype):
    ''' Check that the fparser2 frontend can handle literals of all
    supported datatypes. Note that signed literals are represented in the
    PSyIR as a Unary operation on an unsigned literal.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader("x=" + code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype.intrinsic == dtype
    if dtype != ScalarType.Intrinsic.BOOLEAN:
        assert literal.value == code
    else:
        assert literal.value == code.lower()[1:-1]  # Remove wrapping dots


@pytest.mark.parametrize("value,dprecision,intrinsic",
                         [("0.0", "rdef", ScalarType.Intrinsic.REAL),
                          ("1", "idef", ScalarType.Intrinsic.INTEGER),
                          ("'hello'", "cdef", ScalarType.Intrinsic.CHARACTER),
                          (".tRue.", "ldef", ScalarType.Intrinsic.BOOLEAN),
                          (".false.", "ldef", ScalarType.Intrinsic.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_handling_literal_precision_1(value, dprecision, intrinsic):
    '''Check that the fparser2 frontend can handle literals with a
    specified precision kind symbol.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    if intrinsic == ScalarType.Intrinsic.CHARACTER:
        code = "x={0}_{1}".format(dprecision, value)
    else:
        code = "x={0}_{1}".format(value, dprecision)
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype.intrinsic == intrinsic
    if intrinsic == ScalarType.Intrinsic.BOOLEAN:
        assert ".{0}.".format(literal.value) == value.lower()
    else:
        assert literal.value == value
    assert isinstance(literal.datatype.precision, DataSymbol)
    assert literal.datatype.precision.name == dprecision
    assert isinstance(literal.datatype.precision.datatype, DeferredType)


@pytest.mark.parametrize("value,dprecision,intrinsic",
                         [("0.0", 16, ScalarType.Intrinsic.REAL),
                          ("1", 8, ScalarType.Intrinsic.INTEGER),
                          ("'hello'", 1, ScalarType.Intrinsic.CHARACTER),
                          (".tRue.", 4, ScalarType.Intrinsic.BOOLEAN),
                          (".false.", 8, ScalarType.Intrinsic.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_handling_literal_precision_2(value, dprecision, intrinsic):
    '''Check that the fparser2 frontend can handle literals with a
    specified precision value.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    if intrinsic == ScalarType.Intrinsic.CHARACTER:
        code = "x={0}_{1}".format(dprecision, value)
    else:
        code = "x={0}_{1}".format(value, dprecision)
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype.intrinsic == intrinsic
    if intrinsic == ScalarType.Intrinsic.BOOLEAN:
        assert ".{0}.".format(literal.value) == value.lower()
    else:
        assert literal.value == value
    assert isinstance(literal.datatype.precision, int)
    assert literal.datatype.precision == dprecision


@pytest.mark.parametrize("value,dprecision",
                         [("0.0D0", ScalarType.Precision.DOUBLE),
                          ("0.0d0", ScalarType.Precision.DOUBLE),
                          ("0.0E0", ScalarType.Precision.SINGLE),
                          ("0.0e0", ScalarType.Precision.SINGLE)])
@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_handling_literal_precision_3(value, dprecision):
    '''Check that the fparser2 frontend can handle literals with a
    precision value specified by the exponent. The literal value

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    should always use a lower case "e" for the exponent.
    '''
    code = "x={0}".format(value)
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.value.lower() == "0.0e0"
    assert literal.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert literal.datatype.precision == dprecision


@pytest.mark.parametrize("value,result",
                         [(".3", "0.3"), (".3e4", "0.3e4")])
@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_literal_constant_value_format(value, result):
    '''Test that the Fortran real literal value format which does not have
    a digit before the decimal point is modified to include a "0"
    e.g. ".3" -> "0.3", "-.3e4" -> "-0.3e4"

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader("a = {0}".format(value))
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.value == result
    assert literal.datatype.intrinsic == ScalarType.Intrinsic.REAL


@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_handling_invalid_logic_literal():
    ''' Test that a logic fparser2 literal with an invalid value produces
    an error.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    from psyclone.errors import GenerationError
    reader = FortranStringReader("x = .true.")
    astmt = Fortran2003.Assignment_Stmt(reader)
    astmt.items[2].items = ('invalid', None)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    with pytest.raises(GenerationError) as error:
        processor.process_nodes(fake_parent, [astmt])
    assert "Expected to find '.true.' or '.false.' as fparser2 logical " \
        "literal, but found 'invalid' instead." in str(error.value)


def test_number_handler():
    ''' Check that the number_handler raises a NotImplementedError for an
    unrecognised fparser2 node. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("(1.0, 1.0)")
    with pytest.raises(NotImplementedError):
        processor._number_handler(
            Fortran2003.Complex_Literal_Constant(reader), fake_parent)


# The get_literal_precision() function is covered by the
# test_handling_literal_precision_{1-3} tests above, apart from
# invalid arguments and unsupported datatypes which are tested in the
# next two tests.
@pytest.mark.usefixtures("f2008_parser")
def test_get_literal_precision():
    '''Make sure the get_literal_precision function in fparser2.py behaves
    as expected when the arguments are invalid.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = get_literal_precision(None, None)
    assert ("Unsupported literal type 'NoneType' found in "
            "get_literal_precision." in str(excinfo.value))
    code = "x=0.0"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fparser2_literal = astmt.children[2]
    with pytest.raises(InternalError) as excinfo:
        _ = get_literal_precision(fparser2_literal, None)
    assert ("Expecting argument psyir_literal_parent to be a PSyIR Node but "
            "found 'NoneType' in get_literal_precision." in str(excinfo.value))


@pytest.mark.usefixtures("f2008_parser")
def test_get_literal_precision_type(monkeypatch):
    '''Make sure the get_literal_precision function in fparser2.py behaves
    as expected when an unsupported datatype is found

    '''
    monkeypatch.setattr(fparser2, "CONSTANT_TYPE_MAP", {})
    code = "x=0.0"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fparser2_literal = astmt.children[2]
    with pytest.raises(NotImplementedError) as excinfo:
        _ = get_literal_precision(fparser2_literal, Node())
    assert ("Could not process Real_Literal_Constant. Only 'real', 'integer', "
            "'logical' and 'character' intrinsic types are supported."
            in str(excinfo.value))


@pytest.mark.usefixtures("f2008_parser")
def test_get_literal_precision_missing_table():
    ''' Check that get_literal_precision raises the expected error if it
    fails to find a symbol table. '''
    code = "x=0.0_rdef"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    # Pass get_literal_precision just a Node() (which does not have an
    # associated symbol table).
    with pytest.raises(InternalError) as err:
        get_literal_precision(astmt.children[2], Node())
    assert ("Failed to find a symbol table to which to add the kind"
            in str(err.value))
