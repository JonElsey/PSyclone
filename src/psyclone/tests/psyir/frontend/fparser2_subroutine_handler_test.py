# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, STFC Daresbury Lab
#          A. R. Porter, STFC Daresbury Lab

'''Module containing pytest tests for the _subroutine_handler method
in the class Fparser2Reader. This handler deals with the translation
of the fparser2 Subroutine_Subprogram and Function_Subprogram constructs
to PSyIR.

'''
from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.symbols import DataSymbol, ScalarType
from psyclone.psyir.nodes import Container, Routine, CodeBlock, FileContainer
from psyclone.psyir.frontend.fparser2 import Fparser2Reader

# subroutine no declarations
SUB1_IN = (
    "subroutine sub1()\n"
    "end subroutine\n")
SUB1_OUT = (
    "subroutine sub1()\n\n\n"
    "end subroutine sub1\n")
# subroutine with symbols/declarations
SUB2_IN = (
    "subroutine sub1(a)\n"
    "real :: a\n"
    "end subroutine\n")
SUB2_OUT = (
    "subroutine sub1(a)\n"
    "  real, intent(inout) :: a\n\n\n"
    "end subroutine sub1\n")
# subroutine with executable content
SUB3_IN = (
    "subroutine sub1()\n"
    "real :: a\n"
    "a=0.0\n"
    "end subroutine\n")
SUB3_OUT = (
    "subroutine sub1()\n"
    "  real :: a\n\n"
    "  a = 0.0\n\n"
    "end subroutine sub1\n")


@pytest.mark.parametrize("code,expected",
                         [(SUB1_IN, SUB1_OUT),
                          (SUB2_IN, SUB2_OUT),
                          (SUB3_IN, SUB3_OUT)])
def test_subroutine_handler(parser, fortran_writer, code, expected):
    '''Test that subroutine_handler handles valid Fortran subroutines.'''

    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    subroutine = parse_tree.children[0]
    psyir = processor._subroutine_handler(subroutine, None)
    # Check the expected PSyIR nodes are being created
    assert isinstance(psyir, Routine)
    assert psyir.parent is None
    result = fortran_writer(psyir)
    assert expected == result


def test_function_handler(fortran_reader, fortran_writer):
    '''Test that subroutine_handler correctly handles a function defined
    within a module.

    '''
    code = (
        "module a\n"
        "contains\n"
        "  function my_func()\n"
        "    integer :: my_func\n"
        "    my_func = 1\n"
        "  end function my_func\n"
        "end module\n")
    expected = (
        "module a\n"
        "  implicit none\n\n"
        "  public :: my_func\n\n"
        "  contains\n"
        "  function my_func()\n"
        "    integer :: my_func\n"
        "\n"
        "    my_func = 1\n"
        "\n"
        "  end function my_func\n"
        "\n"
        "end module a\n")
    psyir = fortran_reader.psyir_from_source(code)
    # Check PSyIR nodes are being created
    assert isinstance(psyir, Container)
    routines = psyir.walk(Routine)
    assert len(routines) == 1
    assert isinstance(routines[0].return_symbol, DataSymbol)
    assert routines[0].return_symbol.name == "my_func"
    assert (routines[0].return_symbol.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert psyir.parent is None
    result = fortran_writer(psyir)
    assert result == expected


@pytest.mark.parametrize("basic_type", ["real", "integer"])
def test_function_type_prefix(fortran_reader, fortran_writer, basic_type):
    '''
    Test the handler when the function definition has a type prefix but no
    result suffix.

    '''
    code = (
        "module a\n"
        "contains\n"
        "  {0} function my_func()\n"
        "    my_func = 1\n"
        "  end function my_func\n"
        "end module\n".format(basic_type))
    expected = (
        "module a\n"
        "  implicit none\n\n"
        "  public :: my_func\n\n"
        "  contains\n"
        "  function my_func()\n"
        "    {0} :: my_func\n"
        "\n"
        "    my_func = 1\n"
        "\n"
        "  end function my_func\n"
        "\n"
        "end module a\n".format(basic_type))
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir, FileContainer)
    module = psyir.children[0]
    assert isinstance(module, Container)
    routine = module.children[0]
    assert isinstance(routine, Routine)
    return_sym = routine.return_symbol
    assert isinstance(return_sym, DataSymbol)
    if basic_type == "real":
        assert return_sym.datatype.intrinsic == ScalarType.Intrinsic.REAL
    else:
        assert return_sym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    result = fortran_writer(psyir)
    assert result == expected


FN1_IN = ("  function my_func() result(my_val)\n"
          "    real :: my_val\n"
          "    my_val = 1.0\n"
          "  end function my_func\n")
FN2_IN = ("  real function my_func() result(my_val)\n"
          "    my_val = 1.0\n"
          "  end function my_func\n")
FN3_IN = ("  real(wp) function my_func() result(my_val)\n"
          "    my_val = 1.0\n"
          "  end function my_func\n")
EXPECTED_FN_OUT = ("  function my_func() result(my_val)\n"
                   "    real :: my_val\n\n"
                   "    my_val = 1.0\n\n"
                   "  end function my_func\n")
EXPECTED_FN3_OUT = ("  function my_func() result(my_val)\n"
                    "    real(kind=wp) :: my_val\n\n"
                    "    my_val = 1.0\n\n"
                    "  end function my_func\n")


@pytest.mark.parametrize("code,expected",
                         [(FN1_IN, EXPECTED_FN_OUT),
                          (FN2_IN, EXPECTED_FN_OUT),
                          (FN3_IN, EXPECTED_FN3_OUT)])
def test_function_result_suffix(fortran_reader, fortran_writer,
                                code, expected):
    '''
    Test that we handle a Fortran function with the return value specified
    using the 'result()' suffix. We test when the type is specified by a
    declaration inside the function or by a type specifier in the function
    prefix.

    '''
    code = (
        "module a\n"
        "use kind_params, only: wp\n"
        "contains\n"
        "{0}end module\n".format(code))
    psyir = fortran_reader.psyir_from_source(code)
    # Check PSyIR nodes are being created
    assert isinstance(psyir, Container)
    routines = psyir.walk(Routine)
    assert len(routines) == 1
    assert (routines[0].return_symbol is
            routines[0].symbol_table.lookup("my_val"))
    result = fortran_writer(psyir)
    assert expected in result


def test_function_missing_return_type(fortran_reader):
    '''
    Test that we generate a CodeBlock for a Fortran function without an
    explicit declaration of its return type (i.e. if it's relying on Fortran's
    implicit typing).

    '''
    code = (
        "module a\n"
        "contains\n"
        "  function my_func()\n"
        "    my_func = 1.0\n"
        "  end function my_func\n"
        "end module\n")
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)


@pytest.mark.parametrize("fn_prefix",
                         ["pure real", "real pure", "recursive", "elemental"])
def test_unsupported_function_prefix(fortran_reader, fn_prefix):
    ''' Check that we get a CodeBlock if a Fortran function has an unsupported
    prefix. '''
    code = (
        "module a\n"
        "contains\n"
        "  {0} function my_func()\n"
        "    my_func = 1.0\n"
        "  end function my_func\n"
        "end module\n".format(fn_prefix))
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)
