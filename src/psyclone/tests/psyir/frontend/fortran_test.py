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
# Author S. Siso, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Fortran PSyIR front-end '''

from __future__ import absolute_import
import pytest
from fparser.two import Fortran2003
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Routine, FileContainer, UnaryOperation, \
    BinaryOperation, Literal


# The 'contiguous' keyword is just valid with Fortran 2008
ONLY_2008_CODE = '''
subroutine sub(a)
    integer, dimension(:), contiguous, intent(inout) :: a
end subroutine sub
'''

CODE = '''
subroutine sub(a)
    integer, dimension(:), intent(inout) :: a
    a = a + 1
end subroutine sub
'''


def test_fortran_reader_constructor():
    ''' Test that the constructor initialises the _parser and _processor
    attributes. '''
    freader = FortranReader()
    assert freader._parser is Fortran2003.Program
    assert isinstance(freader._processor, Fparser2Reader)

    # Check that the initialised parser can parse Fortran 2008 standard,
    # the return value of this function is tested in the following tests
    freader.psyir_from_source(ONLY_2008_CODE)


def test_fortran_psyir_from_source():
    ''' Test that the psyir_from_source method parses to PSyIR
    the specified source code. '''
    fortran_reader = FortranReader()
    file_container = fortran_reader.psyir_from_source(CODE)
    assert isinstance(file_container, FileContainer)
    subroutine = file_container.children[0]
    assert isinstance(subroutine, Routine)


def test_fortran_psyir_from_expression(fortran_reader):
    ''' Test that the psyir_from_expression method generates the
    expected PSyIR. '''
    psyir = fortran_reader.psyir_from_expression("3.0")
    assert isinstance(psyir, Literal)
    assert psyir.value == "3.0"
    psyir = fortran_reader.psyir_from_expression("-3.0 + 1.0")
    assert isinstance(psyir, BinaryOperation)
    assert psyir.operator == BinaryOperation.Operator.ADD
    assert isinstance(psyir.children[0], UnaryOperation)
    assert psyir.children[0].operator == UnaryOperation.Operator.MINUS
    assert isinstance(psyir.children[0].children[0], Literal)
    assert psyir.children[0].children[0].value == "3.0"
    psyir = fortran_reader.psyir_from_expression("ABS(-3.0)")
    assert isinstance(psyir, UnaryOperation)
    assert psyir.operator == UnaryOperation.Operator.ABS
    assert isinstance(psyir.children[0], UnaryOperation)
    assert psyir.children[0].operator == UnaryOperation.Operator.MINUS
    assert isinstance(psyir.children[0].children[0], Literal)
    with pytest.raises(NotImplementedError) as err:
        fortran_reader.psyir_from_expression("3.0 + a")
    assert "Expression must contain only literals: '3.0 + a'" in str(err.value)
    with pytest.raises(NotImplementedError) as err:
        fortran_reader.psyir_from_expression("3.0 + sin(a)")
    assert ("Expression must contain only literals: '3.0 + sin(a)'" in
            str(err.value))


def test_fortran_psyir_from_expression_invalid(fortran_reader):
    ''' Test that the psyir_from_expression method raises the expected
    error when given something that is not an expression. '''
    with pytest.raises(NotImplementedError) as err:
        fortran_reader.psyir_from_expression("return")
    assert "Expression must contain only literals: 'return'" in str(err.value)
    with pytest.raises(ValueError) as err:
        fortran_reader.psyir_from_expression("a = b")
    assert "not represent a Fortran expression: 'a = b'" in str(err.value)
    with pytest.raises(ValueError) as err:
        fortran_reader.psyir_from_expression("if(3 == 2)then")
    assert ("not represent a Fortran expression: 'if(3 == 2)then'" in
            str(err.value))
    with pytest.raises(ValueError) as err:
        fortran_reader.psyir_from_expression("this is not Fortran")
    assert ("not represent a Fortran expression: 'this is not Fortran'" in
            str(err.value))


def test_fortran_psyir_from_file(tmpdir_factory):
    ''' Test that the psyir_from_file method reads and parses to PSyIR
    the specified file. '''
    filename = str(tmpdir_factory.mktemp('frontend_test').join("testfile.f90"))
    with open(filename, "w") as wfile:
        wfile.write(CODE)

    # Check with a proper file
    fortran_reader = FortranReader()
    file_container = fortran_reader.psyir_from_file(filename)
    assert isinstance(file_container, FileContainer)
    subroutine = file_container.children[0]
    assert isinstance(subroutine, Routine)

    # Check with a file that doesn't exist
    filename = str(tmpdir_factory.mktemp('frontend_test').join("Idontexist"))
    with pytest.raises(IOError) as err:
        fortran_reader.psyir_from_file(filename)
    assert "No such file or directory: '" + str(filename) in str(err.value)
