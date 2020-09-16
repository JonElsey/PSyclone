# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# Modified J. Henrichs, Bureau of Meteorology

''' This module tests the LFric classes based on ArgOrdering.'''

from __future__ import absolute_import
import os
import pytest

from psyclone.domain.lfric import (KernCallArgList,
                                   KernStubArgList, LFRicArgDescriptor)
from psyclone.dynamo0p3 import DynKern, DynKernMetadata, DynLoop
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_ast, get_base_path, get_invoke

TEST_API = "dynamo0.3"


def test_unexpected_type_error(dist_mem):
    ''' Check that we raise an exception if an unexpected datatype is found
    when running the ArgOrdering generate method. As it is abstract we use
    the KernCallArgList sub class.

    '''
    full_path = os.path.join(get_base_path(TEST_API),
                             "1.0.1_single_named_invoke.f90")
    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    if dist_mem:
        index = 4
    else:
        index = 0
    loop = schedule.children[index]
    kernel = loop.loop_body[0]
    # Sabotage one of the arguments to make it have an invalid type.
    kernel.arguments.args[0]._argument_type = "invalid"
    # Now call KernCallArgList to raise an exception
    create_arg_list = KernCallArgList(kernel)
    with pytest.raises(GenerationError) as excinfo:
        create_arg_list.generate()
    assert (
        "ArgOrdering.generate(): Unexpected argument "
        "type found. Expected one of '{0}' but found 'invalid'".
        format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES)
        in str(excinfo.value))


def test_kernel_stub_invalid_scalar_argument():
    ''' Check that we raise an exception if an unexpected datatype is found
    when using the KernStubArgList scalar method. '''
    ast = get_ast(TEST_API, "testkern_one_int_scalar_mod.f90")

    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Sabotage the scalar argument to make it have an invalid type.
    arg = kernel.arguments.args[1]
    arg._argument_type = "invalid"
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
    with pytest.raises(InternalError) as excinfo:
        create_arg_list.scalar(arg)
    assert ("Expected argument type to be one of {0} but got "
            "'invalid'".format(LFRicArgDescriptor.VALID_SCALAR_NAMES)
            in str(excinfo.value))


def test_kernel_stub_ind_dofmap_errors():
    '''Check that we raise the expected exceptions if the wrong arguments
    are supplied to KernelStubArgList.indirection_dofmap() '''
    ast = get_ast(TEST_API, "testkern_one_int_scalar_mod.f90")
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
    # First call it without an argument object
    with pytest.raises(InternalError) as excinfo:
        create_arg_list.indirection_dofmap("w3")
    assert "No CMA operator supplied" in str(excinfo.value)
    # Second, call it with an argument object but one that is not
    # an operator
    with pytest.raises(InternalError) as excinfo:
        create_arg_list.indirection_dofmap("w3", kernel.arguments.args[1])
    assert ("A CMA operator (gh_columnwise_operator) must be supplied but "
            "got") in str(excinfo.value)


def test_kerncallarglist_args_error(dist_mem):
    '''Check that we raise an exception if we call the methods that return
    information in kerncallarglist without first calling the generate
    method

    '''
    psy, _ = get_invoke("1.0.1_single_named_invoke.f90", api=TEST_API,
                        dist_mem=dist_mem, idx=0)
    schedule = psy.invokes.invoke_list[0].schedule
    if dist_mem:
        loop = schedule.children[4]
    else:
        loop = schedule.children[0]
    create_arg_list = KernCallArgList(loop.loop_body[0])

    # nlayers_positions method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.nlayers_positions
    assert (
        "KernCallArgList: the generate() method should be called before "
        "the nlayers_positions() method") in str(excinfo.value)

    # nqp_positions method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.nqp_positions
    assert (
        "KernCallArgList: the generate() method should be called before "
        "the nqp_positions() method") in str(excinfo.value)

    # ndf_positions method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.ndf_positions
    assert (
        "KernCallArgList: the generate() method should be called before "
        "the ndf_positions() method") in str(excinfo.value)

    # arglist method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.arglist
    assert (
        "The argument list in KernCallArgList "
        "is empty. Has the generate() method been called?"
        ) in str(excinfo.value)


def test_kerncallarglist_quad_rule_error(dist_mem, tmpdir):
    ''' Check that we raise the expected exception if we encounter an
    unsupported quadrature shape in the quad_rule() method. '''
    psy, _ = get_invoke("6_multiple_QR_per_invoke.f90", TEST_API,
                        dist_mem=dist_mem, idx=0)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.walk(DynLoop)[0]
    create_arg_list = KernCallArgList(loop.loop_body[0])
    # Add an invalid shape to the dict of qr rules
    create_arg_list._kern.qr_rules["broken"] = None
    with pytest.raises(NotImplementedError) as err:
        create_arg_list.quad_rule()
    assert ("no support implemented for quadrature with a shape of 'broken'"
            in str(err.value))


def test_kernstubarglist_arglist_error():
    '''Check that we raise an exception if we call the arglist method in
    kernstubarglist without first calling the generate method'''
    ast = get_ast(TEST_API, "testkern_one_int_scalar_mod.f90")

    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.arglist
    assert (
        "The argument list in KernStubArgList is "
        "empty. Has the generate() method been "
        "called?") in str(excinfo.value)


def test_kernstubarglist_eval_shape_error():
    ''' Check that we raise the expected exception if we call the basis() or
    diff_basis() methods and one of the kernel's evaluator shapes is
    invalid. '''
    ast = get_ast(TEST_API, "testkern_qr_faces_mod.F90")
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    create_arg_list = KernStubArgList(kernel)
    # Break the list of qr rules
    kernel.eval_shapes.insert(0, "broken")
    with pytest.raises(InternalError) as err:
        create_arg_list.basis(None)
    assert ("Unrecognised evaluator shape ('broken'). Expected one of: "
            "['gh_quadrature_xyoz'" in str(err.value))
    with pytest.raises(InternalError) as err:
        create_arg_list.diff_basis(None)
    assert ("Unrecognised evaluator shape ('broken'). Expected one of: "
            "['gh_quadrature_xyoz'" in str(err.value))


def test_refelem_stub_arglist_err():
    ''' Check that the KernStubArgList.ref_element_properties method raises
    the expected error if it encounters an unsupported property. '''
    # Create the Kernel object
    ast = get_ast(TEST_API, "testkern_ref_elem_all_faces_mod.F90")
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Break the list of ref-element properties required by the Kernel
    kernel.reference_element.properties.append("Wrong property")
    with pytest.raises(InternalError) as err:
        KernStubArgList(kernel).generate()
    assert "('Wrong property') " in str(err.value)
    assert (
        "Supported properties are: ['Property.NORMALS_TO_HORIZONTAL_FACES', "
        "'Property.NORMALS_TO_VERTICAL_FACES', 'Property.NORMALS_TO_FACES', "
        "'Property.OUTWARD_NORMALS_TO_HORIZONTAL_FACES', "
        "'Property.OUTWARD_NORMALS_TO_VERTICAL_FACES', "
        "'Property.OUTWARD_NORMALS_TO_FACES']" in str(err.value))
