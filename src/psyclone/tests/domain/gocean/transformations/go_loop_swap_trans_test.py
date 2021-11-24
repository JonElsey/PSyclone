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
# ----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' Module containing tests of Loop Swap Transformation when using the
    GOcean 1.0 API '''

from __future__ import absolute_import
import re

import pytest
from psyclone.configuration import Config
from psyclone.domain.gocean.transformations import (GOceanLoopFuseTrans,
                                                    GOLoopSwapTrans)
from psyclone.gocean1p0 import GOLoop
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.gocean1p0_build import GOcean1p0Build
from psyclone.tests.utilities import get_invoke

# The version of the PSyclone API that the tests in this file
# exercise
API = "gocean1.0"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use gocean1.0 as API.'''
    Config.get().api = "gocean1.0"
    yield()
    Config._instance = None


def test_loop_swap_correct(tmpdir):
    ''' Testing correct loop swapping transform. Esp. try first, middle, and
    last invokes to make sure the inserting of the inner loop happens at
    the right place.'''

    psy, _ = get_invoke("test27_loop_swap.f90", API, idx=0, dist_mem=False)
    invoke = psy.invokes.get("invoke_loop1")
    schedule = invoke.schedule
    schedule_str = str(schedule)

    # First make sure to throw an early error if the source file
    # test27_loop_swap.f90 should have been changed
    expected = (
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the first loops
    swap = GOLoopSwapTrans()
    swap.apply(schedule.children[0])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the middle loops
    swap.apply(schedule.children[1])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the last loops
    swap.apply(schedule.children[2])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_go_loop_swap_errors():
    ''' Test loop swapping transform with incorrect parameters. '''

    psy, invoke_loop1 = get_invoke("test27_loop_swap.f90", API, idx=1,
                                   dist_mem=False)

    schedule = invoke_loop1.schedule
    swap = GOLoopSwapTrans()
    assert str(swap) == "Exchange the order of two nested loops: inner "\
        "becomes outer and vice versa"

    # Test error if given node is not the outer loop of at least
    # a double nested loop:
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0])
    assert re.search("Transformation Error: Target of GOLoopSwapTrans "
                     "transformation must be a sub-class of Loop but got "
                     "'GOKern'.", str(error.value), re.S) is not None

    # Not a loop: use the call to bc_ssh_code node as example for this test:
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0].loop_body[0])
    assert ("Target of GOLoopSwapTrans transformation must be a sub-class of "
            "Loop but got 'GOKern'" in str(error.value))

    # Now create an outer loop with more than one inner statement
    # ... by fusing the first and second outer loops :(
    invoke_loop2 = psy.invokes.get("invoke_loop2")
    schedule = invoke_loop2.schedule

    fuse = GOceanLoopFuseTrans()
    fuse.apply(schedule.children[0], schedule.children[1])

    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "and must have exactly one inner loop, but this node "
                     "has 2 inner statements, the first two being .* and .*",
                     str(error.value), re.S) is not None

    # Now remove the body of the first inner loop, and pass the first
    # inner loop --> i.e. a loop with an empty body
    del schedule.children[0].loop_body[0].children[3].children[0]

    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "and must have one inner loop, but this node does not "
                     "have any statements inside.",
                     str(error.value), re.S) is not None


def test_go_loop_swap_wrong_loop_type():
    '''
    Test loop swapping transform when supplied loops are not GOLoops.
    '''
    swap = GOLoopSwapTrans()
    _, invoke = get_invoke("1.0.1_single_named_invoke.f90",
                           "dynamo0.3", idx=0, dist_mem=True)
    with pytest.raises(TransformationError) as error:
        swap.apply(invoke.schedule.children[4])

    assert re.search("Given node .* is not a GOLoop, but an instance of "
                     ".*DynLoop", str(error.value), re.S) is not None

    _, invoke_loop1 = get_invoke("test27_loop_swap.f90", API, idx=1,
                                 dist_mem=False)
    schedule = invoke_loop1.schedule
    loop = schedule[0].loop_body[0]
    assert isinstance(loop, GOLoop)
    # Change the class of the inner loop so that it is not a GOLoop
    loop.__class__ = Loop
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule[0])
    assert "is not a GOLoop, but an instance of 'Loop'" in str(error.value)
