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
# Author I. Kavcic, Met Office
# Modified by A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Module containing tests for PSyclone ExtractTrans
and ExtractNode.
'''

from __future__ import absolute_import

import pytest

from psyclone.psyir.nodes import ExtractNode
from psyclone.psyir.transformations import ExtractTrans
from psyclone.domain.lfric.transformations import LFRicExtractTrans

# --------------------------------------------------------------------------- #
# ================== Extract Transformation tests =========================== #
# --------------------------------------------------------------------------- #


def test_extract_trans():
    '''Tests basic functions in ExtractTrans.'''
    etrans = ExtractTrans()
    assert str(etrans) == "Create a sub-tree of the PSyIR that has " \
                          "ExtractNode at its root."
    assert etrans.name == "ExtractTrans"

    ltrans = LFRicExtractTrans()
    assert str(ltrans) == "Inserts an ExtractNode in an LFRic Schedule."


def test_malformed_extract_node(monkeypatch):
    ''' Check that we raise the expected error if an ExtractNode does not have
    a single Schedule node as its child. '''
    from psyclone.psyir.nodes import Node
    from psyclone.errors import InternalError
    enode = ExtractNode()
    monkeypatch.setattr(enode, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = enode.extract_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(enode, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = enode.extract_body
    assert "malformed or incomplete. It should have a " in str(err.value)
