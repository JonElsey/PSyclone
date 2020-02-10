# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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
# Author R. W. Ford STFC Daresbury Lab.


'''An example PSyclone transformation script to demonstrate
optimisations to the matrix vector kernel to improve its performance
on CPUs.

The matrix vector kernel has been hand optimised for CPUs. This script
will automate these optimisations.

Optimising matvec in PSyclone is work in progress. At the moment the
only automated optimisation possible is the kernel constant
transformation (see eg13) and this does little to matvec as it stands.

Below is a list of things that will be implemented to improve
performance but are not yet supported as transformations in PSyclone.

1) replace Fortran MATMUL intrinsic with equivalent code
   a) get PSyIR to recognise array slice notation
   b) replace MATMUL with equivalent inline code
2) loop fuse gather and matmul loop
3) move indexing lookup before scatter loop
4) loop fuse scatter loop and matmul loop
5) remove scatter and gather
6) interchange k loop to make it inner
7) re-order data-layout for matrix
8) replicate kernel to support specific function spaces (psy-layer
   optimisation)
9) add kernel constants for nlayers, ndf2, ndf1 (existing transformation)

This script can be applied via the '-s' option when running PSyclone:

$ psyclone -s ./matvec_opt.py \
../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
-oalg /dev/null -opsy /dev/null

'''
from __future__ import print_function
from psyclone.psyir.backend.fortran import FortranWriter


def trans(psy):
    '''PSyclone transformation script for the Dynamo0.3 API to optimise
    the matvec kernel for many-core CPUs. For the moment simply find
    the first matvec kernel in the example and then print out its
    PSyIR representation and output it as Fortran using the PSyIR
    Fortran back-end.

    '''
    fortran_writer = FortranWriter()
    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule
        for kernel in schedule.coded_kernels():
            if kernel.name.lower() == "matrix_vector_kernel_code":
                kernel_schedule = kernel.get_kernel_schedule()
                # optimisations will go here.
                kernel_schedule.view()
                result = fortran_writer(kernel_schedule)
                print(result)
                # Abort after the first matrix vector kernel for the
                # time being.
                print("Aborting to view the modifications to the matrix "
                      "vector kernel")
                exit(1)
    return psy
