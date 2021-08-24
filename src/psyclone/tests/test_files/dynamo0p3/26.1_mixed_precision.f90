!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author: R. W. Ford, STFC Daresbury Lab
!
! Mixed precision example where an operator, field and scalar with
! precision r_solver are passed to a kernel.

program operator_example

  use constants_mod,                 only : i_def, r_solver
  use fs_continuity_mod,             only : W0
  use function_space_collection_mod, only : function_space_collection
  use quadrature_xyoz_mod,           only : quadrature_xyoz_type
  use testkern_operator_mod,         only : testkern_operator_type

  use r_solver_field_mod, only : r_solver_field_type
  use r_solver_operator_mod, only : r_solver_operator_type
  use constants_mod, only : r_solver
  
  implicit none

  type(r_solver_FIELD_type)           :: COORD(3)
  type(r_solver_operator_TYPE)        :: MM_w0
  type(Quadrature_xyoz_type), pointer :: qr => null
  real(R_solver)                      :: a

  a = 1.0_r_solver
  call invoke(testkern_operator_type(mm_W0, coord, A, Qr))

end program operator_example
