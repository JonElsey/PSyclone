! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020, Science and Technology Facilities Council.
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

module testkern_chi_read_mod
  use argument_mod
  use kernel_mod
  use constants_mod
  type, extends(kernel_type) :: testkern_chi_read_type
     type(arg_type), dimension(2) :: meta_args =  &
          (/ arg_type(gh_field,gh_inc,w0),        &
             arg_type(gh_field*3,gh_read,wchi)    &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => testkern_chi_read_code
  end type testkern_chi_read_type
contains
  subroutine testkern_chi_read_code(nlayers, field_1_w0, field_2_wchi_v1,   &
       field_2_wchi_v2, field_2_wchi_v3, ndf_w0, undf_w0, map_w0, ndf_wchi, &
       undf_wchi, map_wchi)
      USE constants_mod, ONLY: r_def, i_def
      IMPLICIT NONE
      INTEGER(KIND=i_def), intent(in) :: nlayers
      INTEGER(KIND=i_def), intent(in) :: ndf_w0
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0
      INTEGER(KIND=i_def), intent(in) :: ndf_wchi
      INTEGER(KIND=i_def), intent(in), dimension(ndf_wchi) :: map_wchi
      INTEGER(KIND=i_def), intent(in) :: undf_w0, undf_wchi
      REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: field_1_w0
      REAL(KIND=r_def), intent(in), dimension(undf_wchi) :: field_2_wchi_v1
      REAL(KIND=r_def), intent(in), dimension(undf_wchi) :: field_2_wchi_v2
      REAL(KIND=r_def), intent(in), dimension(undf_wchi) :: field_2_wchi_v3
  end subroutine testkern_chi_read_code
end module testkern_chi_read_mod
