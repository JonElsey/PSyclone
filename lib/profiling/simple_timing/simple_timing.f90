! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018, Australian Bureau of Meteorology
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
! Authors J. Henrichs, Bureau of Meteorology


!> A very simple stand-alone profiling library for PSyclone's
!> profiling API.
module profile_psy_data_mod

  !> The datatype to store information about a region.
  type :: profile_PSyDataType
     !> Name of the module.
     character(:), allocatable :: module_name
     !> Name of the region.
     character(:), allocatable :: region_name
     !> Counts how often this region was executed.
     integer                   :: count
     !> Time at whith PreStart was called.
     real*4                    :: start
     !> Overall time spent in this subroutine, i.e. sum
     !> of each individual call..
     real*4                    :: sum
     !> Shortest measured time of this region.
     real*4                    :: min
     !> Sum Longest measured time of this region.
     real*4                    ::  max
     !> Inidicates if this structure has been initialised.
     logical                   :: initialised = .false.
  contains
      ! The profiling API uses only the two following calls:
      procedure :: PreStart, PostEnd
  end type profile_PSyDataType

  ! --------------------------------------------------------
  !> In order to store an array of pointers, Fortran requires
  !> a new type *sigh*
  type PSyDataTypePointer
     !> The actual pointer to the data in the user's program.
     type(profile_PSyDataType), pointer :: p
  end  type PSyDataTypePointer
  ! --------------------------------------------------------

  !> Maximum number of regions supported. Additional regions
  !> will be silently ignored.
  integer, parameter :: MAX_DATA = 100

  !> This keeps track of all user data areas.
  type(PSyDataTypePointer), dimension(MAX_DATA) :: all_data

  !> How many entries in all_data have been used
  integer :: used_entries

  !> Keeps track if profile_PSyDataInit has been called.
  logical :: has_been_initialised = .false.

contains

  ! ---------------------------------------------------------------------------
  !> The initialisation subroutine. It is not called directly from
  !> any PSyclone created code, so the user has to manually insert a
  !> call to this subroutine. But the simple timing library will
  !> actually call this function itself if it has not been called previously.
  subroutine profile_PSyDataInit()
    implicit none

    used_entries         = 0
    has_been_initialised = .true.

  end subroutine profile_PSyDataInit

  ! ---------------------------------------------------------------------------
  !> Starts a profiling area. The module and region name can be used to create
  !> a unique name for each region.
  !! Parameters:
  !! @param[inout] this This PSyData instance.
  !! @param[in] module_name Name of the module in which the region is
  !! @param[in] region_name Name of the region (could be name of an invoke, or
  !!            subroutine name).
  !! @param[in] num_pre_vars The number of variables that are declared and
  !!            written before the instrumented region.
  !! @param[in] num_post_vars The number of variables that are also declared
  !!            before an instrumented region of code, but are written after
  !!            this region.

  subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                      num_post_vars)
    implicit none

    class(profile_PSyDataType), intent(inout), target :: this
    character*(*)       :: module_name, region_name
    integer             :: count, count_rate
    integer, intent(in) :: num_pre_vars, num_post_vars

    if ( .not. has_been_initialised ) then
       call profile_PSyDataInit()
    endif

    ! Note that the actual initialisation of this
    ! happens in PostEnd, which is when min, sum and
    ! max are properly initialised
    this%module_name = module_name
    this%region_name = region_name
    call system_clock(count, count_rate)
    this%start  = real(count) / count_rate

  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !> Ends a profiling area. It takes a PSyDataType type that corresponds to
  !> to the PreStart call.
  !> @param[inout] this: This PSyData instance.
  subroutine PostEnd(this)
    implicit none

    class(profile_PSyDataType), intent(inout), target :: this

    integer :: count, count_rate
    real *4 :: now, duration
    
    call system_clock(count, count_rate)
    now = real(count) / count_rate
    duration = now - this%start

    ! Now initialise the data
    if (.not. this%initialised) then
       this%sum         = duration
       this%min         = this%sum
       this%max         = this%sum
       this%count       = 1
       this%initialised = .true.

       if (used_entries < MAX_DATA) then
          ! Save a pointer to the profiling data
          used_entries = used_entries + 1
          all_data(used_entries)%p => this
       endif
    else
       this%sum = this%sum + duration
       if (duration < this%min ) this%min = duration
       if (duration > this%max ) this%max = duration
       this%count = this%count + 1
    endif

  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> The finalise function prints the results. This subroutine must be called,
  !> otherwise no results will be printed.
  subroutine profile_PSyDataShutdown()
    implicit none
    integer                    :: i
    integer                    :: max_len, this_len
    type(profile_PSyDataType), pointer :: p
    character                  :: tab = char(9)
    character(:), allocatable  :: heading
    character(:), allocatable  :: spaces

    heading = "module::region"
    ! Determine maximum header length to get proper alignment
    max_len = len(heading)
    do i=1, used_entries
       p => all_data(i)%p    ! to abbreviate code a bit
       if (len(p%module_name) + len(p%region_name) > max_len) then
          max_len = len(p%module_name) + len(p%region_name)
       endif
    enddo

    ! Allow for "::" and one additional space:
    max_len = max_len + 3

    allocate(character(len=max_len) :: spaces)
    do i=1, max_len
       spaces(i:i) = " "
    enddo

    print *
    print *,"==========================================="
    print *, heading, spaces(1:max_len - len(heading)),                       &
             tab, "count", tab, tab, "sum", tab, tab, tab, "min", tab, tab,   &
             "average", tab, tab, tab, "max"
    do i=1, used_entries
       p => all_data(i)%p    ! to abbreviate code a bit
       this_len = len(p%module_name) + len(p%region_name)+3
       print *, p%module_name,"::",p%region_name, spaces(1:max_len-this_len), &
                p%count, tab, p%sum, tab,                                     &
                p%min, tab, p%sum/p%count, tab, p%max
    end do
    print *,"==========================================="
  end subroutine profile_PSyDataShutdown

end module profile_psy_data_mod
