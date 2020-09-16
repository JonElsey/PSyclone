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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Authors J. Henrichs, Bureau of Meteorology

!> This module implements a verification that read-only fields (in the LFRic
!! infrastructure) are not overwritten (due to memory overwrites etc).
!! 

module read_only_verify_psy_data_mod
    use, intrinsic :: iso_fortran_env, only : int64, int32, &
                                              stderr=>Error_Unit
    implicit none

    !> Maximum string length for module- and region-names
    integer, parameter :: MAX_STRING_LENGTH = 512

    !> This is the data type that stores a checksum for each read-only
    !! variable. A static instance of this type is created for each
    !! instrumented region with PSyclone.

    type, public:: read_only_verify_PSyDataType
        !> This field stores a 64-bit integer checksum for each
        !! variable.
        integer(kind=int64), dimension(:), allocatable :: checksums

        !> The index of the variables as they are being declared
        !! and as they are being written. This index is used
        !! to get the variable id when writing data (which depends
        !! on the fact that declaration is done in the same order
        !! in which the values are provided).
        integer :: next_var_index

        !> This boolean flag switches from 'computing and storing checksum'
        !! to 'verify checksum'.
        logical :: verify_checksums

        !> Verbosity level for output at runtime. This is taken from the
        !! PSYDATA_VERBOSE environment variable.
        !! 0: Only errors will be written (PSYDATA_VERBOSE undefined)
        !! 1: Additionally write the name of the confirmed kernel_name
        !! 2: Also write the name of each tested variable
        integer :: verbosity

        !> Store the name of the module and region
        character(MAX_STRING_LENGTH) :: module_name, region_name

    contains
        ! The various procedures used
        procedure :: DeclareScalarInt,    ChecksumScalarInt
        procedure :: DeclareScalarReal,   ChecksumScalarReal
        procedure :: DeclareScalarDouble, ChecksumScalarDouble
        procedure :: DeclareFieldDouble,  ChecksumFieldDouble
        procedure :: PreStart, PreEndDeclaration, PreEnd
        procedure :: PostStart, PostEnd

        !> The generic interface for declaring a variable:
        generic, public :: PreDeclareVariable => DeclareScalarInt,    &
                                                 DeclareScalarReal,   &
                                                 DeclareScalarDouble, &
                                                 DeclareFieldDouble

        !> The generic interface for providing the value of variables,
        !! which in this case is the checksum computation (before
        !! the kernel), and checksum verification after the kernel. The
        !! same functions are used, it uses verify_checksums to change
        !! the state from checksum computation to verication.
        generic, public :: ProvideVariable => ChecksumScalarInt,    &
                                              ChecksumScalarReal,   &
                                              ChecksumScalarDouble, &
                                              ChecksumFieldDouble

    end type read_only_verify_PSyDataType

Contains

    ! -------------------------------------------------------------------------
    !> This subroutine is the first function called when an instrumented region
    !! is entered. It initialises this object, and stores module and regin
    !! names. 
    !! @param[inout] this The instance of the PSyDataType.
    !! @param[in] module_name The name of the module of the instrumented
    !!            region.
    !! @param[in] region_name The name of the instrumented region.
    !! @param[in] num_pre_vars The number of variables that are declared and
    !!            written before the instrumented region.
    !! @param[in] num_post_vars The number of variables that are also declared
    !!            before an instrumented region of code, but are written after
    !!            this region.
    subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                        num_post_vars)
        use gocean_mod, only: gocean_stop
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: module_name, region_name
        integer, intent(in)      :: num_pre_vars, num_post_vars

        character(1) :: verbose
        integer :: status

        call get_environment_variable("PSYDATA_VERBOSE", verbose, status=status)
        this%verbosity=0
        if (status==0) then
            if(verbose=="0") then
                this%verbosity = 0
            else if (verbose=="1") then
                this%verbosity = 1
            else if (verbose=="2") then
                this%verbosity = 2
            else
                write(stderr,*) "PSyData: invalid setting of PSYDATA_VERBOSE."
                write(stderr,*) "It must be '0', 1' or '2', but it is '", verbose,"'."
                this%verbosity = 0
            endif
        endif

        if (num_pre_vars /= num_post_vars) then
            write(stderr,*) "PSyData: The same number of variables must be provided before"
            write(stderr,*) "and after the instrumented region. But the values are:"
            write(stderr,*) "Before: ", num_pre_vars, " after: ", num_post_vars
            call gocean_stop("Inconsistent parameters.")
        endif

        if(this%verbosity>0) &
            write(stderr,*) "PSyData: checking ", module_name, " ", region_name

        allocate(this%checksums(num_pre_vars+num_post_vars), stat=status)
        if(status/=0) then
            write(stderr, *) "Could not allocate ", num_pre_vars+num_post_vars, &
                             " integers, aborting."
            call gocean_stop("Out of memory")
        endif
        this%next_var_index = 1
        this%verify_checksums = .false.
        this%module_name = module_name
        this%region_name = region_name
    end subroutine PreStart

    ! -------------------------------------------------------------------------
    !> This subroutine is called once all variables are declared. It makes
    !! sure that the next variable index is starting at 1 again.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PreEndDeclaration(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        this%next_var_index = 1
    end subroutine PreEndDeclaration
    ! -------------------------------------------------------------------------
    !> This subroutine is called after the value of all variables has been
    !! provided (and declared). After this call the instrumented region will
    !! be executed. Nothing is required to be done here.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PreEnd(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
    end subroutine PreEnd

    ! -------------------------------------------------------------------------
    !> This subroutine is called after the instrumented region has been
    !! executed. After this call the value of variables after the instrumented
    !! region will be provided. This subroutine sets the 'verify_checksum'
    !! flag to true, causing all further checksum calls to verify that the
    !! checksum has not changed. It also resets the next variable index to 1
    !! again.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PostStart(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        this%verify_checksums = .true.
        this%next_var_index = 1
    end subroutine PostStart

    ! -------------------------------------------------------------------------
    !> This subroutine is called after the instrumented region has been
    !! executed and all values of variables after the instrumented
    !! region have been provided. No special functionality required here.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PostEnd(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this

        if(this%verbosity>0) &
            write(stderr, *) "PSyData: checked ", trim(this%module_name), &
                    " ", trim(this%region_name)
    end subroutine PostEnd

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar integer value. It does nothing since
    !! this is not required for read-only verification.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value
    end subroutine DeclareScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums). Since the quantity is a scalar,
    !! no actual summation is required.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumScalarInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value

        if (this%verify_checksums) then
            if (value /= this%checksums(this%next_var_index)) then
                write(stderr, *) "------------------- PSyData -------------------"
                write(stderr, *) "Integer variable ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                ! In case of integer variables, we can use the checksum as the
                ! original value:
                write(stderr, *) "Original value: ", this%checksums(this%next_var_index)
                write(stderr, *) "New value:      ", value
                write(stderr, *) "------------------- PSyData -------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSyData: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = value
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar single precision value. It does
    !! nothing since this is not required for read-only verification.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, intent(in) :: value
    end subroutine DeclareScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums). Since the value is a 32-bit
    !! scalar, the 32-bit value is stored in the 64-bit checksum field.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumScalarReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, intent(in) :: value
        real :: orig_value
        integer(kind=int64) :: chksum64
        integer(kind=int32) :: chksum32

        ! Type-cast from real to 32-bit int (same size, so
        ! no undefined bits in result)
        chksum32 = transfer(value, chksum32)
        ! Now assign to 64 bit, so we have a 64 bit checksum
        chksum64 = chksum32

        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= chksum64) then
                ! Convert back to 32 bit, and then cast to real:
                chksum32 = this%checksums(this%next_var_index)
                orig_value = transfer(chksum32, orig_value)
                write(stderr, *) "------------------- PSyData -------------------"
                write(stderr, *) "Real variable ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                write(stderr, *) "Original value: ", orig_value
                write(stderr, *) "New value:      ", value
                write(stderr, *) "------------------- PSyData -------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = chksum64
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar double precision value. It does
    !! nothing since this is not required for read-only verification.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, intent(in) :: value
    end subroutine DeclareScalarDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums). Since the value is a scalar
    !! value, the 64-bit value is just stored in the 64-bit checksum value.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumScalarDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, intent(in) :: value
        double precision             :: orig_value
        integer(kind=int64):: cksum

        ! We can use the 'cast'ed 64-bit integer values directly as checksum
        cksum = transfer(value, cksum)
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= cksum) then
                ! Convert 64 bit integer back to 64 bit double precision:
                orig_value = transfer(this%checksums(this%next_var_index), orig_value)
                write(stderr, *) "------------------- PSyData -------------------"
                write(stderr, *) "Double precision variable ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original value: ", orig_value
                write(stderr, *) "New value:      ", value
                write(stderr, *) "------------------- PSyData -------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumScalarDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a double precision field as defined in
    !! dl_esm_inf (r2d_field). It does nothing for the read-only verification.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine DeclareFieldDouble(this, name, value)
        use field_mod, only : r2d_field
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine computes a checksum for a 2d-double precision Fortran
    !! array. Each double precision value is 'cast' to a 64-integer value,
    !! and these 64-bit integer values are simply added.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    function ComputeChecksum(field) result(checksum)
        implicit none
        integer(kind=int64) :: checksum
        double precision, dimension(:,:) :: field
        integer :: i, j

        checksum = 0
        do j=1, size(field, 2)
            do i=1, size(field, 1)
                checksum = checksum + transfer(field(i,j), checksum)
            enddo
        enddo
    end function ComputeChecksum

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) of a dl_esm_field (r2d_field)
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumFieldDouble(this, name, value)
        use field_mod, only : r2d_field
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
        integer(kind=int64):: cksum

        this%next_var_index = this%next_var_index + 1

        cksum = ComputeChecksum(value%data)
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= cksum) then
                write(stderr, *) "------------------- PSyData -------------------"
                write(stderr, *) "Double precision field ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", cksum
                write(stderr, *) "------------------- PSyData -------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumFieldDouble
    
end module read_only_verify_psy_data_mod
