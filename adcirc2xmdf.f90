! adcirc2xmdf
! ==========
! A command line tool for converting ADCIRC output to XMDF format.
!
!
! Copyright (c) 2012, Charlie Sharpsteen
! All rights reserved.
!
! Released under a 3-clause BSD license:
!   http://www.opensource.org/licenses/BSD-3-Clause

module adcirc2xmdf_globals
  implicit none
  ! XF_FID = ID value corresponding to opened XMDF file
  ! XF_GID = ID value corresponding to XMDF variable group
  integer:: XF_FID, XF_GID


end module adcirc2xmdf_globals


module adcirc2xmdf_subs
  use netcdf
  use xmdf
  use adcirc2xmdf_globals
  implicit none

contains

  subroutine netcdf_error(error_code)
    integer, intent(in):: error_code

    write(*, *) 'NetCDF Error: ', nf90_strerror(error_code)

  end subroutine netcdf_error


end module adcirc2xmdf_subs


program adcirc2xmdf
  use adcirc2xmdf_subs
  implicit none

  integer:: argc, stat
  character(len=1000):: argv

  integer, dimension(NF90_MAX_VAR_DIMS):: dims
  integer:: n_tstep, n_node
  double precision, dimension(:,:), allocatable:: slice
  double precision, dimension(:), allocatable:: times

  ! NC_FID = Integer id value corresponding to an opened netCDF file.
  ! NC_VID = Integer id value corresponding to a variable in a netCDF file.
  integer:: NC_FID, NC_VID, NC_VID2

  integer:: XF_VID, XF_VID2
  ! XMDF subroutine flags require 2 byte logicals for some strange reason
  logical(kind=2):: XF_FLAG

  ! Iteration variables.
  integer:: i


  ! Open sea surface elevation input file
  stat = nf90_open('fort.63.nc', NF90_NOWRITE, NC_FID)
  if(stat /= NF90_NOERR) call netcdf_error(stat)


  ! Open output file
  XF_FLAG = .TRUE. ! Overwrite existing files
  call xf_create_file('adcirc_output.h5', XF_FLAG, XF_FID, stat)
  if( stat < 0 ) then
    write(*,*) "XMDF Failed with status code: ", stat
    stop "Error initializing XMDF file!"
  end if

  ! Close output so that it can be re-opened in write mode.
  call xf_close_file(XF_FID, stat)

  XF_FLAG = .FALSE. ! Do not open file in read only mode
  call xf_open_file('adcirc_output.h5', XF_FLAG, XF_FID, stat)
  if( stat < 0 ) then
    write(*,*) "XMDF Failed with status code: ", stat
    stop "Could not open XMDF output for writing!"
  end if


  ! Create top-level group in XMDF file, will be named 'Datasets'
  call xf_create_generic_group(XF_FID, MULTI_DATASET_LOCATION, XF_GID, stat)

  ! Start a new dataset for sea surface elevation.
  call xf_create_scalar_dataset(XF_GID, 'Water Surface Elevation (63)', 'm', TS_SECONDS, -1, XF_VID, stat)

  ! Find sea surface elevation time steps
  stat = nf90_inq_varid(NC_FID, 'time', NC_VID)
  if(stat /= NF90_NOERR) call netcdf_error(stat)


  stat = nf90_inquire_variable(NC_FID, NC_VID, dimids = dims)
  if(stat /= NF90_NOERR) call netcdf_error(stat)

  stat = nf90_inquire_dimension(NC_FID, dims(1), len = n_tstep)
  if(stat /= NF90_NOERR) call netcdf_error(stat)

  allocate(times(n_tstep))


  ! FIXME: The timesteps need to be cast into __julian dates__!
  stat = nf90_get_var(NC_FID, NC_VID, times, (/1/), (/n_tstep/))
  if(stat /= NF90_NOERR) call netcdf_error(stat)


  ! Find sea surface elevation variable in NetCDF output.
  stat = nf90_inq_varid(NC_FID, 'zeta', NC_VID)
  if(stat /= NF90_NOERR) call netcdf_error(stat)


  ! Recover dimensions of SSE data set
  stat = nf90_inquire_variable(NC_FID, NC_VID, dimids = dims)
  if(stat /= NF90_NOERR) call netcdf_error(stat)

  ! NOTE: NetCDF stores data in __row major__ order!
  stat = nf90_inquire_dimension(NC_FID, dims(1), len = n_node)
  if(stat /= NF90_NOERR) call netcdf_error(stat)
  stat = nf90_inquire_dimension(NC_FID, dims(2), len = n_tstep)
  if(stat /= NF90_NOERR) call netcdf_error(stat)

  write(*,*) "Number of time steps: ", n_tstep
  write(*,*) "Number of nodes: ", n_node

  allocate(slice(1,n_node))


  do i = 1, n_tstep
    ! Write out a status indicater. Character 13 is a carrage return that moves
    ! the cursor back to the beginning of the line.
    write(*,'(A,A,I3,"%")', advance = 'no') char(13), " Copying Data: ", int(i*100.0/n_tstep)
    stat = nf90_get_var(NC_FID, NC_VID, slice, (/1, i/), (/n_node, 1/))
    if(stat /= NF90_NOERR) call netcdf_error(stat)
    call xf_write_scalar_timestep(XF_VID, times(i), n_node, real(slice(1,:), 4), stat)
  end do
  ! Spit out a newline
  write(*,*)


  stat = nf90_close(NC_FID)
  if(stat /= NF90_NOERR) call netcdf_error(stat)


  ! Open current input file
  stat = nf90_open('fort.64.nc', NF90_NOWRITE, NC_FID)
  if(stat /= NF90_NOERR) call netcdf_error(stat)

  call xf_create_vector_dataset(XF_GID, 'Depth-averaged Velocity (64)', 'm/s', TS_SECONDS, -1, XF_VID, stat)
  call xf_create_scalar_dataset(XF_GID, 'Depth-averaged Velocity (64) mag', 'm/s', TS_SECONDS, -1, XF_VID2, stat)

  deallocate(slice)
  allocate(slice(2, n_node))

  stat = nf90_inq_varid(NC_FID, 'u-vel', NC_VID)
  if(stat /= NF90_NOERR) call netcdf_error(stat)
  stat = nf90_inq_varid(NC_FID, 'v-vel', NC_VID2)
  if(stat /= NF90_NOERR) call netcdf_error(stat)

  write(*,*) "Number of time steps: ", n_tstep
  write(*,*) "Number of nodes: ", n_node
  do i = 1, n_tstep
    ! Write out a status indicater. Character 13 is a carrage return that moves
    ! the cursor back to the beginning of the line.
    write(*,'(A,A,I3,"%")', advance = 'no') char(13), " Copying Data: ", int(i*100.0/n_tstep)
    stat = nf90_get_var(NC_FID, NC_VID, slice(1,:), (/1, i/), (/n_node, 1/))
    stat = nf90_get_var(NC_FID, NC_VID2, slice(2,:), (/1, i/), (/n_node, 1/))
    if(stat /= NF90_NOERR) call netcdf_error(stat)
    call xf_write_vector_timestep(XF_VID, times(i), n_node, 2, real(slice, 4), stat)
    call xf_write_scalar_timestep(XF_VID2, times(i), n_node, real(sqrt(slice(1,:)**2 + slice(2,:)**2), 4), stat)
  end do
  ! Spit out a newline
  write(*,*)


  stat = nf90_close(NC_FID)
  if(stat /= NF90_NOERR) call netcdf_error(stat)

  call xf_close_group(XF_GID, stat)
  call xf_close_file(XF_FID, stat)
  if ( stat < 0 ) stop 'WARNING! Errors occurred while closing the XMDF file!'

  deallocate(slice, times)
stop
end program adcirc2xmdf
