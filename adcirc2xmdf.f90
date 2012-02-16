program adcirc2xmdf
  use netcdf
  use xmdf

  implicit none

  integer:: argc, stat
  character(len=1000):: argv

  integer, dimension(NF90_MAX_VAR_DIMS):: dims
  integer:: n_tstep, n_node
  double precision, dimension(:,:), allocatable:: slice
  double precision, dimension(:), allocatable:: times

  ! NC_FID = Integer id value corresponding to an opened netCDF file.
  ! NC_VID = Integer id value corresponding to a variable in a netCDF file.
  integer:: NC_FID, NC_VID

  integer:: XF_FID, XF_GID, XF_VID
  ! XMDF subroutine flags require 2 byte logicals for some strange reason
  logical(kind=2):: XF_FLAG

  ! Iteration variables.
  integer:: i, j


  argc = command_argument_count()
  if (argc == 1) then
    call get_command_argument(1, argv)
  else
    stop 'Please provide the name of an input file!'
  end if

  ! Open input file
  stat = nf90_open(trim(argv), NF90_NOWRITE, NC_FID)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "Error opening input file!"
  end if


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
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "Could not recover time steps!"
  end if


  stat = nf90_inquire_variable(NC_FID, NC_VID, dimids = dims)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "Could not recover dimensions of time step vector!"
  end if

  stat = nf90_inquire_dimension(NC_FID, dims(1), len = n_tstep)
  allocate(times(n_tstep))


  ! FIXME: The timesteps need to be cast into __julian dates__!
  stat = nf90_get_var(NC_FID, NC_VID, times, (/1/), (/n_tstep/))


  ! Find sea surface elevation variable in NetCDF output.
  stat = nf90_inq_varid(NC_FID, 'zeta', NC_VID)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "Could not locate sea surface elevation!"
  end if


  ! Recover dimensions of SSE data set
  stat = nf90_inquire_variable(NC_FID, NC_VID, dimids = dims)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "Could not recover dimensions of sea surface elevation!"
  end if

  ! NOTE: NetCDF stores data in __row major__ order!
  stat = nf90_inquire_dimension(NC_FID, dims(1), len = n_node)
  stat = nf90_inquire_dimension(NC_FID, dims(2), len = n_tstep)

  write(*,*) "Number of time steps: ", n_tstep
  write(*,*) "Number of nodes: ", n_node

  allocate(slice(1,n_node))


  do i = 1, n_tstep
    stat = nf90_get_var(NC_FID, NC_VID, slice, (/1, i/), (/n_node, 1/))
    if(stat /= NF90_NOERR) then
      write(*,*) nf90_strerror(stat)
      stop "Could not extract data!"
    end if
    call xf_write_scalar_timestep(XF_VID, times(i), n_node, real(slice(1,:), 4), stat)
  end do


  stat = nf90_close(NC_FID)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "WARNING! Errors occurred while closing the netCDF file!"
  end if

  call xf_close_group(XF_GID, stat)
  call xf_close_file(XF_FID, stat)
  if ( stat < 0 ) stop 'WARNING! Errors occurred while closing the XMDF file!'

  deallocate(slice, times)
stop
end program adcirc2xmdf
