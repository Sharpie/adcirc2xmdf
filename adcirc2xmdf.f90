program adcirc2xmdf
  use netcdf
  implicit none

  integer:: argc, stat
  character(len=50):: argv

  integer, dimension(NF90_MAX_VAR_DIMS):: dims
  integer:: n_tstep, n_node
  double precision, dimension(:,:), allocatable:: slice

  ! NC_FID = Integer id value corresponding to an opened netCDF file.
  ! NC_VID = Integer id value corresponding to a variable in a netCDF file.
  integer:: NC_FID, NC_VID

  ! Iteration variables.
  integer:: i, j


  argc = command_argument_count()
  if (argc == 1) then
    call get_command_argument(1, argv)
    open(15, file = argv, iostat = stat)
  else
    stop 'Please provide the name of an input file!'
  end if

  stat = nf90_open(argv, NF90_NOWRITE, NC_FID)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "Error opening input file!"
  end if


  ! Find sea surface elevation variable.
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


  do i = 1, 10
    stat = nf90_get_var(NC_FID, NC_VID, slice, (/1, i/), (/10, 1/))
    if(stat /= NF90_NOERR) then
      write(*,*) nf90_strerror(stat)
      stop "Could not extract data!"
    end if
    write(*,"(10F8.4)") (slice(1,j), j = 1, 10)
  end do


  stat = nf90_close(NC_FID)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "WARNING! Errors occurred while closing the netCDF file!"
  end if

  deallocate(slice)
stop
end program adcirc2xmdf
