program adcirc2xmdf
  use netcdf
  implicit none

  integer:: argc, stat
  character(len=50):: argv

  double precision, dimension(10):: slice

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


  stat = nf90_inq_varid(NC_FID, 'zeta', NC_VID)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "Could not locate sea surface elevation!"
  end if


  do i = 1, 10
    stat = nf90_get_var(NC_FID, NC_VID, slice, (/i, 1/), (/1, 10/))
    if(stat /= NF90_NOERR) then
      write(*,*) nf90_strerror(stat)
      stop "Could not extract data!"
    end if
    write(*,"(10F6.2)") (slice(j), j = 1, 10)
  end do


  stat = nf90_close(NC_FID)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "WARNING! Errors occurred while closing the netCDF file!"
  end if

stop
end program adcirc2xmdf
