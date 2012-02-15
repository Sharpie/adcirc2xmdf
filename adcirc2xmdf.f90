program adcirc2xmdf
  use netcdf
  implicit none

  integer:: argc, stat
  character(len=50):: argv

  ! Integer id value corresponding to an opened netCDF file.
  integer NC_FID


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


  write(*,*) "The file id is: ", NC_FID


  stat = nf90_close(NC_FID)
  if(stat /= NF90_NOERR) then
    write(*,*) nf90_strerror(stat)
    stop "WARNING! Errors occurred while closing the netCDF file!"
  end if

stop
end program adcirc2xmdf
