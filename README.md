adcirc2xmdf
===========

This is a simple utility for converting [ADCIRC][ADCIRC] output into [XMDF
format][XMDF].  XMDF is a file format used by [SMS][SMS] and associated models.

This work is released to the public under the terms of a [3-Clause BSD
License][BSD].


Building
--------

The `adcirc2xmdf` tool requires the [NetCDF library][NetCDF] to be installed along with
its Fortran interface. The [XMDF][XMDF] library is also required. A version of
XMDF repackaged for UNIX-like operating systems can be obtained from:

  https://github.com/Sharpie/xmdf-unix

After these dependencies have been installed, `adcirc2xmdf` may be built using
the [CMake buildsystem][CMake]:

    cd build
    cmake .. -DCMAKE_INSTALL_PREFIX=/path/to/installation/prefix
    make install


Features
--------

  * Converts global sea surface elevation (fort.61) and current (fort.62)
    output to XMDF format.


Limitations
-----------

  * ADCIRC global output must be in NetCDF format.

  * Both output files must present in the working directory from which
    `adcirc2xmdf` is run and be named `fort.61.nc` and `fort.62.nc`.

  * It is assumed that both output files share the same temporal index. That
    is, both records begin on the same date, end on the same date and contain
    observations seperated by the same time step.

  * Reference times are not set for time steps.


  [BSD]: http://www.opensource.org/licenses/BSD-3-Clause
  [ADCIRC]: http://www.adcirc.org
  [XMDF]: http://www.xmdf.org
  [SMS]: http://www.aquaveo.com/sms
  [NetCDF]: http://www.unidata.ucar.edu/software/netcdf
  [CMake]: http://www.cmake.org
