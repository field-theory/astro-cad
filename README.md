AstroCAD - Celestial mechanics [![development status](http://img.shields.io/badge/status-final--release-green.svg)](http://www.field-theory.org)
==============================

Executive Summary
-----------------

This is a collection of programs, functions and procedures written in Turbo Pascal for the numerical simulation of celestial mechanics in our solar system. It was my contribution to the Jugend forscht contest in 1992 and it ranked first place at the regional level and second place at the state level.

The license is GPLv2 (cf. the included file `LICENSE`).

**NOTE** This project is complete and will likely not be updated anymore!

Programming language and requirements
-------------------------------------

The original program has been written in Turbo Pascal. The units and functions are documented in German, but severals names of functions, procedures and variables are in English. There is some redundancy in parts of the programs since I have been experimenting with different ways to solve the differential equations.

In order to back up the files and results I have uploaded them to github. I have identified those parts that do not depend on Turbo Pascal for DOS and which can today be compiled with the Free Pascal Compiler. I found there is only one file that will not compile with Free Pascal -- this requires Turbo Pascal 7 for DOS which can still be run today using emulators like DOSbox. If there is a way to use the graph and crt units that are part of Turbo Pascal 7 with contemporary compilers then you can use these without any restrictions.

Running the project
-------------------

### Logging astronomical events

The main driver is the file `astbase`. Compilation of astbase.pas using Free Pascal is straightforward:

    fpc -Mtp -O3 astbase.pas

This will compile all dependencies and generate the binary `astbase` which can be launched at the command line via:

    ./astbase.pas

The program will read in the initial positions on January 1st, 1991, and then generate the report for the following 600 days. The report is based on a simulation run and contains a log of key events relevant to amateur astronomers in the solar system. It is interesting to see for how long the prediction remains accurate. These days such program runs take seconds. Back then (in 1992) such a run took hours or even days.

The names of the input- and output-files are hardcoded in `astbase.pas`.

### Swing-by maneuver

Compiling and running `SWING-BY.PAS` in the `TP_DOS` directory is best done in the Turbo Pascal IDE. One may need to adapt the path to the `BGI` directory (which contains Borland's graphics drivers) in line 146 from `C:\TP7\BGI` to whatever directory Turbo Pascal is installed in. I have set the line to the Turbo Pascal 7 directory in my DOSBox emulator, but other people may need a different path here.

Included files
--------------

+ `vectors.pas`: A Pascal unit (the Pascal equivalent of a library file) containing elementary vector operations on three-component vectors.
+ `astconv.pas`: The main unit responsible for astronomy-related calculations. It has quite large functionality, although I did not use all of it; I collected everything I knew about in Pascal-style functions and procedures and then referred to those parts I needed in the main driver programs.
+ `astverw.pas`: This is a higher-level unit building on `vectors` and `astconv`. It implements most of the functionality needed for the end-user. The feature I am particularly proud of is the "phenomenon" detector (implemented in the procedure "SolarPheno") which finds astronomical events during the molecular dynamics evolution.
+ `SOLAR.TSS`, `SOLARST.TSS`: The input files for the solar system on January 1st, 1991, 00:00 UT. These files were created by the `ACADENT.PAS` program below. I got the input data from the "Astronomical Almanac".
+ `astbase.pas`: A driver program which uses the "phenomenon" detector in `astverw` to find astronomical events based on the input data from `SOLARST.TSS`. The output is written to stdout. Together with `SWING-BY.PAS` below, this was the main accomplishment of my Jugend Forscht research project.
+ `JuFo.pdf`: The article I have submitted to the Jugend forscht contest. I describes the program and the results in more detail. There are three figures which I directly printed from screen back then and which I do not have anymore. The article is in German. Furthermore, there is a physics mistake in the document which is NOT present in the program. Can you find it?

The following file requires Turbo Pascal for DOS:

+ `TP_DOS/SWING-BY.PAS`: The other main pillar of my project. It demonstrates a real-life swing-by manouver of a spaceprobe along Jupiter. The reason it requires Turbo Pascal is the use of the graph and crt units.

The following are support files and/or tests. I have removed some of the minor ones.

+ `Support/holzeit.pas`: A test of time conversion.
+ `Support/RKFEHLB.MAT`, `Support/RUNGEKT.MAT`: Generic input files encoding the parameters of the Runge-Kutta method for solving differential equations. I guess I never used anything else than fourth order (the "classical Runge Kutta method"), but there was generic support for higher orders.
+ `Support/crmatrix.pas`: Generates the Runge-Kutta input files `*.MAT` above.
+ `Support/acadent.pas`: This is the most important of the support files. It generates the solar system input files like `SOLARST.TSS` (see above). Since I only had the Astronomical Almanac and the celestial position data was converted to my own coordinate system, I had to compute the velocities on my own. This is accomplished in the procedure `GetSpeed` in the `astconv` unit. This program provides the necessary input to `GetSpeed`.

I have not included the part which draws the solar system and the orbits of the planets. This part is quite messy and requires some libraries from other people whose copyright situation I do not understand.

