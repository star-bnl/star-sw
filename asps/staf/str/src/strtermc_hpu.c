/*

	         This file is:  strtermc_sun.c

	              Version:  1.00

	This code provides some common special-purpose terminal application routines,
such as synchronous terminal I/O (I/O waiting or blocking), asynchronous terminal I/O
(no I/O waiting or nonblocking I/O), record terminal I/O (line I/O or nonpunctual I/O)
and character terminal I/O (byte I/O or puntual I/O).

	The code in this file is platform dependent, written with the SGI c compiler,
invoked with the cc command, but should work on all Unix platforms.  The equivalent
VMS code is done in FORTRAN, in STRTERM_VMS.FOR.

	This code, in its original form, is a melange of both original and borrowed
code;  the borrowed code (not easily recognizable) is from "Advanced C Programming",
by ____,chapter 4, "Terminal I/O".

	Some procedures, or subroutines, defined in this file are intended for local
use only, by other c-routines in this same file, and are not a part of the general
STR package, and are not FORTRAN-callable.

	The FORTRAN-callable routines defined here are equivalent to those defined in
STRTERM_VMS.FOR on VMS platforms.

	Note that the order of function definitions in this file is important on
some platforms, such as the DECstation 5000 running ULTRIX;  alphabetical order
doesn't work.

	FORTRAN-callable routines ("void" ==> no argument or no return value)
*/
