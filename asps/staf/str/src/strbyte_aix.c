/*  General Description of this package:

	Filename: strbyte_sgi.c   (completely self-contained, except for system header
	                    stdio.h)


	This package performs Fortran-callable byte-stream file i/o operations,
	not otherwise directly available from Fortran, be it VMS or UNIX.

	This file originated with D. Weygand, ca. 1989, for the purpose of
	reading MPS data-tapes, which were written from a VAX 750 during AGS
	experiment 818, into files of a form that could be transferred over to
	UNIX platforms and read in there by an analysis program.  A few small
	modifications were added in November, 1992, by R. Hackenburg, for
	essentially the same purpose, for AGS experiment 881.

	The procedures in this file work on both ends of the data-transfer process,
	ie, the file-writing routines run on VMS and write files from tapes, while
	the file-reading routines run on UNIX and read those same files, after having
	been shipped over the network.  All procedures in this file are both VMS and
	UNIX compatible.

	The byte order IS corrected in this package.

	This package contains calls to the strlib(VMS)/libstr(UNIX) and to the
	msglib(VMS)/libmsg(UNIX) Fortran libraries.


	This package has been incorporated into the general utility package strlib(VMS)
	or libstr(UNIX) on November 20, 1992 (R. Hackenburg)
*/

strfc()
{
	return;
}
