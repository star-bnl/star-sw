/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:		translate_xdf2rzd.c
*:DESCRIPTION:	Converts a XDF file to a RZD file.
*:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:		18may95-v000a- Only works on flat datasets.
*:BUGS:		-- STILL IN DEVELOPMENT --
*:HISTORY:	12jun95-v001a-cet- seperate main & usage
*:HISTORY:	18may95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

#include <stdio.h>
#include "dstype.h"
#include "dsxdr.h"
#include "dsuType.h"
#include "cfortran.h"
#include "hbook.h"

/*----------------------------------
*  prototypes
*/
void usage(char* prog);

/*----------------------------------
*  PAWC array
*/
#define PAWC_SIZE 5000000
struct pawc {int h[PAWC_SIZE];} pawc_;

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	main
*:DESCRIPTION:	Program mainline.
*:ARGUMENTS:	(int argc, char **argv)
*:RETURN VALUE:	-- NONE --
*:<---------------------------------------------------------------------
*/

main(int argc, char **argv)
{
   int i,status;

   if (argc < 2) usage(argv[0]);

/*- Setup HBOOK. -*/
   HLIMIT(PAWC_SIZE);

   for (i=1;i<argc;i++) {
      status = xdf2rzd(20+i,argv[i]);
   }
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	void usage
*:DESCRIPTION:	Shows usage of main.
*:ARGUMENTS:	char* prog	= Program name.
*:RETURN VALUE:	*** KILLS PROGRAM ***
*:<---------------------------------------------------------------------
*/

void usage(char* prog)
{
   printf("\nUsage: %s file<s>\n\n",prog);
   exit(0);
}

