/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tst_strbracket.c
*:DESCRIPTION:  Test program for strbracket.c
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      08feb96-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES             --*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sutLib.h"

/*-------------------------------------------- MACROS               --*/
#ifndef NULL
#define NULL 0
#endif

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
#ifndef CC_P
#ifdef __cplusplus
#define CC_P "C"
#else
#define CC_P
#endif
#endif /*CC_P*/

//int main(int argc, char **argv);
void usage(char* prog);

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      main
*:DESCRIPTION:  program mainline
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:<---------------------------------------------------------------------
*/

int main(int argc, char **argv)
{
   char **a;
   int i,ii,j;

   if (argc < 3) usage(argv[0]);

   printf("\ntst_strbracket: starting\n\n");

   printf(" delimiter = (%s)(%s)\n",argv[1],argv[2]);
   for(i=3;i<argc;i++){
      printf(" string = (%s)\n",argv[i]);
      ii = strbracket(argv[i],argv[1],argv[2],&a);
      printf("********\n");
      for(j=0;j<ii;j++){printf("a[%d] = (%s)\n",j,a[j]);}
   }

   printf("\ntst_strbracket: exiting\n\n");
   return 1;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void usage
*:DESCRIPTION:  Shows usage of main.
*:ARGUMENTS:    char* prog      = Program name.
*:RETURN VALUE: *** KILLS PROGRAM ***
*:<---------------------------------------------------------------------
*/

void usage(char* prog)
{
   printf("Usage: %s token string ... \n",prog);
   printf("Examples: \n");
   printf("\t %s / `pwd` \n",prog);
   exit(0);
}

