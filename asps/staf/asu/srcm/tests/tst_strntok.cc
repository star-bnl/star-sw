/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tst_strntok.c
*:DESCRIPTION:  Test program for strntok.c
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      08feb96-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES             --*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*-------------------------------------------- MACROS               --*/
#ifndef NULL
#define NULL 0
#endif

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
#ifdef __cplusplus
#define CC_P "C"
#else
#define CC_P
#endif

int main(int argc, char **argv);
extern CC_P void usage(char* prog);
#include "sutLib.h"

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
   int j;
   int i=0;
   char *c;
   char *s=argv[1];
   char *t=argv[2];

   if (argc != 3) usage(argv[0]);

   printf("\ntst_strntok: starting\n\n");

   printf("( %s %s %s )\n",argv[0],s,t);

   for( j=0;j<3;j++ ){
      printf("----------------------------------------\n");
      printf("( %s %s )\n",s,t);
      i=0;
      while( c=(char*)strntok(s,t,i) ){
	 printf("#%d(%s)",i++,c);
      }
      printf("\n");
   }

   printf("\ntst_strntok: exiting\n\n");
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
   printf("Usage: %s string token \n",prog);
   printf("Examples: \n");
   printf("\t %s `pwd` / \n",prog);
   exit(0);
}

