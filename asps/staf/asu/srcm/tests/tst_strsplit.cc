/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tst_strsplit.c
*:DESCRIPTION:  Test program for strsplit.c
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
#ifdef __cplusplus
#define CC_P "C"
#else
#define CC_P
#endif

int main(int argc, char **argv);
extern CC_P void usage(char* prog);

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
   int i,j;

   if (argc < 3) usage(argv[0]);

   printf("\ntst_strsplit: starting\n\n");

   printf(" delimiter = (%s)\n",argv[1]);
   for(i=2;i<argc;i++){
      printf(" string = (%s)\n",argv[i]);
      j = strsplit(argv[i],argv[1],&a);
      for(;j>0;printf("a[%d] = (%s)\n",j,a[j])){j--;}
   }

   printf("\ntst_strsplit: exiting\n\n");
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

