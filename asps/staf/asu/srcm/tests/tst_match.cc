/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tst_match.cc
*:DESCRIPTION:  Test of match.c
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      02may96-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- MACROS               --*/
/*-------------------------------------------- INCLUDES             --*/
#include <stdio.h>
#include <stdlib.h>

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
void usage(char* prog);
extern "C" int sutMatchReg(char* patt, char* string);
extern "C" int sutMatchWild(char* patt, char* string);

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
   int i;
   if (argc < 3) usage(argv[0]);
   printf("tst_match: starting\n");
   for(i=2;i<argc;i++){
      printf("(%s) (%s)\t\t",argv[1],argv[i]);
      if( sutMatchWild(argv[1],argv[i]) ){
	 printf("MATCH\n");
      }
      else {
	 printf("\n");
      }
   }
   printf("tst_match: exiting\n");
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
   printf("Usage: %s pattern string [...] \n",prog);
   exit(0);
}

