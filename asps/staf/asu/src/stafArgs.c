/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         stafArgs.c
*:DESCRIPTION:  Handle Unix command line arguments to STAF.
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      17jun96-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- MACROS               --*/
/*-------------------------------------------- INCLUDES             --*/
#include <stdio.h>
#include <stdlib.h>

/*-------------------------------------------- TYPEDEFS             --*/
#define TRUE 1
#define FALSE 0
#define MAX_ARGS 100
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
int stafArgs(int argc, char **argv);
void usage(char* prog);

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      stafArgs
*:DESCRIPTION:  Store, interpret, & implement Unix command line
*:			arguments.
*:ARGUMENTS:    Same as main.
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/

int stafArgs(int argc, char **argv)
{
   int i=0;

   static int nargs=0;
/*   static char **argvs; */
   static char* argvs[MAX_ARGS];  /*fix write bad index -akio*/

   int none=0,error=0;
   int help=0,shared=0,paw=0,exec=0;

/*- Store arguments. -*/
   nargs = argc; 
   if(nargs > MAX_ARGS){     /*fix write bad index -akio*/
     puts("stafArgs: Too mant arguments. Ignore rest");
     nargs=MAX_ARGS;
   }
/*   argvs = (char**)malloc(nargs); */
   for( i=0;i<nargs;i++ ){
     argvs[i] = (char*)malloc(strlen(argv[i]) +1);
     strcpy(argvs[i],argv[i]);
   }

/*- Interpret arguments. -*/
   for( i=1;i<nargs;i++ ){
        if( 0 == strcmp("-",argvs[i]) ){
	   none = i; /*printf("No argument\n");*/
        }
        else if( 0 == strcmp("-help",argvs[i]) ){
	   help = i; /*printf("HELP argument\n");*/
        }
        else if( 0 == strcmp("-exec",argvs[i]) ){
	   exec = i; /*printf("EXECUTE argument\n");*/
        }
        else if( 0 == strcmp("-paw",argvs[i]) ){
	   paw = i; /*printf("PAW argument\n");*/
        }
        else if( 0 == strcmp("-shared",argvs[i]) ){
	   shared = i; /*printf("SHARED argument\n");*/
        }
        else {
	   error = i; /*printf("Unrecognized argument\n");*/
        }
   }

/*- Implement arguments. -*/
   if( help ){
      printf("HELP option\n");
   }

   if( shared ){
      printf("SHARED option\n");
   }

   if( paw ){
      printf("PAW option\n");
   }

   if( exec ){
      i = exec +1;
      while( '-' != argvs[i][0] ){
         printf("EXECUTE %s\n",argvs[i++]);
      }
   }

   return TRUE;
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
   printf("Usage: %s \n",prog);
   exit(0);
}

