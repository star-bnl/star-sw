/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dui_utils.C
*:DESCRIPTION:  Functions for parsing and handling path names.
*:DESCRIPTION:  To compile as stand-alone test program:
*:DESCRIPTION:  	CC -DTEST dui_path.C -o dui_path
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      31jan96-v001b-cet- mv dui_path.C dui_utils.C
*:HISTORY:      22jan96-v001a-cet- remove TEST code
*:HISTORY:      20nov95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES             --*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "asuAlloc.h"
#include "dui_types.h"

/*-------------------------------------------- MACROS               --*/
/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
#include "sutLib.h"

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      char * rootof
*:DESCRIPTION:  Find root of an absolute path
*:ARGUMENTS:    char * path	= path string (absolute)
*:RETURN VALUE: char * root	= root of path arg
*:<---------------------------------------------------------------------
*/
char * dui_rootof(const char* path)
{
   return (char*)strntok(path,"/",0);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      char * dirof
*:DESCRIPTION:  Find directory part of path
*:ARGUMENTS:    char * path	= path string (absolute)
*:RETURN VALUE: char * 		= directory part of path
*:<---------------------------------------------------------------------
*/
char * dui_dirof(const char* path)
{
   char *d=NULL,*n,*p;
   int i=0;

   d=(char*)MALLOC(strlen(path)+1);
   d[0] = '\000';
   p=NULL;
   while( n = (char*)strntok(path,"/",i++) ){
      if(p){
         strcat(d,"/");
         strcat(d,p);
         FREE(p);
      }
      p=(char*)n;
   }
   if(p)FREE(p);
   return d;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      char * notdirof
*:DESCRIPTION:  Find non-directory part of path
*:ARGUMENTS:    char * path	= path string (absolute)
*:RETURN VALUE: char * 		= non-directory part of path
*:<---------------------------------------------------------------------
*/

char * dui_notdirof(const char * path)
{
   char * c=NULL;
   char * p=NULL;
   int i=0;

   while( c = (char*)strntok(path,"/",i++) ){
      if(p)FREE(p);
      p = (char*)c;
   }
   return p;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      char * pathof
*:DESCRIPTION:  Construct absolute path from abs. base & rel. mod
*:ARGUMENTS:    char * base	= base path string (absolute)
*				= must be non-NULL
*:ARGUMENTS:    char * mod	= modifier path string (relative)
*:RETURN VALUE: char *	 	= parsed absolute path
*:<---------------------------------------------------------------------
*/
char * dui_pathof(const char* base,const char* mod)
{
   char *p=NULL;
   if( NULL == strchr("/",base[0]) ){
      return NULL;	// base must be absolute path.
   }
   if( mod == NULL ){
      p = (char*)MALLOC(strlen(base)+1);
      strcpy(p,base);
      return p;		// bad mod
   }
   switch (mod[0]) {
   case '/':		// mod looks like an absolute path
      p = (char*)MALLOC(strlen(mod)+1);
      strcpy(p,mod);
      return p;
      break;
   default:		// mod looks like a relative path
   { //-for d[] scope
      char *d[128];	// max directory depth of 128
      int i=0;		// depth counter
      int l=0;		// string length
      while( d[i]=strntok(base,"/",i) ){	// load absolute base
         l += strlen(d[i]);
         i++;
      }
      int j=0;
      while( d[i]=strntok(mod,"/",j++) ){	// add relative mod
         if( 0 == strcmp("..",d[i]) ){
            FREE(d[i--]);
            l -= strlen(d[i]);
            FREE(d[i]);
         }
         else if( 0 == strcmp(".",d[i]) ){
            FREE(d[i]);
         }
         else {
            l += strlen(d[i]);
            i++;
         }
      }
      char* ap = (char*)MALLOC(l+i+1);
      memset(ap,0,l+i+1);		//ap[0] = '\000';
      for( int k=0;k<i;k++ ){
         strcat(ap,"/");
         strcat(ap,d[k]);
         FREE(d[k]);
      }
      return ap;
      break;
   } //-for d[] scope
   }
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      int duiFindDS
*:DESCRIPTION:  Recursive search for DSL node within hierarchy.
*:DESCRIPTION:	** Should be a dsl function? **
*:ARGUMENTS:    DS_DATASET_T&* node	= dataset found (or NULL)
*:ARGUMENTS:    DS_DATASET_T* root	= root dataset
*:ARGUMENTS:    char* path		= Unix-like path
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/
int duiFindDS(DS_DATASET_T *& node, DS_DATASET_T* root, char* path)
{
   char *s;
   DS_DATASET_T *pDSr=root;
   DS_DATASET_T *pDSc=NULL;
   int i=0;
   char *name;
   bool_t isDataset;

//printf("path = %s \n",path);			.. ***** DEBUG *****
   while( (s = strntok(path,"/",i++)) != NULL ){
//printf("elem = %s \t",s);			.. ***** DEBUG *****
//printf("base = %s \n",pDSr->name);		.. ***** DEBUG *****
   if( 0 != strcmp(s,pDSr->name)){ 	// ***** HACK *****
      if( !dsIsDataset(&isDataset,pDSr)
      ||  !isDataset
      ||  !dsFindEntry(&pDSc, pDSr, s)
      ){
	 dsPerror("can't find DSL node");
//printf("elem = %s ??? \t",s);			.. ***** DEBUG *****
//printf("base = %s ??? \n",pDSr->name);	.. ***** DEBUG *****
	 FREE(s);
	 return FALSE;
      }
      FREE(s);
      pDSr = pDSc;
   }
   }
   node = pDSr;
   return TRUE;
}

