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
#include "emlLib.h"
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
   while ((n = (char*)strntok(path,"/",i++))) {
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

   while ((c = (char*)strntok(path,"/",i++))){
      if(p)FREE(p);
      p = (char*)c;
   }
   return p;
}
#define DUI_DEPTH 128
/*
*:>---------------------------------------------------------------------
*:ROUTINE:      int dui_deleteable
*:DESCRIPTION:  prevents free()ing of non-allocated memory
*:RETURN VALUE: 7 (true) if memory is free()able, 0 otherwise
*:<---------------------------------------------------------------------
*/
int dui_deleteable(int nok,char *del[],char *testMe) {
  int ii;
  for(ii=nok-1;ii>=0;ii--) { if(testMe==del[ii]) return 7; }
  return 0;
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
   { // These curly brackets define the scope of d[].
      char *d[DUI_DEPTH],*e[DUI_DEPTH]; // directory depth of DUI_DEPTH
      char *herb;       // 20Feb98
      int i=0;		// depth counter
      int l=0;		// string length
      int herb2=-8;     // max index for FREE() {some of the d[i]'s are
                        // dynamically allocated, and some are not}
      while ((d[i]=strntok(base,"/",i))){	// load absolute base
         l += strlen(d[i]); 
         e[i]=d[i]; herb2=i+1;
         i++;
      }
      herb=(char*)MALLOC(strlen(mod)+1); if(!herb) return NULL;
      strcpy(herb,mod);   // so we can strtok (mod is const char*)
      d[i]=strtok(herb,"/");
      for(;;) {
         if(!d[i]) break;
         if( 0 == strcmp("..",d[i]) ){
            if(dui_deleteable(herb2,e,d[i])) FREE(d[i]);
            l -= strlen(d[--i]);
            if(dui_deleteable(herb2,e,d[i])) FREE(d[i]);
         }
         else if( 0 == strcmp(".",d[i]) ){
            if(dui_deleteable(herb2,e,d[i])) FREE(d[i]);
         }
         else {
            l += strlen(d[i]);
            i++;
         }
         d[i]=strtok(NULL,"/");
      }
      char* ap = (char*)MALLOC(l+i+1);
      memset(ap,0,l+i+1);		//ap[0] = '\000';
      for( int k=0;k<i;k++ ){
         strcat(ap,"/");
         strcat(ap,d[k]);
         if(dui_deleteable(herb2,e,d[k])) FREE(d[k]);
      }
      FREE(herb);
      return ap;
      break;
   } //  These curly brackets define the scope of d[].
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
   int i=1;   // hjw 14 June 1998:  We need to start from 1, not 0.
   bool_t isDataset;

   while( (s = strntok(path,"/",i++)) != NULL ){
     // hjw 14 June 1998:  I removed an if() here.  The non-pernicious
     // functionality of the if() is taken over by the new initialization
     // for i above (1 instead of 0).  The pernicious functionality caused
     // the cd recursion bug reported in Craig's email to me of 12 Jun 1998.
     if( !dsIsDataset(&isDataset,pDSr)
	   ||  !isDataset
	   ||  !dsFindEntry(&pDSc, pDSr, s)
	   ){
	 if(s) FREE(s);  /*fix memory leak -akio/phenix*/
	 EML_PUSHERROR(DSL_NODE_NOT_FOUND);
	 return FALSE;
     }
     pDSr = pDSc;
     if(s) FREE(s); /*fix memory leak -akio*/
   }
   node = pDSr;
   return TRUE;
}

