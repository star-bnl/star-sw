/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         ahsLib.c
*:DESCRIPTION:  Parsing and unparsing functions for AHSs
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      10may96-v000b-cet- 
*:HISTORY:      03may96-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- MACROS               --*/
#define _PRINTF printf("%s.%d-",__FILE__,__LINE__);fflush(0);printf

/*-------------------------------------------- INCLUDES             --*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "asuAlloc.h"
#include "ahsLib.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:
*:DESCRIPTION:
*:ARGUMENTS:
*:RETURN VALUE:
*:<---------------------------------------------------------------------
*/

/*--------------------------------------------------------------------*/
int isValidAhsSpec(char *s){ if(s)return TRUE;else return FALSE; }
/*--------------------------------------------------------------------*/
int isValidShapeSpec(char *s){ if(s)return TRUE;else return FALSE; }
/*--------------------------------------------------------------------*/
int ahs_parseSpec(char* spec, AHS_STRUCT_T* a)
{

   ahs_zeroAHS(*a);

   char *locationSpec, *pathSpec, *qualifierSpec, *entrySpec;
/*-(" Split specification into sub specs. \n");-*/
   ahs_specSplit(spec,locationSpec,pathSpec,qualifierSpec,entrySpec);

/*# DEBUG
printf("                     locationSpec = %s\n",locationSpec);
printf("                         pathSpec = %s\n",pathSpec);
printf("                    qualifierSpec = %s\n",qualifierSpec);
printf("                        entrySpec = %s\n",entrySpec);
fflush(0);
DEBUG #*/

/*-(" Load the AHS location and qualifier. \n");-*/
   a->location = locationSpec; locationSpec = NULL;
   a->qualifier = qualifierSpec; qualifierSpec = NULL;
/*$ DEBUG $*/
   a->entryName = entrySpec;
/*$ DEBUG $*/

/*-(" Split path into nodes. \n");-*/
   if(pathSpec)
      a->pathDepth = ahs_spec2nodes(pathSpec,a->nodes);

/*-(" Parse the entry. \n");-*/
   if(entrySpec)
      ahs_spec2nameShape(entrySpec ,a->entryName,a->entryShape);

   return TRUE;
}

/*--------------------------------------------------------------------*/
int ahs_zeroAHS(AHS_STRUCT_T& a)
{
/*-(" Zero-out the AHS. \n");-*/
   a.location = NULL;
   a.pathDepth = 0;
   a.nodes = NULL;
   a.qualifier = NULL;
   a.entryName = NULL;
   a.entryShape.rank = 0;
   a.entryShape.indexes = NULL;

   return TRUE;
}

/*--------------------------------------------------------------------*/
int ahs_printAHS(AHS_STRUCT_T a)
{
   int i,j;

   printf(
   "====================================\n"
   "   LOCATION = (%s)\n"
   " PATH DEPTH = (%d)\n"
   		,a.location
   		,a.pathDepth); fflush(0);
   for(i=0;i<a.pathDepth;i++){
   printf(
   "    NODE[%d] = (%s) \tRANK = %d SHAPE = ["
		,i
		,a.nodes[i].name
		,a.nodes[i].shape.rank); fflush(0);
   for(j=0;j<a.nodes[i].shape.rank;j++){
   if(j>0)printf(", "); fflush(0);
   printf("%d",a.nodes[i].shape.indexes[j]); fflush(0);
   }
   printf("]\n"); fflush(0);
   }
   printf(
   "  QUALIFIER = (%s)\n"
   "      ENTRY = (%s) \tRANK = %d SHAPE = ["
	,a.qualifier
	,a.entryName
	,a.entryShape.rank); fflush(0);
   for(j=0;j<a.entryShape.rank;j++){
   if(j>0)printf(", "); fflush(0);
   printf("%d",a.entryShape.indexes[j]); fflush(0);
   }
   printf("]\n"); fflush(0);
   return TRUE;
}

/*--------------------------------------------------------------------*/
int ahs_specSplit(const char *s
                , char*& loc, char*& path, char*& qual, char*& entr)
{
   char *spec;
   char *c, *cc;
   char **buffs; int nbuffs;
   int nspec = 0;

   spec = (char*)MALLOC(strlen(s) +1);
   strcpy(spec,s);

/*-(" Is there a location in the specification? \n");-*/
   if( (cc=strchr(spec,':')) ){      /* first ':' */
      nspec++;
      nbuffs = strsplit(spec,":",&buffs);
      if(nbuffs != 2){ _PRINTF("syntax error\n"); return FALSE; }
      loc = buffs[0]; buffs[0] = NULL;
      FREE(spec);
      spec = buffs[1]; buffs[1] = NULL;
      FREE(buffs);
   }
   else {
      loc = NULL;
   }

/*-(" Is there an entry in the specification? \n");-*/
   if( (cc=strrchr(spec,'.')) ){     /* last '.' */
      if( (0 == strncmp(cc,"./",2))
      ||  (0 == strncmp(cc,"../",3))
      ||  (0 == strncmp(cc-1,"/.",2))
      ||  (0 == strncmp(cc-2,"/..",3))
      ){
	 entr = NULL;
      }
      else {
	 nspec++;
	 entr = (char*)MALLOC(strlen(spec) - (cc+1-spec) +1);
	 strncpy(entr,cc+1,strlen(spec) - (cc+1-spec)); 
	 c = (char*)MALLOC((cc-spec) +1);
	 strncpy(c,spec,(cc-spec)); 
	 c[(cc-spec)]=0; /* hjw 19Feb98 */
	 FREE(spec);
	 spec = c; c = NULL;
      }
   }
   else {
      entr = NULL;
   }

/*-(" Is there a qualifier in the specification? \n");-*/
   if( (c=strchr(spec,'(')) && (cc=strrchr(spec,')')) ){
      nspec++;
      nbuffs = strsplit(spec,"()",&buffs);
      if(nbuffs != 2){ _PRINTF("syntax error\n"); return FALSE; }
      FREE(spec);
      spec = buffs[0]; buffs[0] = NULL;
      qual = buffs[1]; buffs[1] = NULL;
      FREE(buffs);
   }
   else {
      qual = NULL;
   }

/*-(" Is there a path in the specification? \n");-*/
   if( TRUE ){
      path = spec; spec = NULL;
      nspec++;
   }
   else {
      path = NULL;
   }

   if(spec)FREE(spec);
   return nspec;
}

/*--------------------------------------------------------------------*/
int ahs_spec2shape(const char *spec, SHAPE_T& shape)
{
   int isGood = TRUE;
   char **buffs; int nbuffs;
   int i;

   if( !isValidShapeSpec((char*)spec) ){
      shape.rank = 0;
      shape.indexes = NULL;
      return FALSE;
   }

/*-(" Parse the shape specificatoin. \n");-*/
   nbuffs = strsplit(spec,"[,]",&buffs);
   if(nbuffs < 1){
      shape.rank = 0;
      shape.indexes = NULL;
      return FALSE;
   }

   shape.rank = nbuffs;
   shape.indexes = (int*)MALLOC(nbuffs*sizeof(int*));

   for( i=0;i<nbuffs;i++ ){
      if( isInteger(buffs[i]) ){
	 shape.indexes[i] = atoi(buffs[i]);
      }
      else {
	 shape.indexes[i] = -11301957; /*-AHS_E_NO_INDEX-*/
	 isGood = FALSE;
      }
   }
   return isGood;
}

/*--------------------------------------------------------------------*/
int ahs_spec2nodes(const char *spec, AHSNODE_T*& nodes)
{
   int depth;
   char **buffs; int nbuffs;
   char *cc;
   int i;

/*-(" Split path into nodes. \n");-*/
   if( (cc=strchr(spec,'/')) ){
      nbuffs = strsplit(spec,"/",&buffs); /* delimiter = "/" */
      depth = nbuffs;
   }
   else {
      depth = 1;
      buffs = (char**)MALLOC(1*sizeof(char**));
      buffs[0] = (char*)MALLOC(strlen(spec) +1);
      strcpy(buffs[0],spec);
   }

/*-(" Create nodes array. \n");-*/
   if( NULL == nodes ){
      nodes = (AHSNODE_T*)MALLOC(depth*sizeof(AHSNODE_T*));
   }

/*-(" Load the AHS nodes. \n");-*/
   for(i=0;i<depth;i++){
      ahs_spec2nameShape(buffs[i],nodes[i].name,nodes[i].shape);
      FREE(buffs[i]);
   }
   FREE(buffs);

   return depth;
}

/*--------------------------------------------------------------------*/
int ahs_spec2nameShape(const char *spec, char*& name, SHAPE_T& shape)
{
   char **buffs; int nbuffs;

   nbuffs = strsplit(spec,"[]",&buffs);
   name = buffs[0]; buffs[0] = NULL;
   if( nbuffs == 2 ){
      ahs_spec2shape(buffs[1],shape);
      FREE(buffs[1]);
   }
   else {
      shape.rank = 0;
      shape.indexes = NULL;
   }
   FREE(buffs);
   return TRUE;
}
/*--------------------------------------------------------------------*/
