/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         spx_kam.c
*:DESCRIPTION:  C KUIP Action Modules for Error & Message Logger
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      26jul95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "asuAlloc.h"
#include "asuLib.h"
#define KUIP
#include "emlLib.h"
#include "spxLib.h"

#include "kuip.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spxdummy_ncalls_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/DUMMY/NCALLS NAME
*:<---------------------------------------------------------------------
*/
void kam_spxdummy_ncalls_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */

        STAFCV_T status = spxdummy_ncalls(name);
}
STAFCV_T spxdummy_ncalls(char* name)
{
   spxDummy* dummy;		/* spxDummy object */

   if( !spx->findDummy(name, dummy) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("SPXDUMMY:\tNumber of calls = %d \n",dummy->nCalls());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spxdummy_null_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/DUMMY/NULL NAME
*:<---------------------------------------------------------------------
*/
void kam_spxdummy_null_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */

        STAFCV_T status = spxdummy_null(name);
}
STAFCV_T spxdummy_null(char* name)
{
   spxDummy* dummy;		/* spxDummy object */

   if( !spx->findDummy(name, dummy) 
   ||  !dummy->null() ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spxdummy_time_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/DUMMY/TIME NAME
*:<---------------------------------------------------------------------
*/
void kam_spxdummy_time_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */

        STAFCV_T status = spxdummy_time(name);
}
STAFCV_T spxdummy_time(char* name)
{
   spxDummy* dummy;		/* spxDummy object */

   if( !spx->findDummy(name, dummy) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   char* tim;
   if( !dummy->getTime(tim) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   printf("SPXDUMMY:\tTime = %s \n",tim);
   FREE(tim);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spxgrid_height_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/GRID/HEIGHT NAME
*:<---------------------------------------------------------------------
*/
void kam_spxgrid_height_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */

        STAFCV_T status = spxgrid_height(name);
}
STAFCV_T spxgrid_height(char* name)
{
   spxGrid* grid;		/* spxGrid object */

   if( !spx->findGrid(name, grid) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("SPXGRID:\tHeight = %d \n",grid->height());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spxgrid_width_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/GRID/WIDTH NAME
*:<---------------------------------------------------------------------
*/
void kam_spxgrid_width_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */

        STAFCV_T status = spxgrid_width(name);
}
STAFCV_T spxgrid_width(char* name)
{
   spxGrid* grid;		/* spxGrid object */

   if( !spx->findGrid(name, grid) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("SPXGRID:\tWidth = %d \n",grid->width());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spxgrid_get_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/GRID/GET NAME M N
*:<---------------------------------------------------------------------
*/
void kam_spxgrid_get_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */
   short m = ku_geti();		/* first index */
   short n = ku_geti();		/* second index */

        STAFCV_T status = spxgrid_get(name,m,n);
}
STAFCV_T spxgrid_get(char* name,short m,short n)
{
   spxGrid* grid;		/* spxGrid object */

   if( !spx->findGrid(name, grid) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   long value;
   if( !grid->get(m,n,value) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   printf("SPXGRID:\tCell value = %d \n",value);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spxgrid_set_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/GRID/SET NAME M N VALUE
*:<---------------------------------------------------------------------
*/
void kam_spxgrid_set_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */
   short m = ku_geti();		/* first index */
   short n = ku_geti();		/* second index */
   long value = ku_geti();	/* new grid value */

        STAFCV_T status = spxgrid_set(name,m,n,value);
}
STAFCV_T spxgrid_set(char* name, short m, short n, long value)
{
   spxGrid* grid;		/* spxGrid object */

   if( !spx->findGrid(name, grid) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   if( !grid->set(m,n,value) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spx_count_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/COUNT
*:<---------------------------------------------------------------------
*/
void kam_spx_count_()
{
   long npars = ku_npar();	/* number of KUIP parameters */

        STAFCV_T status = spx_count();
}
STAFCV_T spx_count()
{
   printf("SPX:\tObject count = %d \n",spx->count());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spx_list_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/LIST
*:<---------------------------------------------------------------------
*/
void kam_spx_list_()
{
   long npars = ku_npar();	/* number of KUIP parameters */

        STAFCV_T status = spx_list();
}
STAFCV_T spx_list()
{
   char *herb980615;
   char *c=NULL;
   c=spx->list();
   herb980615=strtok(c,"\n");
   while(herb980615) {
     printf("%s\n",herb980615);
     herb980615=strtok(NULL,"\n");
   }
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spx_newdummy_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/NEWDUMMY NAME
*:<---------------------------------------------------------------------
*/
void kam_spx_newdummy_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */

        STAFCV_T status = spx_newdummy(name);
}
STAFCV_T spx_newdummy(char* name)
{
   if( !spx->newDummy(name) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spx_newgrid_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/NEWGRID NAME HEIGHT WIDTH
*:<---------------------------------------------------------------------
*/
void kam_spx_newgrid_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */
   short height = ku_geti();	/* spxGrid height */
   short width = ku_geti();	/* spxGrid width */

        STAFCV_T status = spx_newgrid(name,width,height);
}
STAFCV_T spx_newgrid(char* name, short height, short width)
{
   if( !spx->newGrid(name,height,width) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

