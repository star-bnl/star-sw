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
void kam_spxdummy_ncalls_(){kam_spxdummy_ncalls();}
int kam_spxdummy_ncalls()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */

   spxDummy* dummy;		/* spxDummy object */

   if( !spx->findDummy(name, dummy) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("SPXDUMMY:\tNumber of calls = %d \n",dummy->nCalls());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spxdummy_hello_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/DUMMY/HELLO NAME MESSAGE
*:<---------------------------------------------------------------------
*/
void kam_spxdummy_hello_(){kam_spxdummy_hello();}
int kam_spxdummy_hello()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */
   char* message = ku_gets();	/* message */

   spxDummy* dummy;		/* spxDummy object */

   if( !spx->findDummy(name, dummy) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !dummy->hello(message) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
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
void kam_spxdummy_null_(){kam_spxdummy_null();}
int kam_spxdummy_null()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */

   spxDummy* dummy;		/* spxDummy object */

   if( !spx->findDummy(name, dummy) 
   ||  !dummy->null() ){
      EML_ERROR(KAM_METHOD_FAILURE);
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
void kam_spxdummy_time_(){kam_spxdummy_time();}
int kam_spxdummy_time()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */

   spxDummy* dummy;		/* spxDummy object */

   if( !spx->findDummy(name, dummy) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   char* tim;
   if( !dummy->getTime(tim) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   printf("SPXDUMMY:\tTime = %s \n",tim);
   ASUFREE(tim);
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
void kam_spxgrid_height_(){kam_spxgrid_height();}
int kam_spxgrid_height()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */

   spxGrid* grid;		/* spxGrid object */

   if( !spx->findGrid(name, grid) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
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
void kam_spxgrid_width_(){kam_spxgrid_width();}
int kam_spxgrid_width()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */

   spxGrid* grid;		/* spxGrid object */

   if( !spx->findGrid(name, grid) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
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
void kam_spxgrid_get_(){kam_spxgrid_get();}
int kam_spxgrid_get()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */
   short m = ku_geti();		/* first index */
   short n = ku_geti();		/* second index */

   spxGrid* grid;		/* spxGrid object */

   if( !spx->findGrid(name, grid) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   long value;
   if( !grid->get(m,n,value) ){
      EML_ERROR(KAM_METHOD_FAILURE);
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
void kam_spxgrid_set_(){kam_spxgrid_set();}
int kam_spxgrid_set()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */
   short m = ku_geti();		/* first index */
   short n = ku_geti();		/* second index */
   long value = ku_geti();	/* new grid value */

   spxGrid* grid;		/* spxGrid object */

   if( !spx->findGrid(name, grid) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !grid->set(m,n,value) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spx_count_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/MANAGER/COUNT
*:<---------------------------------------------------------------------
*/
void kam_spx_count_(){kam_spx_count();}
int kam_spx_count()
{
   long npars = ku_npar();	/* number of KUIP parameters */

   printf("SPX:\tObject count = %d \n",spx->count());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spx_list_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/MANAGER/LIST
*:<---------------------------------------------------------------------
*/
void kam_spx_list_(){kam_spx_list();}
int kam_spx_list()
{
   long npars = ku_npar();	/* number of KUIP parameters */

   printf("%s",spx->list() );
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spx_newdummy_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/MANAGER/NEWDUMMY NAME
*:<---------------------------------------------------------------------
*/
void kam_spx_newdummy_(){kam_spx_newdummy();}
int kam_spx_newdummy()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxDummy name */

   if( !spx->newDummy(name) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_spx_newgrid_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SPX/MANAGER/NEWGRID NAME HEIGHT WIDTH
*:<---------------------------------------------------------------------
*/
void kam_spx_newgrid_(){kam_spx_newgrid();}
int kam_spx_newgrid()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* spxGrid name */
   short height = ku_geti();	/* spxGrid height */
   short width = ku_geti();	/* spxGrid width */

   if( !spx->newGrid(name,height,width) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

