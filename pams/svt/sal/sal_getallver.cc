//*********************************************************************
//File: sal_getallver.c
//Description: 
//Input:  g2t_vertex
//Output: sal_vrtx
//*********************************************************************


#include "sal_getallver.h"


long type_of_call sal_getallver_( 
TABLE_HEAD_ST    *g2t_vertex_h, G2T_VERTEX_ST  *g2t_vertex ,
TABLE_HEAD_ST    *sal_vrtx_h, SAL_VRTX_ST  *sal_vrtx)
{

int hit,chit;

if(g2t_vertex_h->nok <1)
 {
   printf("\n Empty table g2t_svt_hit_h in function sal_getallev \n ");
    getchar();
   return STAFCV_BAD;
  }

   chit = sal_vrtx_h->nok ;

   hit = 0 ;
   sal_vrtx[chit].id = chit;

   sal_vrtx[chit].x[0] = g2t_vertex[hit].ge_x[0];
   sal_vrtx[chit].x[1] = g2t_vertex[hit].ge_x[1];
   sal_vrtx[chit].x[2] = g2t_vertex[hit].ge_x[2];


 sal_vrtx_h->nok += 1 ;


return STAFCV_OK;
}


