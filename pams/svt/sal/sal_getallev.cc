//*********************************************************************
//File: sal_getallev.c
//Description: 
//Input:  ,g2t_svt_hit
//Output: sal_sptallev
//*********************************************************************


#include "sal_getallev.h"


long type_of_call sal_getallev_( 
TABLE_HEAD_ST    *g2t_svt_hit_h, G2T_SVT_HIT_ST  *g2t_svt_hit ,
TABLE_HEAD_ST    *g2t_svt_allev_h, G2T_SVT_HIT_ST  *g2t_svt_allev)
{

int hit,chit;

if(g2t_svt_hit_h->nok <1)
 {
   printf("\n Empty table g2t_svt_hit_h in function sal_getallev \n ");
    getchar();
   return STAFCV_BAD;
  }

chit = g2t_svt_allev_h->nok ;

for ( hit =0 ; hit < g2t_svt_hit_h->nok; hit++)
  {
   g2t_svt_allev[chit].id = g2t_svt_hit[hit].id + g2t_svt_allev_h->nok -1;

   g2t_svt_allev[chit].x[0] = g2t_svt_hit[hit].x[0];
   g2t_svt_allev[chit].x[1] = g2t_svt_hit[hit].x[1];
   g2t_svt_allev[chit].x[2] = g2t_svt_hit[hit].x[2];

   g2t_svt_allev[chit].next_tr_hit_p = g2t_svt_hit[hit].next_tr_hit_p;
   g2t_svt_allev[chit].volume_id = g2t_svt_hit[hit].volume_id;
   g2t_svt_allev[chit].de = g2t_svt_hit[hit].de;
   g2t_svt_allev[chit].ds = g2t_svt_hit[hit].ds;
   g2t_svt_allev[chit].p[0] = g2t_svt_hit[hit].p[0];
   g2t_svt_allev[chit].p[1] = g2t_svt_hit[hit].p[1];
   g2t_svt_allev[chit].p[2] = g2t_svt_hit[hit].p[2];
   g2t_svt_allev[chit].tof = g2t_svt_hit[hit].tof;
   g2t_svt_allev[chit].track_p = g2t_svt_hit[hit].track_p;
  chit++;
}

g2t_svt_allev[chit-1].id=-g2t_svt_allev[chit-1].id;

g2t_svt_allev_h->nok += g2t_svt_hit_h->nok ;


return STAFCV_OK;
}


