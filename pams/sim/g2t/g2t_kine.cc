/*:>--------------------------------------------------------------------
**: FILE:       g2t_kine.cc
**: HISTORY:
**:             15dec97-v000a-Yuri Fisyak
**:             31dec97-v001a-Pavel Nevski
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "g2t_kine.h"
#include "g2t_hits.h"
#include "geant_gcnum.h"
long g2t_kine_()
{
   long  npars   = 3;                   /* number of tables  */
   char* pname   = "g2t_get_kine";      /* first PAM name    */
   char* hname   = "g2t_get_hits";      /* second PAM name   */
   long  ntrack  = Gcnum.ntrack;
   long  nvertx  = Gcnum.nvertx;
   char* names[] = {"g2t_vertex","g2t_track","g2t_hits"}; /* TABLE names */
   STAFCV_T status;

   printf (" in g2t got Ntrack=%d, Nvertx=%d \n", Gcnum.ntrack,Gcnum.nvertx);

   /*             name      spec      Nrow  */
   tdm_newtable (names[0], names[0], nvertx);
   tdm_newtable (names[1], names[1], ntrack);
   tdm_newtable (names[2], names[2], 100000);

   tdmtable_maxrowcount(names[0], nvertx);
   tdmtable_maxrowcount(names[1], ntrack);
   tdmtable_maxrowcount(names[2], 100000);

   printf (" -> Calling g2t_kine \n"); 
   status = ami_call(pname,2,names);

   printf (" -> Calling g2t_hits \n"); 
   status = ami_call(hname,3,names);

   return STAFCV_OK;
}








