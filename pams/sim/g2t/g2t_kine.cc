/*:>--------------------------------------------------------------------
**: FILE:       g2t_kine.cc
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.26 1997/11/10 19:59:13 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "g2t_kine.h"
#include "geant_gcnum.h"
long g2t_kine_()
{
   long npars = 2;      /* number of tables  */
   char*  pname = "g2t_get_kine";     /* PAM name */
   long ntrack = Gcnum.ntrack;
   long nvertx = Gcnum.nvertx;
   char *names[] = {"g2t_vertex","g2t_track"}; /*  array of TABLE names */
   char *tnames[2]; 
   printf (" Got Ntrack=%d, Nvertx=%d \n", Gcnum.ntrack,Gcnum.nvertx);
   tnames[0] = names[0]; 
   tnames[1] = names[1]; 
   /*             name      spec      Nrow  */
   tdm_newtable (names[0], names[0], nvertx);
   tdm_newtable (names[1], names[1], ntrack);
   tdmtable_maxrowcount(names[0], nvertx);
   tdmtable_maxrowcount(names[1], ntrack);
   printf (" tables ready, Calling g2t_kine \n"); 
   STAFCV_T status = ami_call(pname,npars, tnames);
   return STAFCV_OK;
}








