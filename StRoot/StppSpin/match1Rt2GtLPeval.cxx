//*-- Author : Jan Balewski
//  
// $Id: match1Rt2GtLPeval.cxx,v 1.1.1.1 2001/04/21 00:43:14 fisyak Exp $
// $Log: match1Rt2GtLPeval.cxx,v $
// Revision 1.1.1.1  2001/04/21 00:43:14  fisyak
// *** empty log message ***
//
// Revision 1.1  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                    
//   compares reconstructed track to the generated one, using hits from TPC
//
//////////////////////////////////////////////////////////////////////////


#include <assert.h>
#include <math.h>
#include <strings.h>

#include "StppLPevalMaker.h"

#include "tables/St_jdata1_Table.h"

#include "KeyCounter.h"

#include "tables/St_g2t_track_Table.h" 
#include "tables/St_dst_track_Table.h" 


//________________________________________________________________
//________________________________________________________________
//________________________________________________________________
int StppLPevalMaker::match1Gt2Rt(St_jdata1 *jgtph,St_jdata1 *jutpcl, int *nMatchOne)
{// Return:  rtr_id if matched; <=0 of no matched _hits_ found
  //         *nMatchOne hits

  //  printf(" In matchRtra() ...\n");
  assert(jgtph);
  assert(jutpcl);

  KeyCounter kc;// counts hit-match to different Gtracks

  kc.clear();  

  //jgtph->Print(0,10);

  jdata1_st *GH=jgtph->GetTable(); assert(GH);
  int gtr_id=GH->id;
  for(int ig=0; ig<jgtph->GetNRows(); ig++,GH++)    {
    if( gtr_id!=GH->id) {
      printf("Error 1  matchRtra(): changed gtr_id=%d -->%d\n",gtr_id,GH->id);
      assert(gtr_id==GH->id);
    }

    //printf("Testing Ghit=%d , Rxy=%f..\n",ig,sqrt(GH->x[0]*GH->x[0] +GH->x[1]*GH->x[1] ));
    jdata1_st *CL=jutpcl->GetTable(); assert(CL);
    jdata1_st *CL0=NULL;
  
    float dr_min=h2hMatchRmin;  //(cm) maximal allowed distance
    float dr2_min=dr_min*dr_min;
    //  loop over Rhits to find match for this track
    for(int ir=0; ir<jutpcl->GetNRows(); ir++,CL++) {
      if(CL->id<0) continue; // hit already matched
      float dz=GH->x[2]-CL->x[2];
      if(fabs(dz)>dr_min) continue; // too fare in Z
      float dx=GH->x[0]-CL->x[0];
      float dy=GH->x[1]-CL->x[1];
      float dr2=dx*dx+dy*dy+dz*dz;
      if(dr2>dr2_min) continue; // too fare in 3_D
      CL0=CL;
      dr2_min=dr2;
      dr_min=sqrt(dr2);
    }
    //printf("   Ghit=%d done, dr_min=%f, add=%d\n",ig,dr_min,(int)CL);
    if(CL0==NULL)hv[3]->Fill(-.05);
    if(CL0==NULL) continue; // No H2H  match within  range

    kc.add(CL0->id);
    //  printf("   Rtr_id=%d\n",CL0->id);
    CL0->id=-1; // remove this hit from the pool
    hv[3]->Fill(dr_min);
  }

  int rtr_id=-1,  nMatchAll=-10;
  kc.getBest(&rtr_id, nMatchOne, &nMatchAll);
  
  printf("  h2h match Gid=%d Rid=%d, nMatchOne=%d, nMatchAll=%d\n",gtr_id,rtr_id,  *nMatchOne ,nMatchAll);
  
  return rtr_id;

}
