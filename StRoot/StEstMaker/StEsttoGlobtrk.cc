/***************************************************************************
 *
 * $Id: StEsttoGlobtrk.cc,v 1.7 2001/02/28 18:27:19 caines Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods to save the Est results into the global tables
 *
 ***************************************************************************
 *
 * $Log: StEsttoGlobtrk.cc,v $
 * Revision 1.7  2001/02/28 18:27:19  caines
 * Get psi angle correctly
 *
 * Revision 1.6  2001/02/27 16:56:24  caines
 * Get helix track info. directly from the fit track
 *
 * Revision 1.5  2001/02/23 14:53:31  lmartin
 * cout replaced by gMessMgr.
 *
 * Revision 1.4  2001/02/21 23:50:21  caines
 * Add some more info to SVT track for kalman fitting initial guess
 *
 * Revision 1.3  2001/01/31 16:59:54  lmartin
 * mParams[]->debug replaced by mDebug.
 *
 * Revision 1.2  2001/01/25 18:10:21  lmartin
 * Method declared as StEstTracker method.
 * Output tables passed as arguments of the method.
 *
 * Revision 1.1  2000/12/07 11:14:22  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StMessMgr.h"
#include "StEstTracker.h"
#include "StEstParams.hh"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstHit.hh"
#include "Infrastructure/StEstTrack.hh"
#include "Infrastructure/StEstTPCTrack.hh"
#include "tables/St_svm_evt_match_Table.h"
#include "tables/St_stk_track_Table.h"
#include "tables/St_sgr_groups_Table.h"
#include "math_constants.h"

#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)

void StEstTracker::EsttoGlobtrk(St_stk_track* svttrk,
				St_sgr_groups* svtgrps,
				St_svm_evt_match* EstMatch){

  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);			    
  

  if (mDebugLevel>0)
    gMessMgr->Info()<<"StEstTracker::StEsttoGlobtrk : Saving into the global tables"<<endm;
  int CountHits=0;
  int CountMatch=0;
  int SaveHit, j;
  double q;
  
  StEstBranch *branch;
  StEstHit *hit;
  
  stk_track_st* svtTrkPtr  = svttrk->GetTable();
  sgr_groups_st* groups = svtgrps->GetTable();
  svm_evt_match_st* svtMatchPtr  = EstMatch->GetTable();
  
  for( int i=0; i<mNTrack; i++){

    CountMatch++;
    svtTrkPtr->id = CountMatch;
    svtTrkPtr->nspt = 0;
    svtMatchPtr->id  = CountMatch;
    svtMatchPtr->idsvt = svtTrkPtr->id;
    svtMatchPtr->idtpc = mTrack[i]->mTPCTrack->GetId();
 

    for( j=0;j<mTrack[i]->GetNBranches();j++) {
      branch = mTrack[i]->GetBranch(j);
      for ( int k=0;k<branch->GetNHits();k++) {
        hit = branch->GetHit(k);

	for( int l=0;l<mNSvtHit;l++) {
	  if (hit==mSvtHit[l]){
	    
	    svtTrkPtr->nspt++;
	    groups->id1 = svtTrkPtr->id;
	    groups->id2 = mSvtHit[l]->GetId();
	    groups->ident = 3;
	    groups++;
	    CountHits++;
	    break;
	  }

	}
      }
    }

    j = mTrack[i]->GetNBranches()-1;
    if(svtTrkPtr->nspt > 0){
      groups -= svtTrkPtr->nspt;
      //Now reorder hits so track goes from inner barrel out
      for( int nHits=0; nHits< svtTrkPtr->nspt/2; nHits++){
	SaveHit = groups[nHits].id2;
	groups[nHits].id2 = groups[svtTrkPtr->nspt-nHits-1].id2;
	groups[svtTrkPtr->nspt-nHits-1].id2 = SaveHit;
      }
      groups += svtTrkPtr->nspt;

      svtTrkPtr->z0 = mTrack[i]->GetBranch(j)->GetHelix()->z(0);
      svtTrkPtr->psi = mTrack[i]->GetBranch(j)->GetHelix()->phase()
	+mTrack[i]->GetBranch(j)->GetHelix()->h()*M_PI_2;
      svtTrkPtr->psi *= C_DEG_PER_RAD;
      svtTrkPtr->tanl = tan(mTrack[i]->GetBranch(j)->GetHelix()->dipAngle());
      q = ((b[2] * mTrack[i]->GetBranch(j)->GetHelix()->h()) > 0 ? -1 : 1);
      svtTrkPtr->invpt = q/mTrack[i]->GetTPCTrack()->GetPt();
      svtTrkPtr->r0 = sqrt(mTrack[i]->GetBranch(j)->GetHelix()->x(0)*
			   mTrack[i]->GetBranch(j)->GetHelix()->x(0)
			   +mTrack[i]->GetBranch(j)->GetHelix()->y(0)*
			   mTrack[i]->GetBranch(j)->GetHelix()->y(0));
      svtTrkPtr->phi0 =  atan2(mTrack[i]->GetBranch(j)->GetHelix()->y(0),
			       mTrack[i]->GetBranch(j)->GetHelix()->x(0))
	*C_DEG_PER_RAD;
      svtTrkPtr++;
      svtMatchPtr++;
      
    }
    else{         
      CountMatch--;
    }
    
  }
  
  svtgrps->SetNRows(CountHits);
  EstMatch->SetNRows(CountMatch);
  svttrk->SetNRows(CountMatch);
  gMessMgr->Info()<<"NRows(svttrk)="<<svttrk->GetNRows()<<endm;
  gMessMgr->Info()<<"NRows(svtgrps)="<<svtgrps->GetNRows()<<endm;
  gMessMgr->Info()<<"NRows(EstMatch)="<<EstMatch->GetNRows()<<endm;
}
