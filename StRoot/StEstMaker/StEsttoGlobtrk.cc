/***************************************************************************
 *
 * $Id: StEsttoGlobtrk.cc,v 1.3 2001/01/31 16:59:54 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods to save the Est results into the global tables
 *
 ***************************************************************************
 *
 * $Log: StEsttoGlobtrk.cc,v $
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
#include "StEstTracker.h"
#include "StEstParams.hh"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstHit.hh"
#include "Infrastructure/StEstTrack.hh"
#include "Infrastructure/StEstTPCTrack.hh"
#include "tables/St_svm_evt_match_Table.h"
#include "tables/St_stk_track_Table.h"
#include "tables/St_sgr_groups_Table.h"

//class St_stk_track;
//class St_sgr_groups;
//class St_svm_match;

void StEstTracker::EsttoGlobtrk(St_stk_track* svttrk,
				St_sgr_groups* svtgrps,
				St_svm_evt_match* EstMatch){
				    
  if (mDebugLevel>0)
    cout<<"StEstTracker::StEsttoGlobtrk : Saving into the global tables"<<endl;
  int CountHits=0;
  int CountMatch=0;
  int SaveHit;
  
  StEstBranch *branch;
  StEstHit *hit;
  
  //  St_stk_track     *svttrk     = new St_stk_track("EstSvtTrk",mNTrack);
  //  AddData(svttrk);
  stk_track_st* svtTrkPtr  = svttrk->GetTable();


  //  St_sgr_groups     *svtgrps     = new St_sgr_groups("EstGroups",mNTrack*10);
  //  AddData(svtgrps);
  sgr_groups_st* groups = svtgrps->GetTable();
  
  //  St_svm_evt_match     *EstMatch     = new St_svm_evt_match("EstMatch",mNTrack);
  //  AddData(EstMatch);
  svm_evt_match_st* svtMatchPtr  = EstMatch->GetTable();
  
  for( int i=0; i<mNTrack; i++){

    CountMatch++;
    svtTrkPtr->id = CountMatch;
    svtTrkPtr->nspt = 0;
    svtMatchPtr->id  = CountMatch;
    svtMatchPtr->idsvt = svtTrkPtr->id;
    svtMatchPtr->idtpc = mTrack[i]->mTPCTrack->GetId();
 
    for( int j=0;j<mTrack[i]->GetNBranches();j++) {
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
	  }
	}
      }
    }

    if(svtTrkPtr->nspt > 0){
      groups -= svtTrkPtr->nspt;
      //Now reorder hits so track goes from inner barrel out
      for( int nHits=0; nHits< svtTrkPtr->nspt/2; nHits++){
	SaveHit = groups[nHits].id2;
	groups[nHits].id2 = groups[svtTrkPtr->nspt-nHits-1].id2;
	groups[svtTrkPtr->nspt-nHits-1].id2 = SaveHit;
      }
      groups += svtTrkPtr->nspt;
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
  if (mDebugLevel>0)
    cout<<"StEstTracker::StEsttoGlobtrk : Stop"<<endl;
  
}
