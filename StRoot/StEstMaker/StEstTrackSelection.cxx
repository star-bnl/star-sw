/***************************************************************************
 *
 * $Id: StEstTrackSelection.cxx,v 1.2 2000/12/07 16:49:24 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the track and branch selections
 *
 ***************************************************************************
 *
 * $Log: StEstTrackSelection.cxx,v $
 * Revision 1.2  2000/12/07 16:49:24  lmartin
 * Remove unused methods to compile under Sun
 *
 * Revision 1.1  2000/12/07 11:14:22  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstMaker.h"


void StEstMaker::FlagTPCTracksSP(int overPass) {
  // method to temporarily (within a given superpass) flag (with a negative 
  // value) some of the TPC tracks :
  // If the number of hits is smaller than minPTChits : flag=-1
  // If the radius (in the x-y plane) of the track origin is 
  // bigger than rmaxTPC  : flag=-2

  long i;

  for (i=0;i<mNTrack;i++) {
    mTrack[i]->mTPCTrack->SetFlagSP(0);
    if (mTrack[i]->mTPCTrack->GetR() > mSegments[overPass]->rminTPC)
      mTrack[i]->mTPCTrack->SetFlagSP(-1);
    if (mTrack[i]->mTPCTrack->GetNHits() < mSegments[overPass]->minTPChits)
      mTrack[i]->mTPCTrack->SetFlagSP(-2);
  }
  
}


void StEstMaker::ChooseBestNBranches(StEstTrack *tr, int slay) {
  // Method used to prevent a large number of branches to be formed for a given track
  // For a given track in a given layer the mParams[mPass]->ntotBranch[slay]
  // branches with the best chisq are selected.
  // The branches are sorted according to their chisq (minus the TPC chisq)
  // The unselected branches are deleted.
  int i, j;
  int indtmp, *indchi;
  double chii, chij;

  if(mParams[mPass]->debug>2)
    cout << "\n\nStEstMaker::ChooseBestNBranches ****START****" <<endl;
  //  for (i=0;i<tr->GetNBranches();i++)
    //    for (j=0;j<tr->GetNBranches()->GetNHits();j++)

  StEstBranch **br_tmp = new StEstBranch*[tr->GetNBranches()];

  if(mParams[mPass]->debug>2)
    cout << "tr->GetNBranches()= "<<tr->GetNBranches()<<endl;

  indchi = new int[mParams[0]->maxbranches];

  for (i=0;i<tr->GetNBranches();i++)
    indchi[i] = i;
  for (i=0;i<tr->GetNBranches()-1;i++) {
    for (j=i+1;j<tr->GetNBranches();j++) {
      //      chii = tr->GetBranch(indchi[i])->GetChiSq() - tr->mTPCTrack->mChiSq;
      //      chij = tr->GetBranch(indchi[j])->GetChiSq() - tr->mTPCTrack->mChiSq;
      chii = tr->GetBranch(indchi[i])->GetChiSq()/tr->GetBranch(indchi[i])->GetNHits();
      chij = tr->GetBranch(indchi[j])->GetChiSq()/tr->GetBranch(indchi[i])->GetNHits();
      if (chii>chij) {
	indtmp    = indchi[i];
	indchi[i] = indchi[j];
	indchi[j] = indtmp;
      }
    }
  }
  
  for (i=mParams[mPass]->ntotbranch[slay]; i<tr->GetNBranches();i++) {
    br_tmp[i] = tr->GetBranch(indchi[i]);
  }
  int brmax = tr->GetNBranches();
  for (i=mParams[mPass]->ntotbranch[slay]; i<brmax;i++) {
    delete br_tmp[i];
  }

  if(mParams[mPass]->debug>2)
    cout << "tr->GetNBranches()= "<<tr->GetNBranches()<<endl;

  delete[] indchi;
  delete[] br_tmp;

  if(mParams[mPass]->debug>2)
    cout << "StEstMaker::ChooseBestNBranches ****STOP****" <<endl;
}
       
void StEstMaker::ChooseBestBranch(StEstTrack *tr, int overPass) {
  long i;
  double chimin=100000000, chi;
  int chinr = 0;

  StEstBranch **br_tmp = new StEstBranch*[tr->GetNBranches()];

  for (i=0;i<tr->GetNBranches();i++) {
    //    chi = tr->GetBranch(i)->GetChiSq() - tr->mTPCTrack->mChiSq;
    chi = tr->GetBranch(i)->GetChiSq()/tr->GetBranch(i)->GetNHits();
      if (chi<chimin) {
	chimin = chi;
	chinr  = i;
      }
      if (tr->GetNBranches()>1 && tr->GetBranch(i)->GetNHits()==0) cout <<"StEstMaker::ChooseBestBranch  branch with no hit while track with >1 branch  track= "<<tr->mTPCTrack->mId<<endl;    
  }

  // loop to fill the temporary table of branches
  for (i=0; i<tr->GetNBranches();i++) {
    br_tmp[i] = tr->GetBranch(i);
  }
  int brmax = tr->GetNBranches();

  for (i=0;i<brmax;i++) {
    if (i!=chinr) {      
      // killing me softly...
      delete br_tmp[i];
    }
    else {
      tr->SetBranch(0,br_tmp[i]);
    }
  }
  delete[] br_tmp;
}
    
void StEstMaker::RemoveOneHitTracks() {

  long i,j;

  if(mParams[mPass]->debug>2) cout << "*** Removal of one-hit-tracks ***"<<endl;
  for (i=0;i<mNTrack;i++) { // loop over the tracks
    // we assume we have already last only one branch
    if(mParams[mPass]->debug>2)
      cout<<" Track #"<<i<< "  "<<mTrack[i]->GetBranch(0)->GetNHits();
    if (mTrack[i]->GetBranch(0)->GetNHits()<2 && mTrack[i]->GetBranch(0)->GetNHits()>0) {
      if(mParams[mPass]->debug>2) cout <<" <-- remove"<<endl;
      for (j=0;j<mTrack[i]->GetBranch(0)->GetNHits();j++) 
	mTrack[i]->GetBranch(0)->GetHit(j)->LeaveBranch(mTrack[i]->GetBranch(0));
    }
    else if(mParams[mPass]->debug>2) cout <<endl;
  }
}


void StEstMaker::RemoveHitSharing() {
  // Hit sharing removal.
  // For each hit shared by several branches, we determine the branch (brmin)
  // with the lowest chisq (minbr) then the other branches are either deleted 
  // (if their mother track has several branches) or reset by removing the hits 
  // (if the mother track has only one branch).
  
  long brmax,i,j,brmin;
  long kl,nhit;
  long HowMany;
  double minbr;

  StEstHit    **hit_tmp = new StEstHit*[20];
  StEstTrack *track;

  if(mParams[0]->debug>2)
    cout << "StEstMaker::RemoveHitSharing ****START****"<<endl;

  cout<<"Removing Hit Sharing : ";
  HowMany=0;
  for (i=0;i<mNSvtHit;i++) {    
    brmin=0;
    minbr=1.e+99;
    StEstBranch **br_tmp = new StEstBranch*[mSvtHit[i]->GetNBranch()];

    for (j=0;j<mSvtHit[i]->GetNBranch();j++){
      br_tmp[j] = mSvtHit[i]->mBranch[j];
      if (br_tmp[j]->GetChiSq()<minbr){
	brmin=j;
	minbr=br_tmp[j]->GetChiSq();
      }
    }
    brmax = mSvtHit[i]->GetNBranch(); 

    for (j=0;j<brmax;j++) {
      if(j==brmin) continue;
      track=br_tmp[j]->mTrack;
      if (track->GetNBranches()>1) {
	br_tmp[j]->LeaveTrack();
	HowMany++;
      }
      else {
	nhit=br_tmp[j]->GetNHits();
	for (kl=0;kl<nhit;kl++) 
	  hit_tmp[kl] = br_tmp[j]->GetHit(kl);
	for (kl=0;kl<nhit;kl++) 
	  hit_tmp[kl]->LeaveBranch(br_tmp[j]);
	br_tmp[j]->SetIsGood(0);
	HowMany++;
      }
    }
    delete[] br_tmp;
  }
  delete[] hit_tmp;
  cout<<HowMany<<" branches cleared"<<endl;
  if(mParams[0]->debug>2)
        cout << "StEstMaker::RemoveHitSharing ****STOP****"<<endl;
}


void StEstMaker::ChooseSegment(int overPass,int layer) {
  // method to select the branches according to the 
  // superpass segment condition. 
  // modified on Aug-24 by lm. The method is now called 
  // at the layer loop level to remove branches which will not
  // fulfilled the condition at the end of the layer loop

  long   il, jl, kl, maxl;
  int    pattern0, pattern1=0, brmax;
  int    MaxRemainingHits=0;
  StEstBranch *br;
  StEstHit    **hit_tmp = new StEstHit*[20];

  // pattern1 contains the mandatory layers (slay=2)
  for (il=layer; il<4; il++)
    if (mSegments[overPass]->slay[il]==2 &&
	mParams[0]->onoff[il]==1) pattern1 |= int(pow(2,il));
  // MaxRemainingHits is the maximum number of hits
  // which can be associated in the layers which still
  // have to be scanned.
    for (il=0; il<layer; il++)
    if (mSegments[overPass]->slay[il]>0 &&
	mParams[0]->onoff[il]==1) MaxRemainingHits++;

  for (il=0; il<mNTrack; il++) {
    if(mTrack[il]->GetDoIt()!=1) continue;

    StEstBranch **br_tmp = new StEstBranch*[mTrack[il]->GetNBranches()];

    for (jl=0;jl<mTrack[il]->GetNBranches();jl++) 
      br_tmp[jl] = mTrack[il]->GetBranch(jl); 

    brmax = mTrack[il]->GetNBranches();

    for (jl=0;jl<brmax;jl++) {      
      br   = br_tmp[jl];
      maxl = br->GetNHits();
      pattern0=0;
    
      for(kl=0; kl<maxl; kl++)
	pattern0 |= int(pow(2,br->GetHit(kl)->GetWafer()->GetLayer()));
      
      if ((pattern1!=(pattern1 & pattern0)) || 
	  (maxl<mSegments[overPass]->minhits-MaxRemainingHits)) { 
	// segment or minimum hit number condition not fulfilled
	if (mTrack[il]->GetNBranches()>1) {
	  // we kill the branch if it's not the last one
	  delete br_tmp[jl];
	}
	else {
	  // in other case we remove the hits from the branch
	  for (kl=0;kl<maxl;kl++) 
	    hit_tmp[kl] = br->GetHit(kl);
	  for (kl=0;kl<maxl;kl++) 
	    hit_tmp[kl]->LeaveBranch(br);
	}
      }
      
    }
    delete [] br_tmp;
  }
  delete [] hit_tmp;
}


void StEstMaker::FinishFlag() {
  // flag the found tracks

  long il,maxl;
  int jl;
  StEstBranch *br;
  
  for (il=0;il<mNTrack;il++) {
    if(mTrack[il]->GetDone()==1){
      br = mTrack[il]->GetBranch(0);
      maxl = br->GetNHits();
      if (maxl>0) {
	for (jl=0;jl<maxl;jl++)
	  br->GetHit(jl)->SetFlag(1);
	mTrack[il]->SetFlag(1);
      }
    }
  }
}
void StEstMaker::ReInitializeHelix() {
  // ReInitialize the helix of the branch 0 
  // in case of no segment has been kept 

  long il,HowMany;
  StEstBranch *br;
  
  HowMany=0;
  cout<<"ReInitializing the unselected tracks : ";
  for (il=0;il<mNTrack;il++) {
    if(mTrack[il]->GetDone()==1 && mTrack[il]->GetFlag()==0){
      br = mTrack[il]->GetBranch(0);
      StHelix *new_helix = new StHelix(*mTrack[il]->GetHelix());
      if (br->GetNHits()==0) {
	br->SetHelix(new_helix);
	br->SetIsGood(1);
	br->SetIsGoodOld(1);
	mTrack[il]->SetDone(0);
	HowMany++;
      }
      else cout<<"ReInitializeHelix : Error !! The track_id "
	       <<mTrack[il]->mTPCTrack->GetId()
	       <<" is reinitialized but still have hits attached "<<endl;
    }
  }
  cout<<HowMany<<" tracks initialized"<<endl;
}
