/***************************************************************************
 *
 * $Id: StEstTrackSelection.cxx,v 1.14 2003/10/11 02:51:20 perev Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the track and branch selections
 *
 ***************************************************************************
 *
 * $Log: StEstTrackSelection.cxx,v $
 * Revision 1.14  2003/10/11 02:51:20  perev
 * Cleanup+bugfix: test for zer pointer, initialization added.
 *
 * Revision 1.13  2003/04/30 20:36:55  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.12  2002/02/20 17:22:03  caines
 * Comment out some of the print statements
 *
 * Revision 1.11  2001/07/15 20:31:31  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.10  2001/04/25 17:34:46  perev
 * HPcorrs
 *
 * Revision 1.9  2001/04/20 16:22:40  lmartin
 * New version of ChooseBestNBranches.
 *
 * Revision 1.8  2001/02/23 08:50:19  lmartin
 * StMessMgr.h included in order to use gMessMgr.
 *
 * Revision 1.7  2001/02/23 08:47:05  lmartin
 * cout replaced by gMessMgr.
 *
 * Revision 1.6  2001/02/14 17:51:48  perev
 * delete [] added
 *
 * Revision 1.5  2001/02/07 19:16:30  caines
 * Fix sun non compilation for non-fixed size array
 *
 * Revision 1.4  2001/01/31 16:56:03  lmartin
 * mParams[]->debug replaced by mDebug.
 *
 * Revision 1.3  2001/01/25 18:06:18  lmartin
 * Methods declared as StEstTracker methods.
 * ChooseBestBranch and RemoveHitSharing methods rewritten to prevent memory leak.
 *
 * Revision 1.2  2000/12/07 16:49:24  lmartin
 * Remove unused methods to compile under Sun
 *
 * Revision 1.1  2000/12/07 11:14:22  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StMessMgr.h"
#include "StEstTracker.h"
#include "StEstParams.hh"
#include "Infrastructure/StEstWafer.hh"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstHit.hh"
#include "Infrastructure/StEstTrack.hh"
#include "Infrastructure/StEstTPCTrack.hh"

void StEstTracker::FlagTPCTracksSP(int overPass) {
  // method to temporarily (within a given superpass) flag (with a negative 
  // value) some of the TPC tracks :
  // If the number of hits is smaller than minPTChits : flag=-1
  // If the radius (in the x-y plane) of the track origin is 
  // bigger than rmaxTPC  : flag=-2

  int i;

  for (i=0;i<mNTrack;i++) {
    mTrack[i]->mTPCTrack->SetFlagSP(0);
    if (mTrack[i]->mTPCTrack->GetR() > mSegments[overPass]->rminTPC)
      mTrack[i]->mTPCTrack->SetFlagSP(-1);
    if (mTrack[i]->mTPCTrack->GetNHits() < mSegments[overPass]->minTPChits)
      mTrack[i]->mTPCTrack->SetFlagSP(-2);
  }
  
}


void StEstTracker::ChooseBestNBranches(StEstTrack *tr, int slay) {
  // Method used to prevent a large number of branches to be formed for a given track
  // For a given track in a given layer the mParams[mPass]->ntotBranch[slay]
  // branches with the best chisq are selected.
  // The branches are sorted according to their chisq (weighted by the number of hits)
  // The branches are then swapped according to the sorted list.
  // The unselected branches are removed from the track.
  int i,j;
  int itemp,j_2swap;
  int indtmp, *indchi;
  double chii, chij;
  StEstBranch *br_2swap;

  if(mDebugLevel>1)
    gMessMgr->Info()<<"StEstMaker::ChooseBestNBranches ****START****"<<endm;


  if(mDebugLevel>1)
    gMessMgr->Info()<<"tr->GetNBranches()= "<<tr->GetNBranches()
		    <<" Keeping "<<mParams[mPass]->ntotbranch[slay]<<" branches"<<endm;

  // filling the indchi array from smallest to biggest chisq
  indchi = new int[mParams[0]->maxbranches];
  for (i=0;i<tr->GetNBranches();i++)
    indchi[i] = i;
  for (i=0;i<tr->GetNBranches()-1;i++) {
    for (j=i+1;j<tr->GetNBranches();j++) {
      chii = tr->GetBranch(indchi[i])->GetChiSq()/tr->GetBranch(indchi[i])->GetNHits();
      chij = tr->GetBranch(indchi[j])->GetChiSq()/tr->GetBranch(indchi[i])->GetNHits();
      if (chii>chij) {
	indtmp    = indchi[i];
	indchi[i] = indchi[j];
	indchi[j] = indtmp;
      }
    }
  }

  // swapping the branches according to the indchi list
  for (i=0;i<tr->GetNBranches();i++) {
    br_2swap=tr->GetBranch(i);
    tr->SetBranch(i,tr->GetBranch(indchi[i]));
    tr->SetBranch(indchi[i],br_2swap);
    for (j=0;j<tr->GetNBranches();j++)
      if (indchi[j]==i) j_2swap=j;
    itemp=indchi[i];
    indchi[i]=i;
    indchi[j_2swap]=itemp;
  }

  // removing the worst branches
  int brmax = tr->GetNBranches();
  for (i=brmax-1;i>mParams[mPass]->ntotbranch[slay];i--)
    tr->RemoveBranch(i);

  if(mDebugLevel>1)
    gMessMgr->Info()<<"tr->GetNBranches()= "<<tr->GetNBranches()<<endm;

  delete[] indchi;

  if(mDebugLevel>1)
    gMessMgr->Info()<<"StEstMaker::ChooseBestNBranches ****STOP****" <<endm;
}
       
void StEstTracker::ChooseBestBranch(StEstTrack *tr, int overPass) {
  // method to keep only one branch per track.
  // The branch with the smallest chisq is located
  // and swapped with the branch 0. The other branches
  // are then removed.

  int i;
  double chimin=100000000, chi;
  int chinr = -1;
  StEstBranch *br_best=0;

  for (i=0;i<tr->GetNBranches();i++) {
    StEstBranch *br = tr->GetBranch(i);
    if(!br)	continue;
    int nhits = br->GetNHits();
    if(!nhits) 	continue;
    chi = br->GetChiSq()/nhits;
    if (chi<chimin) {
	chimin = chi;
	chinr  = i;
        br_best==br;
    }
    if (tr->GetNBranches()>1 && tr->GetBranch(i)->GetNHits()==0) gMessMgr->Error()<<"StEstMaker::ChooseBestBranch  branch with no hit while track with >1 branch  track= "<<tr->mTPCTrack->mId<<endm;    
  }

  if (br_best) {
    tr->SetBranch(chinr,tr->GetBranch(0));
    tr->SetBranch(0,br_best);
  }
  int brmax = tr->GetNBranches();
  for (i=brmax-1;i>0;i--) tr->RemoveBranch(i);

}
    
void StEstTracker::RemoveOneHitTracks() {

  int i,j;

  if(mDebugLevel>2) gMessMgr->Info()<<"*** Removal of one-hit-tracks ***"<<endm;
  for (i=0;i<mNTrack;i++) { // loop over the tracks
    // we assume we have already last only one branch
    if(mDebugLevel>2)
      gMessMgr->Info()<<" Track #"<<i<< "  "<<mTrack[i]->GetBranch(0)->GetNHits()<<endm;
    if (mTrack[i]->GetBranch(0)->GetNHits()<2 && mTrack[i]->GetBranch(0)->GetNHits()>0) {
      if(mDebugLevel>2) gMessMgr->Info()<<"<-- remove"<<endm;
      for (j=0;j<mTrack[i]->GetBranch(0)->GetNHits();j++) 
	mTrack[i]->GetBranch(0)->GetHit(j)->LeaveBranch(mTrack[i]->GetBranch(0));
    }
  }
}


void StEstTracker::RemoveHitSharing() {
  // Hit sharing removal.
  // For each hit shared by several branches, we determine the branch (brmin)
  // with the lowest chisq (minbr). The  other branches are deleted. We assume
  // here that when this method is called only one branch per track survives
  // the previous selection (such as ChooseBestBranch).
  
  int brmax,i,j,k,brmin;
  int HowMany;
  double minbr;

  if(mDebugLevel>2)
    gMessMgr->Info()<<"StEstMaker::RemoveHitSharing ****START****"<<endm;

  HowMany=0;
  for (i=0;i<mNSvtHit;i++) {    
    if (mSvtHit[i]->GetNBranch()<2) continue;
    brmin=0;
    minbr=1.e+99;
    StEstTrack **track = new StEstTrack*[mSvtHit[i]->GetNBranch()];
    int* br_index = new int[mSvtHit[i]->GetNBranch()];

    for (j=0;j<mSvtHit[i]->GetNBranch();j++){
      track[j]=mSvtHit[i]->mBranch[j]->mTrack;
      for (k=0;k<track[j]->GetNBranches();k++)
	if (track[j]->GetBranch(k)==mSvtHit[i]->mBranch[j]) br_index[j]=k;
      if (mSvtHit[i]->mBranch[j]->GetChiSq()<minbr){
	brmin=j;
	minbr=mSvtHit[i]->mBranch[j]->GetChiSq();
      }
    }
    brmax = mSvtHit[i]->GetNBranch(); 

    for (j=0;j<brmax;j++) {
      if(j==brmin) continue;
      track[j]->RemoveBranch(br_index[k]);
      HowMany++;
    }
    for (j=0;j<brmax;j++) track[j]=0;
    delete [] track;
    delete [] br_index;
  }
  gMessMgr->Info()<<HowMany<<" branches cleared"<<endm;
  if(mDebugLevel>2)
        gMessMgr->Info()<<"StEstMaker::RemoveHitSharing ****STOP****"<<endm;
}
void StEstTracker::RemoveHitSharing2() {
  // Hit sharing removal.
  // For each hit shared by several branches, we determine the branch (brmin)
  // with the lowest chisq (minbr). The  other branches are deleted. We assume
  // here that when this method is called only one branch per track survives
  // the previous selection (such as ChooseBestBranch).
  
  int brmax,i,j,brmin; //VP unused k removed
  int HowMany;
  double minbr;

  if(mDebugLevel>2)
    gMessMgr->Info()<<"StEstMaker::RemoveHitSharing ****START****"<<endm;

  HowMany=0;
  for (i=0;i<mNSvtHit;i++) {    
    if (mSvtHit[i]->GetNBranch()<2) continue;
    brmin=0;
    minbr=1.e+99;
    StEstTrack *mytrack;

    for (j=0;j<mSvtHit[i]->GetNBranch();j++){
      if (mSvtHit[i]->mBranch[j]->GetChiSq()<minbr){
	brmin=j;
	minbr=mSvtHit[i]->mBranch[j]->GetChiSq();
      }
      //cout<<"j,nbranch,chisq,brmin,minbr = "<<j<<" "<<mSvtHit[i]->GetNBranch()
      //	  <<" "<<mSvtHit[i]->mBranch[j]->GetChiSq()<<" "<<brmin<<" "<<minbr<<endl;
    }

    brmax = mSvtHit[i]->GetNBranch(); 

    for (j=0;j<brmax;j++) {
      if(j==brmin) continue;
      mytrack=mSvtHit[i]->mBranch[j]->mTrack;
      mytrack->RemoveBranch(0);
      HowMany++;
    }
  }
  gMessMgr->Info()<<HowMany<<" branches cleared"<<endm;
  if(mDebugLevel>2)
        gMessMgr->Info()<<"StEstMaker::RemoveHitSharing ****STOP****"<<endm;
}

void StEstTracker::ChooseSegment(int overPass,int layer) {
  // method to select the branches according to the 
  // superpass segment condition. 
  // modified on Aug-24 by lm. The method is now called 
  // at the layer loop level to remove branches which will not
  // fulfilled the condition at the end of the layer loop
  // The branches matching the segment criteria are identified
  // and placed at the top of the branch list. The remaining branches
  // are then removed.

  int   i,j,k;
  int    pattern0, pattern1=0, nbr,k_swap,lastgood,isok_swap;
  int    MaxRemainingHits=0;
  StEstBranch *br_swap;

  // pattern1 contains the mandatory layers (slay=2)
  for (i=layer; i<4; i++)
    if (mSegments[overPass]->slay[i]==2 &&
	mParams[0]->onoff[i]==1) pattern1 |= (1<<i);
  // MaxRemainingHits is the maximum number of hits
  // which can be associated in the layers which still
  // have to be scanned.
  for (i=0; i<layer; i++)
    if (mSegments[overPass]->slay[i]>0 &&
	mParams[0]->onoff[i]==1) MaxRemainingHits++;

  // now we scan all the tracks (concerned by this pass)
  for (i=0; i<mNTrack; i++) {
    if(mTrack[i]->GetDoIt()!=1) continue;
    nbr=mTrack[i]->GetNBranches();
    int* isok = new int[nbr];    
    for (j=0;j<nbr;j++) 
      isok[j] = 1;

    for (j=0;j<nbr;j++) {      
      pattern0=0;
      for(k=0; k<mTrack[i]->GetBranch(j)->GetNHits(); k++)
	pattern0 |= (1<<(mTrack[i]->GetBranch(j)->GetHit(k)->GetWafer()->GetLayer()));
      // segment or minimum hit number condition not fulfilled
      if ((pattern1!=(pattern1 & pattern0)) || 
	  (mTrack[i]->GetBranch(j)->GetNHits()<mSegments[overPass]->minhits-MaxRemainingHits)) 
	isok[j]=0;
    }
    // now we sort the branches. The good ones are put at the beginning of the branch list
    for (j=0;j<nbr;j++) {
      if (isok[j]==0) {
	k_swap=-1;
	for (k=j+1;k<nbr;k++)
	  if (k_swap==-1 && isok[k]==1) k_swap=k;
	if (k_swap!=-1) {
	  br_swap=mTrack[i]->GetBranch(k_swap);
	  mTrack[i]->SetBranch(k_swap,mTrack[i]->GetBranch(j));
	  mTrack[i]->SetBranch(j,br_swap);
	  isok_swap=isok[k_swap];
	  isok[k_swap]=isok[j];
	  isok[j]=isok_swap;
	}
      }
    }
    lastgood=-1;
    for (j=0;j<nbr;j++)
      if (isok[j]==1) lastgood=j;
    // now we kill all but the good branches...
    for (j=nbr-1;j>lastgood;j--) mTrack[i]->RemoveBranch(j);
    delete [] isok;
  }
}


void StEstTracker::FinishFlag() {
  // flag the found tracks and their hits

  int il,maxl;
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
void StEstTracker::ReInitializeHelix() {
  // ReInitialize the helix of the branch 0 
  // in case of no segment has been kept 

  int il,HowMany;
  StEstBranch *br;
  
  HowMany=0;
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
      else gMessMgr->Error()<<"ReInitializeHelix : Error !! The track_id "
			    <<mTrack[il]->mTPCTrack->GetId()
			    <<" is reinitialized but still have hits attached "<<endm;
    }
  }
  if (mDebugLevel>0)
    gMessMgr->Info()<<"ReInitializing the unselected tracks : "<<HowMany<<" tracks initialized"<<endm;
}
