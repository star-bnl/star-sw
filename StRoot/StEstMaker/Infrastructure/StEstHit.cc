/***************************************************************************
 *
 * $Id: StEstHit.cc,v 1.6 2002/02/20 17:22:07 caines Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the StEstHit class
 *
 ***************************************************************************
 *
 * $Log: StEstHit.cc,v $
 * Revision 1.6  2002/02/20 17:22:07  caines
 * Comment out some of the print statements
 *
 * Revision 1.5  2001/07/15 20:31:33  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.4  2001/02/23 13:27:12  lmartin
 * cout replaced by gMessMgr.
 *
 * Revision 1.3  2001/01/26 09:49:25  lmartin
 * Minor changes. Useless data member mEvalTrack removed. Short description of the
 * data members added.
 *
 * Revision 1.2  2001/01/25 18:15:21  lmartin
 * New method DetachFromWafer.
 *
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StMessMgr.h"
#include "StEstHit.hh"
#include "StEstWafer.hh"
#include "StEstBranch.hh"


StEstHit::StEstHit(long id, StThreeVectorD *xg, StThreeVectorD *xl, StThreeVectorD *eg, StThreeVectorD *el, long maxbranches, long maxsh, StEstWafer *det) {
  int i;
  mId = id;
  mXL = xl;
  mXG = xg;
  mEL = el;
  mEG = eg;
  mMaxShare   = maxsh;
  mMaxBranches = maxbranches;
  mDetector   = det;
  mBranch     = new StEstBranch*[mMaxBranches*mMaxShare];
  for(i=0;i<mMaxBranches*mMaxShare;i++)
    mBranch[i]=NULL;
  mNShare     = 0;
  mNBranch    = 0;
  mDebugLevel = 0;
  mFlag       = 0;
  if(!mBranch)
    gMessMgr->Error()<<"StEstHit::StEstHit mBrach=NULL"<<endm;
}

StEstHit::~StEstHit() {
  delete [] mBranch;
  delete mXL;
  delete mXG;
  delete mEL;
  delete mEG;
};

void StEstHit::DetachFromWafer() {
  mDetector->RemoveHit(this);
  mDetector=NULL;};

int StEstHit::JoinBranch(StEstBranch *br, StEstTrack *tr) {
  // method to add a hit to a branch and the branch to the hit branch list. 
  // two conditions have to be fulfilled :
  // the number of branches possessing the hit (mNBranch) should be smaller than mMaxBranches
  // the number of tracks possessing the hit (mNShare) should be smaller than mMaxShare
  // OkShare is set to one if the branch is from a new track and hit sharing is still allowed
  // OkBranch is set to one if the branch can be added (branch from this track already sharing the hit
  // and the maximum number of branch is not reached.)

  int OkShare=0,OkBranch=0,i;
  int Ok;

  if(mDebugLevel>0) {
    gMessMgr->Info()<<"StEstHit::JoinBranch ****START****"<<endm;
    gMessMgr->Info()<<" trying to join the branch br="<<br<<" br->GetNHits()="<<br->GetNHits()<<endm;
    gMessMgr->Info()<<" to the hit : "<<this->GetId()
		    <<" mNShare= "<<mNShare<<" mMaxShare= "<<mMaxShare
		    <<" mNBranch= "<<mNBranch<<" mMaxBranches= "<<mMaxBranches<<endm;
    gMessMgr->Info()<<" branches already using the hit : "<<endm;
    for (i=0;i<mNBranch;i++) gMessMgr->Info()<<mBranch[i]<<endm;
  }
  
  if (br->CheckAvailability()) {
    if(mDebugLevel>0) gMessMgr->Info()<<" BRANCH AVAILABLE "<<endm; 
    if(mNBranch>0 && mNBranch<mMaxBranches*mMaxShare) { 
      // the hit is already used but can there is some free space in the list.
      // we have to check if the track branch already shares the hit or can share it.
      Ok=0;
      for (i=0;i<mNBranch;i++) {
	if (mBranch[i]->GetTrack()==tr) Ok=1; 
      }
      if (Ok==1||mNShare<mMaxShare) {
	if (mBranch[mNBranch-1]->GetChiSq() <= br->GetChiSq()) {
	  // the branch has the worst chisq so we put it at the end of the list
	  mBranch[mNBranch] = br;
	  OkBranch =1;
	  if (Ok==0) OkShare=1;
	}
	else {
	  // the branch as a chisq better than the last we have to find the correct place in the list.
	  for (int i=mNBranch-1;i>=0;i--) {
	    if(mDebugLevel>1) {
	      gMessMgr->Info()<<" i="<< i<<" mBranch[i]="<<mBranch[i]
			      <<" mBranch[i]->GetNHits()="<<mBranch[i]->GetNHits()<<endm;
	      gMessMgr->Info()<<" mBranch[i]->GetChiSq()= "<<mBranch[i]->GetChiSq()<<endm;
	    }
	    if (mBranch[i]->GetChiSq() > br->GetChiSq()) { //one cell up
	      mBranch[i+1] = mBranch[i];
	      mBranch[i]   = br;
	      OkBranch=1;
	      if (Ok==0) OkShare=1;
	    }
	    else break; //branch in good place     
	  }
	}
      }
    }
    if (mNShare ==0)  { 
      mBranch[0] = br;
      OkBranch=1;
      OkShare=1;
    }
    if(mDebugLevel>0) gMessMgr->Info()<<"OkShare="<<OkShare<<endm;
    if (OkShare==1) {
      mNShare++;
    }
    if (OkBranch==1) {
      if(mDebugLevel>0) gMessMgr->Info()<<"mBranch[mNShare]="<<mBranch[mNShare]<<endm;
      mNBranch++;
    }
  } //if (br->CheckAvailability())
  else
    gMessMgr->Info()<<" BRANCH NOT AVALAIBLE "<<endm;

  if(mDebugLevel>0) {
    gMessMgr->Info()<<" JoinBranch : branches using the hit : "<<this->GetId()<<" "<<endm;
    for (i=0;i<mNBranch;i++) gMessMgr->Info()<<mBranch[i]<<endm;
  }
  if(mDebugLevel>0)
    gMessMgr->Info()<<"StEstHit::JoinBranch ****STOP**** OkBranch = "<<OkBranch<<" OkShare = "<<OkShare<<endm;
  return OkBranch;
};


void StEstHit::LeaveBranch(StEstBranch *br) {  
  // method used to remove a branch br from the branch list of the current hit.
  // the variable i is used to identified the branch from the list.
  // once found in the list, we check that an other branch from the same track is also 
  // in the list in order to prevent mNShare to be decrease by unity.
  
//   StEstBranch *brToKill;
  long int i,j, ok=0;
  int SharingSame;
  if(mDebugLevel>0) {
    gMessMgr->Info()<<"****StEstHit::LeaveBranch**** START"<<endm;
    gMessMgr->Info()<<" in hit : "<<this<<" id : "<<this->GetId()<<endm;
    gMessMgr->Info()<<" mNBranch : "<<mNBranch<<" mNShare : "<<mNShare<<endm;
    gMessMgr->Info()<<" br=" <<br<<endm;
    gMessMgr->Info()<<" br->GetNHits()="<<br->GetNHits()<<endm;
  } 

  if (mNBranch==0) 
    gMessMgr->Error()<<"StEstHit::LeaveBranch mNShare=0"<<endm;
  else {

    for (i=0;i<mNBranch;i++) {
      if(mDebugLevel>0) gMessMgr->Info()<<" i="<<i<<"  mBranch[i]="<<mBranch[i]<<endm;
      if (mBranch[i] == br) { 
	ok = 1;
	SharingSame=0;
	for (j=0;j<mNBranch;j++)
	  if (mBranch[j]->GetTrack()==mBranch[i]->GetTrack()&&j!=i) SharingSame=1;
	
	if (mDebugLevel>0) gMessMgr->Info()<<"SharingSame= "<<SharingSame<<endm;
	if (mDebugLevel>0) gMessMgr->Info()<<"  mBranch["<<i<<"]==br<--- this branch is being removed"<<endm;
// 	brToKill = mBranch[i]; // We point to the branch we want to leave and we will delete it after
	for (j=i;j<mNBranch-1;j++) { mBranch[j] = mBranch[j+1]; //cout << " j " << j << endl;
	}
	break;
      }
    }
    if(ok) {
      //cout << " to delete mBranch["<<mNBranch-1<<"] and mNBranch = "<< mNBranch <<endl;

      mNBranch--;
      if (SharingSame==0) mNShare--;
      if (mDebugLevel>0) gMessMgr->Info()<<" the branch is not in the hit list anymore : mNBranch : "<<mNBranch<<" mNShare : "<<mNShare<<endm;
      br->RemoveHit(this);
    }
    if(mDebugLevel>0) gMessMgr->Info()<<"the hit is removed from branch "<<br<<" NHits="<<br->GetNHits()<<endm;
  }

  if (ok==0)
    gMessMgr->Error()<<"ERROR!!! StEstHit::LeaveBranch branch="<<br<<" wasn't found in hit="<<this<<" mId="<<mId<<endm;
  for (i=0;i<br->GetNHits();i++) {
    if(mDebugLevel>0) gMessMgr->Info()<<"  i="<<i<<endm;
    if (br->GetHit(i) == this) {
      if(mDebugLevel>0) gMessMgr->Info()<<"  br->GetHit("<<i<<") == this"<<endm;
      br->RemoveHit(i);
      break;
    }
  }

//   if(ok) delete brToKill;

  if(mDebugLevel>0) gMessMgr->Info()<< "****StEstHit::LeaveBranch**** STOP"<<endm; 
}










