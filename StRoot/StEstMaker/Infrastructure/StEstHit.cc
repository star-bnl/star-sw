/***************************************************************************
 *
 * $Id: StEstHit.cc,v 1.1 2000/12/07 11:14:27 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the StEstHit class
 *
 ***************************************************************************
 *
 * $Log: StEstHit.cc,v $
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstMaker/Infrastructure/StEstHit.hh"

StEstHit::StEstHit(long id, StThreeVectorD *xg, StThreeVectorD *xl, long maxbranches, long maxsh, StEstWafer *det) {
  int i;
  mId = id;
  mXL = xl;
  mXG = xg;
  mMaxShare   = maxsh;
  mMaxBranches = maxbranches;
  mDetector   = det;
  mBranch     = new StEstBranch*[mMaxBranches*mMaxShare];
  for(i=0;i<=mMaxBranches*mMaxShare;i++)
    mBranch[i]=NULL;
  mNShare     = 0;
  mNBranch    = 0;
  mDebugLevel = 0;
  mFlag       = 0;
  mEvalTrack  = 0;
  //  if (mId==6647) mDebugLevel=1;
  if(!mBranch)
    cerr << "ERROR!!! StEstHit::StEstHit mBrach=NULL" << endl;
}


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
    cout<<"StEstHit::JoinBranch ****START****"<<endl;
    cout<<" trying to join the branch br=" <<br<<" br->GetNHits()="<<br->GetNHits()<<endl;
    cout<<" to the hit : "<<this->GetId();
    cout<<" mNShare= "<<mNShare<<" mMaxShare= "<<mMaxShare;
    cout<<" mNBranch= "<<mNBranch<<" mMaxBranches= "<<mMaxBranches<<endl;
    cout<<" branches already using the hit : ";
    for (i=0;i<mNBranch;i++) cout<<mBranch[i]<<" ";
    cout<<endl;
  }
  
  if (br->CheckAvailability()) {
    if(mDebugLevel>0) cout << " BRANCH AVALAIBLE "<<endl; 
    //    if (mNShare>0 
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
	      cout << " i=" << i<< " mBranch[i]="<<mBranch[i];
	      cout <<" mBranch[i]->GetNHits()="<< mBranch[i]->GetNHits()<<endl;
	      cout << " mBranch[i]->GetChiSq() = "<<mBranch[i]->GetChiSq()<<endl;
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
    if(mDebugLevel>0) cout << "OkShare="<<OkShare<<endl;
    if (OkShare==1) {
      mNShare++;
    }
    if (OkBranch==1) {
      if(mDebugLevel>0) cout << " mBranch[mNShare]=" << mBranch[mNShare] <<endl;
      mNBranch++;
    }
  } //if (br->CheckAvailability())
  else
    cout << " BRANCH NOT AVALAIBLE "<<endl;

  if(mDebugLevel>0) {
    cout<<" JoinBranch : branches using the hit : "<<this->GetId()<<" ";
    for (i=0;i<mNBranch;i++) cout<<mBranch[i]<<" ";
    cout<<endl;
  }
  if(mDebugLevel>0)
    cout << "StEstHit::JoinBranch ****STOP**** OkBranch = "<<OkBranch<<" OkShare = "<<OkShare<<endl;
  return OkBranch;
};


void StEstHit::LeaveBranch(StEstBranch *br) {  
  // method used to remove a branch br from the branch list of the current hit.
  // the variable i is used to identified the branch from the list.
  // once found in the list, we check that an other branch from the same track is also 
  // in the list in order to prevent mNShare to be decrease by unity.
  
  long int i,j, ok=0;
  int SharingSame;
  if(mDebugLevel>0) {
    cout << "****StEstHit::LeaveBranch**** START"<<endl;
    cout << " in hit : "<<this<<" id : "<<this->GetId()<<endl;
    cout << " mNBranch : "<<mNBranch<<" mNShare : "<<mNShare<<endl;
    cout << " br=" <<br<<endl;
    cout << " br->GetNHits()=" << br->GetNHits() << endl;
  } 
  if (mNBranch==0) 
    cout <<"ERROR!!! StEstHit::LeaveBranch mNShare=0"<<endl;
  else {

    for (i=0;i<mNBranch;i++) {
      if(mDebugLevel>0) cout << "  i=" << i<<"   mBranch[i]="<<mBranch[i]<<endl ; //mBranch<<endl;
      if (mBranch[i] == br) { 
	ok = 1;
	SharingSame=0;
	for (j=0;j<mNBranch;j++) if (mBranch[j]->GetTrack()==mBranch[i]->GetTrack()&&j!=i) SharingSame=1;
	if (mDebugLevel>0) cout<<"SharingSame= "<<SharingSame<<endl;
	if(mDebugLevel>0) cout << "  mBranch["<<i<<"] == br <--- this branch is being removed"<<endl;
	for (j=i;j<mNBranch-1;j++) mBranch[j] = mBranch[j+1];
	break;
      }
    }
    if(ok) {
      mNBranch--;
      if (SharingSame==0) mNShare--;
      if (mDebugLevel>0) cout<<" the branch is not in the hit list anymore : mNBranch : "<<mNBranch<<" mNShare : "<<mNShare<<endl;
      br->RemoveHit(this);
    }
    if(mDebugLevel>0) cout << "the hit is removed from branch " << br << " NHits="<< br->GetNHits() << endl;
  }

  if (ok==0)
    cout << "ERROR!!! StEstHit::LeaveBranch branch="<<br<<" wasn't found in hit="<<this<<" mId="<<mId<<endl;
  for (i=0;i<br->GetNHits();i++) {
    if(mDebugLevel>0) cout << "  i=" << i << endl;
    if (br->GetHit(i) == this) {
      if(mDebugLevel>0) cout << "  br->GetHit("<<i<<") == this"<<endl;
      br->RemoveHit(i);
      break;
    }
  } 
  
  if(mDebugLevel>0) cout << "****StEstHit::LeaveBranch**** STOP"<<endl; 
}










