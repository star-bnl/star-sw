/***************************************************************************
 *
 * $Id: StEstTrack.cc,v 1.1 2000/12/07 11:14:27 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the StEstTrack class
 *
 ***************************************************************************
 *
 * $Log: StEstTrack.cc,v $
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstMaker/Infrastructure/StEstTrack.hh"

StEstTrack::StEstTrack(long maxbranch, StEstTPCTrack *tr=NULL) {
  mMaxBranch = maxbranch;
  mTPCTrack  = tr;
  mNBranch   = 0;
  mBranch    = new StEstBranch*[mMaxBranch];
  mIdealPattern=0;
  mIdealNHits=0;
  mFindablePattern=0;
  mFindableNHits=0;
  mFlag      = 0;
  mDone      = 0;
  mHelix = NULL;
  
  
  if(mBranch==NULL)
    cerr << "ERROR StEstTrack::StEstTrack mBranch==NULL" << endl;
  //  mMaxTPCHits = 40;
};

int StEstTrack::RemoveBranch(long int nbr) {
  long i,j;

  if (mNBranch<nbr) {
    cerr << "ERROR  StEstTrack::RemoveBranch nbr>mNBranch" << endl;
    return 1; 
  }
  if(nbr<0) {
    cerr << "ERROR  StEstTrack::RemoveBranch nbr<0" << endl;
    return 1; 
  }
  if (mNBranch>0) {
    for (j=0;j<mBranch[nbr]->GetNHits();j++) mBranch[nbr]->GetHit(j)->LeaveBranch(mBranch[nbr]);
    for (i=nbr; i<mNBranch-1;i++) mBranch[i] = mBranch[i+1];
    mBranch[mNBranch-1] = NULL;
    mNBranch--;
    return 0; 
  }
  else {
    cerr << "ERROR  StEstTrack::RemoveBranch mNBranch<=0" << endl;
    return 1; 
  }
};
