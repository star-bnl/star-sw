/***************************************************************************
 *
 * $Id: StEstBranch.cc,v 1.6 2004/11/12 23:20:26 caines Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the StEstBranch class
 *
 ***************************************************************************
 *
 * $Log: StEstBranch.cc,v $
 * Revision 1.6  2004/11/12 23:20:26  caines
 * Initialization fixes
 *
 * Revision 1.5  2001/07/15 20:31:33  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.4  2001/02/23 13:01:10  lmartin
 * cout replaced by gMessMgr.
 *
 * Revision 1.3  2001/01/26 10:16:45  lmartin
 * Minor changes. Unused mLastLay data member removed. Short description of the data members added.
 *
 * Revision 1.2  2001/01/25 18:13:27  lmartin
 * Minor changes in the GetHit and SetHelix methods
 *
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StMessMgr.h"
#include "StEstBranch.hh"
#include "StEstWafer.hh"
#include "StEstHit.hh"
#include "StEstTrack.hh"
#include "StHelix.hh"

StEstBranch::StEstBranch(StEstTrack *tr, 
			     long maxhits, 
			     long nf, 
			     long nh, 
			     StEstHit **hit, 
			     double *dist, 
			     int isgood) {

  int i;

  // zap all to zero to be sure
  memset(MemBegin,0,MemEnd-MemBegin);

  mDebugLevel = 0;
  mMaxHits    = maxhits;
  mNHits      = nh;
  mNFit       = nf;
  mIsGood     = isgood;

  if (mMaxHits==0) gMessMgr->Error()<<"ERROR StEstBranch::StEstBranch  mMaxHits=0"<<endm;
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::StEstBranch ***START***"<<endm;
  if (mNHits>mMaxHits)
    gMessMgr->Error()<<"ERROR StEstBranch::StEstBranch mNHits>mMaxHits"<<endm;

  mHits = new StEstHit*[mMaxHits];
  mDist = new double[mMaxHits];

  if (mHits==NULL) 
    gMessMgr->Error()<<"ERROR StEstBranch::StEstBranch not enough memory mHits=NULL"<<endm;

  if (hit!=NULL) {
    if (mDebugLevel>0) 
      gMessMgr->Info()<<"mNHits = "<<mNHits<<endm;

    for (i=0;i<mNHits;i++) {

      if (mDebugLevel>0)
	gMessMgr->Info()<<"  i = "<<i<<"  hit[i] = "<<hit[i]<<endm;

      mHits[i] = hit[i];
      mDist[i] = dist[i];
      if (mDebugLevel>0)
	gMessMgr->Info()<<"StEstBranch::StEstBranch mHits["<<i<<"]->GetNBranch()="
			<<mHits[i]->GetNBranch()<<endm;
      if (mHits[i]->GetDebugLevel()>0)
	gMessMgr->Info()<<"StEstBranch before JoinBranch : hit_id,NBranch,MaxBranches,NShare,MaxShare "
			<<mHits[i]->GetId()<<" "<<mHits[i]->GetNBranch()<<" "<<mHits[i]->GetMaxBranches()
			<<" "<<mHits[i]->GetNShare()<<" "<<mHits[i]->GetMaxShare()<<endm;
      
      mHits[i]->JoinBranch(this,tr);

      if (mHits[i]->GetDebugLevel()>0)
	gMessMgr->Info()<<"StEstBranch before JoinBranch : hit_id,NBranch,MaxBranches,NShare,MaxShare "
			<<mHits[i]->GetId()<<" "<<mHits[i]->GetNBranch()<<" "<<mHits[i]->GetMaxBranches()
			<<" "<<mHits[i]->GetNShare()<<" "<<mHits[i]->GetMaxShare()<<endm;
      if (mDebugLevel>0)
	gMessMgr->Info()<<"StEstBranch::StEstBranch mHits["<<i<<"]->GetNBranch()="
			<< mHits[i]->GetNBranch()<<endm;
    }
  }

  if (tr!=NULL) {  
    mTrack = tr;
    if(tr->AddBranch(this)==1)
      gMessMgr->Error()<<"StEstBranch::StEstBranch tr->AddBranch(this)==1 too many branches for track"<<endm;
  }

  mHelix = NULL;

  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::StEstBranch ***STOP***"<<endm;
}

  
StEstBranch::~StEstBranch() {
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::~StEstBranch ***START***"<<endm;
  for (int i=mNHits-1;i>=0;i--) {
    mHits[i]->LeaveBranch(this);
  }
  if(mHelix!=NULL) 
    delete mHelix;
  delete [] mDist;
  delete [] mHits;
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::~StEstBranch ***STOP***"<<endm;
};


StEstHit* StEstBranch::GetHit(long nr) {
  if(nr>=mNHits) {
    gMessMgr->Error()<<"ERROR StEstBranch::GetHit  nr>=mNHits"<<endm;
    return NULL;
  }
  if(nr>=mMaxHits) {
    gMessMgr->Error()<<"ERROR StEstBranch::GetHit  nr>=mMaxHits"<<endm;
    gMessMgr->Error()<<"      nr= "<<nr<<" mNHits= "<<mNHits<<" mMaxHits= "<<mMaxHits<<endm;
    return NULL;
  }
  if(nr<0) {
    gMessMgr->Error()<<"ERROR StEstBranch::GetHit  nr<0"<<endm;
    return NULL;
  }
  return mHits[nr];
}

void StEstBranch::SetHelix(StHelix *hel) {
  if (mHelix!=NULL)
    delete mHelix;
  mHelix=hel;
};
StEstBranch* StEstBranch::Duplicate() {
  // method to copy a branch. Most of the data member are assigned by 
  // calling the branch constructor. The chisqs and the fitstatus are
  // assigned here.
    
  StEstBranch *br = new StEstBranch(mTrack, mMaxHits, mNFit, mNHits, mHits, mDist, mIsGood);
  if(br==NULL) gMessMgr->Error()<<"ERROR StEstBranch::Duplicate br=NULL"<<endm;
  else {
    br->SetChiSq(this->GetChiSq());
    br->SetChiSqCir(this->GetChiSqCir());
    br->SetChiSqLin(this->GetChiSqLin());
    br->SetChiSqLin(this->GetChiSqLin());
    br->mLastFitStatus=this->mLastFitStatus;
    br->SetIsGoodOld(this->GetIsGoodOld());
    if (this->GetHelix()) {
//       StHelix *helix = new StHelix((StHelix)(*(this->GetHelix())));
      const StHelix helixTemp = (StHelix)(*(this->GetHelix()));
      StHelix *helix = new StHelix(helixTemp);
      br->SetHelix(helix);
    }
  }
  return br;
}
  
void StEstBranch::LeaveTrack() {
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::LeaveTrack ***START***" <<endm;
  long i;
  for (i=0;i<mTrack->GetNBranches();i++) {
    if (mTrack->GetBranch(i) == this) {
      gMessMgr->Info()<<"LeaveTrack this="<<this<<" i="<<i<<endm;
      mTrack->RemoveBranch(i);
      mTrack = NULL;
      break;
    }
  }	
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::LeaveTrack ***STOP***"<<endm;
}


int StEstBranch::JoinTrack(StEstTrack *tr, int IsIdeal) {
  // method to add a branch to a given track (IsIdeal=0) or
  // the ideal branch to its track (IsIdeal=1).
  // If the branch is the ideal branch, the AddIdealBranch is called
  // In the case of a branch formed during the tracking part,
  // the branch is simply added to the track branch list if a cell is available
  // otherwise the branch is added at the right location in the sorted (on chisq)
  // list of branches.

  long i;
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::JoinTrack ***START****"<<endm;

  if (IsIdeal==0) {
    if (!tr->CheckAvailability()) {
      for (i=tr->GetNBranches();i>=0;i--) {
	if (tr->GetBranch(i)->mChisq > mChisq) {
	  tr->SetBranch(i+1,tr->GetBranch(i));
	  tr->SetBranch(i,this);
	  mTrack=tr;
	  // ok=1;
	}
	else {
	  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::JoinTrack ***STOP*** 1"<<endm;
	  return 1;
	}
      }
    }
    else {
      tr->AddBranch(this);
      mTrack=tr;
    }     
  }
  else {
    if(IsIdeal==1) 
      tr->AddIdealBranch(this);
    else
      tr->AddFindableBranch(this);
    mTrack=tr;
  }
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::JoinTrack ***STOP*** 2"<<endm;
  return 0;
}

int StEstBranch::AddHit(StEstHit *hit, double dist) {
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::AddHit ***START***"<<endm;
  if(mNHits>=mMaxHits){
    gMessMgr->Error()<<"ERROR StEstBranch::AddHit mNHits ("<<mNHits<<") >=mMaxHits ("<<mMaxHits<<")"<<endm;
    return 1;
  }
  mHits[mNHits] = hit; 
  mDist[mNHits] = dist;
  mNHits++;

  if (mDebugLevel>0) gMessMgr->Info()<<"ADD HIT check mNHits: "<<mNHits<<endm;
  for (int i=0; i<mNHits; i++) {
    if (mHits[i] == NULL && mDebugLevel>0) gMessMgr->Info()<<"  "<<i<<"\t"<<mHits[i]<<"<-- HIT NULL!"<<endm;
    else if (mDebugLevel>0) gMessMgr->Info()<<"  "<<i<<"\t"<<mHits[i]<<endm;
  }
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch::AddHit ***STOP***"<<endm;
  return 0;
}

int StEstBranch::RemoveHit(long nr) {
  

  int i;
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch:RemoveHit v1 ***START***"<<endm;

  if(nr>=mMaxHits) {
    gMessMgr->Error()<<"StEstBranch::RemoveHit  nr>=mMaxHits"<<endm;
    return  1;
  }
  if(nr>=mNHits) {
    gMessMgr->Error()<<"StEstBranch::RemoveHit  nr>=mNHits"<<endm;
    return  1;
  }
  if(nr<0) {
    gMessMgr->Error()<<"StEstBranch::RemoveHit  nr<0"<<endm;
    return  1;
  }
  if (mNHits>0) { //?
    for (i=nr;i<mNHits-1;i++) {
      mHits[i] = mHits[i+1];
      mDist[i] = mDist[i+1];
    }
    mHits[mNHits-1] = NULL;
    mDist[mNHits-1] = 0;
    mNHits--;  

    for (i=0; i<mNHits; i++)
      if (mHits[i] == NULL)
	gMessMgr->Error()<<"StEstBranch::RemoveHit HIT "<<i<<" of "<<mNHits<<"  IS NULL!"<<endm;
    
    if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch:RemoveHit v1 ***STOP***"<<endm;
    return 0;
  }
  else {
    if (mDebugLevel>0) gMessMgr->Error()<<"StEstBranch:RemoveHit v1 ***STOP*** mNHits=0"<<endm;
    return 1;
  }
}


int StEstBranch::RemoveHit(StEstHit* hit) {
  
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch:RemoveHit v2 ***START***"<<endm;
  if (mNHits>0) { 
    int i=0;
    while(mHits[i] != hit && i<mNHits) i++;
    if (mHits[i] == hit) {
      for (;i<mNHits-1;i++) {
	mHits[i] = mHits[i+1];
	mDist[i] = mDist[i+1];
      }
      mHits[mNHits-1] = NULL;
      mDist[mNHits-1] = 0;
      mNHits--;

      for (i=0; i<mNHits; i++)
	if (mHits[i] == NULL)
	  gMessMgr->Error()<<"StEstBranch::RemoveHit HIT "<<i<<" of "<<mNHits<<"  IS NULL!"<<endm;
      
      if (mDebugLevel>0) gMessMgr->Info()<<"StEstBranch:RemoveHit v2 ***STOP*** 1"<<endm;
     return 0;
    }
    else {
      gMessMgr->Error()<<"StEstBranch:RemoveHit v2 ***STOP*** 2 : hit"<<hit<<" not in the branch"<<endm;
      return 1;
    }
  }
  else {
    gMessMgr->Error()<<"StEstBranch:RemoveHit v2 ***STOP*** 3 mNHits<=0"<<endm;
    return 1;
  }
}

double StEstBranch::GetDist(StEstHit* hit) {
  int i=0;
  double dist=-1;
  for(i=0; i<mNHits; i++) 
    if(hit==mHits[i]){
      dist=mDist[i];
      break;
    }  
  return dist;
}






