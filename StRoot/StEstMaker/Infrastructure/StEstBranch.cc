/***************************************************************************
 *
 * $Id: StEstBranch.cc,v 1.1 2000/12/07 11:14:27 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the StEstBranch class
 *
 ***************************************************************************
 *
 * $Log: StEstBranch.cc,v $
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstMaker/Infrastructure/StEstBranch.hh"

StEstBranch::StEstBranch(StEstTrack *tr, 
			     long int maxhits, 
			     long int nf, 
			     long int nh, 
			     StEstHit **hit, 
			     double *dist, 
			     int isgood) {

  int i;

  mDebugLevel = 0;
  mMaxHits    = maxhits;
  mNHits      = nh;
  mNFit       = nf;
  mLastLay    = 4; 
  mIsGood     = isgood;

  if (mMaxHits==0) cout << "ERROR StEstBranch::StEstBranch  mMaxHits=0"<<endl;
  if (mDebugLevel>0) cout << "StEstBranch::StEstBranch ***START***" << endl;
  if (mNHits>mMaxHits)
    cout << "ERROR StEstBranch::StEstBranch mNHits>mMaxHits"<<endl;

  mHits = new StEstHit*[mMaxHits];
  mDist = new double[mMaxHits];

  if (mHits==NULL) 
    cout << "ERROR StEstBranch::StEstBranch not enough memory mHits=NULL" << endl;

  if (hit!=NULL) {
    if (mDebugLevel>0) 
      cout << "mNHits = " << mNHits <<endl;

    for (i=0;i<mNHits;i++) {

      if (mDebugLevel>0)
	cout << "  i = " << i << "  hit[i] = "<<hit[i]<<endl;

      mHits[i] = hit[i];
      mDist[i] = dist[i];
      if (mDebugLevel>0) {
	cout << "  StEstBranch::StEstBranch mHits["<<i<<"]->GetNBranch()="<<endl;
	cout <<mHits[i]->GetNBranch()<<endl;
      }
      if (mHits[i]->GetDebugLevel()>0) {
	cout << "  StEstBranch before JoinBranch : hit_id,NBranch,MaxBranches,NShare,MaxShare ";
	cout <<mHits[i]->GetId()<<" "<<mHits[i]->GetNBranch()<<" "<<mHits[i]->GetMaxBranches()<<" "<<mHits[i]->GetNShare()<<" "<<mHits[i]->GetMaxShare()<<endl;
      }
      mHits[i]->JoinBranch(this,tr);
      if (mHits[i]->GetDebugLevel()>0) {
	cout << "  StEstBranch after JoinBranch : hit_id,NBranch,MaxBranches,NShare,MaxShare ";
	cout <<mHits[i]->GetId()<<" "<<mHits[i]->GetNBranch()<<" "<<mHits[i]->GetMaxBranches()<<" "<<mHits[i]->GetNShare()<<" "<<mHits[i]->GetMaxShare()<<endl;
      }
      if (mDebugLevel>0) cout << "  mLastLay = ";
      mLastLay=mHits[i]->GetWafer()->GetLayer();
      if (mDebugLevel>0) cout << mLastLay << endl;
      if (mDebugLevel>0)
	cout << "  StEstBranch::StEstBranch mHits["<<i<<"]->GetNBranch()="<< mHits[i]->GetNBranch()<<endl;
    }
  }

  if (tr!=NULL) {  
    mTrack = tr;
    if(tr->AddBranch(this)==1)
      cerr << "ERROR!!! StEstBranch::StEstBranch tr->AddBranch(this)==1 too many branches for track";
  }

  mHelix = NULL;

  if (mDebugLevel>0) cout << "StEstBranch::StEstBranch ***STOP***" << endl;
}

  
StEstBranch::~StEstBranch() {
  if (mDebugLevel>0) cout << endl<<"StEstBranch::~StEstBranch ***START***" << endl;
  for (int i=mNHits-1;i>=0;i--) {
    mHits[i]->LeaveBranch(this);
  }
  if (mTrack!=NULL) LeaveTrack();
  if(mHelix!=NULL) 
    delete mHelix;
  //delete [] mHits; dont even think about it
  if (mDebugLevel>0) cout << "StEstBranch::~StEstBranch ***STOP***" << endl;
};

StEstBranch* StEstBranch::Duplicate() {
  // method to copy a branch. Most of the data member are assigned by 
  // calling the branch constructor. The chisqs and the fitstatus are
  // assigned here.
    
  StEstBranch *br = new StEstBranch(mTrack, mMaxHits, mNFit, mNHits, mHits, mDist, mIsGood);
  if(br==NULL) cerr << "ERROR StEstBranch::Duplicate br=NULL" << endl;
  else {
    br->SetChiSq(this->GetChiSq());
    br->SetChiSqCir(this->GetChiSqCir());
    br->SetChiSqLin(this->GetChiSqLin());
    br->SetChiSqLin(this->GetChiSqLin());
    br->mLastFitStatus=this->mLastFitStatus;
    br->SetIsGoodOld(this->GetIsGoodOld());
    if (this->GetHelix()) {
      StHelix *helix = new StHelix(*this->GetHelix());
      br->SetHelix(helix);
    }
  }
  return br;
}
  
void StEstBranch::LeaveTrack() {
  if (mDebugLevel>0) cout <<"StEstBranch::LeaveTrack ***START***" <<endl;
  long int i;
  for (i=0;i<mTrack->GetNBranches();i++) {
    if (mTrack->GetBranch(i) == this) {
      mTrack->RemoveBranch(i);
      mTrack = NULL;
      break;
    }
  }	
  if (mDebugLevel>0) cout <<"StEstBranch::LeaveTrack ***STOP***" <<endl;
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
  if (mDebugLevel>0) cout <<"StEstBranch::JoinTrack ***START****"<<endl;

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
	  if (mDebugLevel>0) cout <<"StEstBranch::JoinTrack ***STOP*** 1"<<endl;
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
  if (mDebugLevel>0) cout <<"StEstBranch::JoinTrack ***STOP*** 2"<<endl;
  return 0;
}

int StEstBranch::AddHit(StEstHit *hit, double dist) {
  if (mDebugLevel>0) cout <<"StEstBranch::AddHit ***START***"<<endl;
  if(mNHits>=mMaxHits){
    cout<<"ERROR StEstBranch::AddHit mNHits ("<<mNHits<<") >=mMaxHits ("<<mMaxHits<<")"<<endl;
    return 1;
  }
  mHits[mNHits] = hit; 
  mDist[mNHits] = dist;
  mNHits++;

  if (mDebugLevel>0) cout << "  ADD HIT check mNHits: "<<mNHits<<endl;
  for (int i=0; i<mNHits; i++) {
    if (mDebugLevel>0) cout << "  " << i << "\t" <<mHits[i];
    if (mHits[i] == NULL && mDebugLevel>0) cout << "<-- HIT NULL!"<<endl;
    else if (mDebugLevel>0) cout <<endl;
  }
  mLastLay=hit->GetWafer()->GetLayer();
  if (mDebugLevel>0)
    cout << " StEstBranch::AddHit mLastLay="<<mLastLay<<endl;
  if (mDebugLevel>0) cout <<"StEstBranch::AddHit ***STOP***"<<endl;
  return 0;
}

int StEstBranch::RemoveHit(long int nr) {
  

  int i;
  if (mDebugLevel>0) cout <<"StEstBranch:RemoveHit v1 ***START***"<<endl;

  if(nr>=mMaxHits) {
    cerr << "ERROR StEstBranch::RemoveHit  nr>=mMaxHits"<<endl;
    return  1;
  }
  if(nr>=mNHits) {
    cerr << "ERROR StEstBranch::RemoveHit  nr>=mNHits"<<endl;
    return  1;
  }
  if(nr<0) {
    cerr << "ERROR StEstBranch::RemoveHit  nr<0" << endl;
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

    if (mDebugLevel>0) cout << "  mNHits: "<<mNHits<<endl;
    for (i=0; i<mNHits; i++) {
      if (mDebugLevel>0) cout << "  " << i << "\t"<<mHits[i];
      if (mHits[i] == NULL) {
	if (mDebugLevel>0) cout << "<-- HIT NULL!" <<endl;
	cerr << "ERROR!!! StEstBranch::RemoveHit HIT NULL!"<<endl;
      }
      else if (mDebugLevel>0) cout <<endl;
    }
    if (mDebugLevel>0) cout <<"StEstBranch:RemoveHit v1 ***STOP***"<<endl;
    return 0;
  }
  else {
    if (mDebugLevel>0) cout <<"StEstBranch:RemoveHit v1 ***STOP***"<<endl;
    return 1;
  }
}


int StEstBranch::RemoveHit(StEstHit* hit) {
  
  if (mDebugLevel>0) cout <<"StEstBranch:RemoveHit v2 ***START***"<<endl;
  //  cout <<"StEstBranch:RemoveHit v2 ***START***"<<endl;
  //  cout <<" mNHits : "<<mNHits<<endl;
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
      //      cout <<" mNHits : "<<mNHits<<endl;
      if (mDebugLevel>0) cout << "  mNHits: "<<mNHits<<endl;
      for (i=0; i<mNHits; i++) {
	if (mDebugLevel>0) cout <<i<<"\t"<<mHits[i];
	if (mHits[i] == NULL) cout << "<-- HIT NULL!"<<endl;
	else if (mDebugLevel>0) cout <<endl;
      }
      //      cout <<"StEstBranch:RemoveHit v2 ***STOP*** 1 "<<endl;
      if (mDebugLevel>0) cout <<"StEstBranch:RemoveHit v2 ***STOP*** 1 "<<endl;
     return 0;
    }
    else {
      cout <<"ERROR!!! RemoveHit "<<hit<<"  - no such hit in the branch"<<endl;
      cout <<"StEstBranch:RemoveHit v2 ***STOP*** 2 "<<endl;
      return 1;
    }
  }
  else {
    cout << "ERROR!!! StEstBranch::RemoveHit mNhits<=0" << endl;
    cout <<"StEstBranch:RemoveHit v2 ***STOP*** 3 "<<endl;
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






