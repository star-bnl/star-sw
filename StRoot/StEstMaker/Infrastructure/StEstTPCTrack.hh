/***************************************************************************
 *
 * $Id: StEstTPCTrack.hh,v 1.5 2003/09/18 22:47:50 caines Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstTPCTrack Class
 *
 ***************************************************************************
 *
 * $Log: StEstTPCTrack.hh,v $
 * Revision 1.5  2003/09/18 22:47:50  caines
 * Fix initialization ofr new RH system
 *
 * Revision 1.4  2001/02/23 14:48:33  lmartin
 * cout replaced by gMessMgr.
 *
 * Revision 1.3  2001/02/21 23:50:22  caines
 * Add some more info to SVT track for kalman fitting initial guess
 *
 * Revision 1.2  2001/01/25 18:17:56  lmartin
 * Delete of mR and mdR arrays added to the destructor.
 *
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstTPCTrack_hh
#define StEstTPCTrack_hh
#include "StMessMgr.h"
#include "StMaker.h"
#include "StThreeVector.hh"
#include "StHelix.hh"
#include "StThreeVectorD.hh"

class StEstTracker;

class StEstTPCTrack {
  
protected:
  
  StThreeVectorD** mR;     // hit table
  StThreeVectorD** mdR;    // hit error table
  int* mHitFlag; // hit flag status
  StHelix* mHelix;
  long int *mHitId;
  long int mId;
  long int mMcId;
  long int mPid; 
  long int mParentPid; 
  long int mParentMcId; 
  long int mVid; 
  long int mNHits;
  long int mMaxHits;
  long int *mHitIndex;
  int mType; // type of track (1=primary, 2 = secondary)
  int    mFlag;
  int    mFlagSP;                 // flag for SuperPasses
  int*   row;
  double mPt;
  double mChiSq;
  double mChiSqCir;
  double mChiSqLin;
  long int mNFit;
  double mr; // r of first hit

  void Sort(long left, long right)  {
    //*-*-*-*-*-*-*-*-*-*    Quick sorter    *-*-*-*-*-*-*-*-*
    //*-*     Srow               ============
    //
    //    qsort method, creates an index ind for table t1, 
    //    from element left to right
    long i, last;

    if (left>=right) return;    
    Swap(left,(left+right)/2);
    last=left;
    for (i=left+1;i<=right;i++)
      if (mHitId[mHitIndex[i]]<mHitId[mHitIndex[left]]) Swap(++last,i);
    Swap(left,last);
    Sort(left,last-1);
    Sort(last+1,right);
  }
  
  void Swap(long i, long j) {
    long tmp;
    
    tmp = mHitIndex[j];
    mHitIndex[j] = mHitIndex[i];
    mHitIndex[i] = tmp;
  }

public:
  
  StEstTPCTrack(long int id, long int maxhits, StHelix *hel, double pt) {
    mNHits   = 0;
    mMaxHits = maxhits;
    mR       = new StThreeVectorD*[mMaxHits];
    mdR      = new StThreeVectorD*[mMaxHits];
    row      = new int[mMaxHits];
    if(!mR || !mdR)
      gMessMgr->Error()<<"StEstTPCTrack : not enougth memory mR or mdR = NULL"<<endm;
    mHitId = new long int[mMaxHits];
    if(!mHitId) 
      gMessMgr->Error()<<"StEstTPCTrack: not enougth memory mHitId=NULL"<<endm;
    mHitIndex = new long int[mMaxHits];
    mHitFlag = new int[mMaxHits];
    mHelix    = hel;
    mPt       = pt;
    mId       = id;
    mFlagSP   = 0;
    mType     = 0;
    mNFit     = 0;
    mParentPid =0;
    mParentMcId =0;
    mVid        =0;
    for(int i=0; i<mMaxHits; i++){
      mR[i] = NULL;
      mdR[i] = NULL;
      row[i] = 0;
      mHitId[i] = 0;
      mHitIndex[i] = 0;
      mHitFlag[i] = 0;
    }
  };  
  
  ~StEstTPCTrack() {
    long i;
    for (i=0;i<mMaxHits;i++) {
      delete mR[i];
      delete mdR[i];
    }
    delete [] mR;
    delete [] mdR;
    delete [] mHitId;
    delete [] mHitIndex;
    delete [] mHitFlag;
    delete [] row;
    if (mHelix!=NULL) delete mHelix;
  };
  
  void SetR(double rr) { mr = rr;};

  double GetR() { return mr;};

  void SetFlagSP(int fl) {mFlagSP = fl;};
  int GetFlagSP() {return mFlagSP;};

  int AddHit(long int id, StThreeVectorD *x, StThreeVectorD *dx, int nrow,int flag) {
    if(mNHits>=mMaxHits) 
      return 0;
    mHitId[mNHits] = id;
    mHitIndex[mNHits]=mNHits;
    if (mR[mNHits]!=NULL) delete mR[mNHits];
    mR[mNHits] =x; 
    if (mdR[mNHits]!=NULL) delete mdR[mNHits];
    mdR[mNHits]=dx;
    row[mNHits]=nrow;
    mHitFlag[mNHits]=flag;
    mNHits++;
    return 1;
  };

  long int GetNHits() {return mNHits;};
  StHelix* GetHelix() {return mHelix;};

  void SetFlag(int fl) {mFlag = fl;};
  int GetFlag() {return mFlag;};

  void SetNFit(long int nf) {mNFit = nf;};
  long int GetNFit() {return mNFit;};
  
  void SortHits() {
    Sort(0,mNHits-1);
  }

  long int GetMcId() {return mMcId;};
  void SetMcId(long int mcid) {mMcId = mcid;};

  long int GetId() {return mId;};
  void SetId(long int id) {mId = id;};

  long int GetPid() {return mPid;};
  void SetPid(long int pid) {mPid = pid;};

  long int GetParentPid() {return mParentPid;};
  void SetParentPid(long int parent_pid) {mParentPid = parent_pid;};
  
  long int GetParentMcId() {return mParentMcId;};
  void SetParentMcId(long int parent_mcid) {mParentMcId = parent_mcid;};

  long int GetVid() {return mVid;};
  void SetVid(long int vid) {mVid = vid;};
  double GetPt() {return mPt;};

  friend class StEstTracker;

};

#endif




