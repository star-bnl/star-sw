/***************************************************************************
 *
 * $Id: StEstTPCTrack.hh,v 1.1 2000/12/07 11:14:27 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstTPCTrack Class
 *
 ***************************************************************************
 *
 * $Log: StEstTPCTrack.hh,v $
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstTPCTrack_hh
#define StEstTPCTrack_hh
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StThreeVector.hh"
#include "StHelix.hh"
#include "StThreeVectorD.hh"

class StEstMaker;

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
  long int *mHitTrack;
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
    if(!mR || !mdR){
      cerr << "ERROR!!! not enougth memory" << endl;
      cerr << "StEstTPCTrack::StEstTPCTrack mR or mdR = NULL" << endl;
    }
    mHitId = new long int[mMaxHits];
    if(!mHitId) 
      cerr << "ERROR!!! StEstTPCTrack::StEstTPCTrack mHitId=NULL"<<endl;
    mHitIndex = new long int[mMaxHits];
    mHitFlag = new int[mMaxHits];
    mHitTrack = new long int[mMaxHits];
    mHelix    = hel;
    mPt       = pt;
    mId       = id;
    mFlagSP   = 0;
    mType     = 0;
    mNFit     = 0;
    mParentPid =0;
    mParentMcId =0;
    mVid        =0;
  };  
  
  ~StEstTPCTrack() {
    delete [] mR;
    delete [] mdR;
    delete [] mHitId;
    delete [] mHitIndex;
    delete [] mHitFlag;
    delete [] row;
  };
  
  void SetR(double rr) { mr = rr;};

  double GetR() { return mr;};

  void SetFlagSP(int fl) {mFlagSP = fl;};
  int GetFlagSP() {return mFlagSP;};

  int AddHit(long int id, StThreeVectorD *x, StThreeVectorD *dx, int nrow,int flag,long track) {
    if(mNHits>=mMaxHits) 
      return 0;
    mHitId[mNHits] = id;
    mHitIndex[mNHits]=mNHits;
    mR[mNHits] =x; 
    mdR[mNHits]=dx;
    row[mNHits]=nrow;
    mHitFlag[mNHits]=flag;
    mHitTrack[mNHits]=track;
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

  friend class StEstMaker;

};

#endif





