/***************************************************************************
 *
 * $Id: StEstBranch.hh,v 1.1 2000/12/07 11:14:27 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstBranch Class
 *
 ***************************************************************************
 *
 * $Log: StEstBranch.hh,v $
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstBranch_hh
#define StEstBranch_hh
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StHelix.hh"
#include "StEstTrack.hh"
#include "StEstTPCTrack.hh"
#include "StEstHit.hh"
#include "StEstWafer.hh"

class StEstTrack;
class StEstHit;
class StEstMaker;

class StEstBranch {
  
protected:
  
  StEstTrack* mTrack;
  StEstHit**  mHits;
  StHelix* mHelix;
  long int mNHits;
  long int mMaxHits;
  long int mNFit;
  long int mLastFitStatus;
  double mChisq;
  double mChisqCir;
  double mChisqLin;
  double *mDist;        // table of distances between hits and projections
  int mLastLay;
  int mDebugLevel;
  int mStep;
  int mIsGood;
  int mIsGoodOld;
  int mHitPosition;
  
public :
  StEstBranch(StEstTrack* tr=NULL, long int maxhits=1, long int nf=0, long int nh=0, StEstHit** hit=NULL, double *dist=0, int isgood=1);
  ~StEstBranch();
  int AddHit(StEstHit *hit, double dist);
  int RemoveHit(long int nr);
  int RemoveHit(StEstHit* hit);
  long int GetNHits();
  StEstHit* GetHit(long int nr);
  int CheckAvailability();
  StEstBranch* Duplicate();
  int JoinTrack(StEstTrack *tr, int IsIdeal);
  void LeaveTrack();
  StEstTrack* GetTrack();
  void SetHelix(StHelix *hel);
  StHelix* GetHelix();
  void SetChiSq(double chi);
  void SetChiSqCir(double chi);
  void SetChiSqLin(double chi);
  void SetNFit(long int nf);
  double GetChiSq();
  double GetChiSqCir();
  double GetChiSqLin();
  double GetDist(long nr);
  double GetDist(StEstHit* hit);
  long int GetNFit();
  int GetLastLayer();
  void SetDebugLevel(int deb);
  void SetStep(int step);
  int GetStep();
  void SetIsGood(int isgood);
  int GetIsGood();
  void SetIsGoodOld(int isgood);
  int GetIsGoodOld();
  friend class StEstMaker;
  //  StEstBranch& operator=(const StEstBranch);
};



inline StEstHit* StEstBranch::GetHit(long int nr) {
  if(nr>=mNHits) {
    cerr << "ERROR StEstBranch::GetHit  nr>=mNHits"<<endl;
    return NULL;
  }
  if(nr>=mMaxHits) {
    cout << "ERROR StEstBranch::GetHit  nr>=mMaxHits"<<endl;
    cout << "      nr= "<<nr<<" mNHits= "<<mNHits<<" mMaxHits= "<<mMaxHits<<endl;
    return NULL;
  }
  if(nr<0) {
    cerr << "ERROR StEstBranch::GetHit  nr<0"<<endl;
    return NULL;
  }
  return mHits[nr];
}
 
inline int StEstBranch::CheckAvailability() {
  if (mNHits<mMaxHits) return 1;
  else return 0;
}

inline long int StEstBranch::GetNHits()             {return mNHits;};
inline void     StEstBranch::SetHelix(StHelix *hel) {
  if (mHelix!=NULL)
    delete mHelix;
  mHelix=hel;
};
inline StHelix* StEstBranch::GetHelix()             {return mHelix;};
inline void     StEstBranch::SetChiSq(double chi)   {mChisq = chi;};
inline void     StEstBranch::SetChiSqLin(double chi)   {mChisqLin = chi;};
inline void     StEstBranch::SetChiSqCir(double chi)   {mChisqCir = chi;};
inline double   StEstBranch::GetChiSq()             {return mChisq;};
inline double   StEstBranch::GetChiSqCir()             {return mChisqCir;};
inline double   StEstBranch::GetChiSqLin()             {return mChisqLin;};
inline double   StEstBranch::GetDist(long nr)          {return mDist[nr];};
inline int      StEstBranch::GetLastLayer()         {return mLastLay;}; 
inline void     StEstBranch::SetDebugLevel(int deb) {mDebugLevel = deb;};
inline void     StEstBranch::SetStep(int step) {mStep = step;};
inline int      StEstBranch::GetStep() {return mStep;};
inline void     StEstBranch::SetIsGood(int isgood) {mIsGood = isgood;};
inline int      StEstBranch::GetIsGood() {return mIsGood;};
inline void     StEstBranch::SetIsGoodOld(int isgood) {mIsGoodOld = isgood;};
inline int      StEstBranch::GetIsGoodOld() {return mIsGoodOld;};
inline StEstTrack* StEstBranch::GetTrack() {return mTrack;};
inline void     StEstBranch::SetNFit(long nf) {mNFit=nf;};
inline long int StEstBranch::GetNFit() {return mNFit;};
  
#endif




