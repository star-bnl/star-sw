/***************************************************************************
 *
 * $Id: StEstHit.hh,v 1.1 2000/12/07 11:14:27 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the StEstHit Class
 *
 ***************************************************************************
 *
 * $Log: StEstHit.hh,v $
 * Revision 1.1  2000/12/07 11:14:27  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstHit_hh
#define StEstHit_hh
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StThreeVectorD.hh"
#include "StEstBranch.hh"

class StEstWafer;
class StEstBranch;
class StEstTrack;
class StEstMaker;

class StEstHit {

protected:
  StEstWafer*   mDetector;
  StEstBranch** mBranch;
  StThreeVectorD* mXL;   // local coor
  StThreeVectorD* mXG;   // global coor
  long mNShare;
  long mMaxShare;
  long mNBranch;
  long mMaxBranches;
  int mDebugLevel;
  long mId;
  long mMcId;
  int mFlag; // 0=hit available 1=hit already used -1=problems
  long mEvalTrack; // number of ideal track

public:
  
  StEstHit(long id, StThreeVectorD *xg, StThreeVectorD *xl, long maxbranches, long maxsh, StEstWafer *det);
  ~StEstHit() {
    delete [] mBranch;
    delete mXL;
    delete mXG;
  };
  
  int  GetNBranch();
  int  GetMaxBranches();
  int  GetNShare();
  int  GetMaxShare();
  int  JoinBranch(StEstBranch *br, StEstTrack *tr);
  void LeaveBranch(StEstBranch *br);
  void SetDebugLevel(int deb);
  int  GetDebugLevel();
  int  CheckAvailability();
  StThreeVectorD* GetGlobX();
  StThreeVectorD* GetLocX();
  StEstWafer*  GetWafer();
  StEstBranch* GetBranch(int i);
  int GetFlag();
  void SetFlag(int fl);
  long GetId();
  void SetId(long i);
  long GetMcId();
  void SetMcId(long i);

  friend class StEstMaker;
};

inline int StEstHit::CheckAvailability() {
  if (mNShare<mMaxShare && mFlag==0) return 1;
  else return 0;
};
inline StThreeVectorD* StEstHit::GetGlobX() {return mXG;};
inline StThreeVectorD* StEstHit::GetLocX()  {return mXL;};
inline StEstWafer* StEstHit::GetWafer() {return mDetector;};
inline int StEstHit::GetNBranch() {return mNBranch;};
inline int StEstHit::GetMaxBranches() {return mMaxBranches;};
inline int StEstHit::GetNShare() {return mNShare;};
inline int StEstHit::GetMaxShare() {return mMaxShare;};
inline StEstBranch* StEstHit::GetBranch(int i) {
  if (i<mNBranch&&i>=0) return mBranch[i]; 
  else return NULL;
};
inline void StEstHit::SetDebugLevel(int deb) {mDebugLevel=deb;};
inline int StEstHit::GetDebugLevel() {return mDebugLevel;};
inline void StEstHit::SetFlag(int fl) {mFlag = fl;};
inline int StEstHit::GetFlag() {return mFlag;};
inline long StEstHit::GetId() {return mId;};
inline void StEstHit::SetId(long i) {mId=i;};
inline long StEstHit::GetMcId() {return mMcId;};
inline void StEstHit::SetMcId(long i) {mMcId=i;};
#endif







