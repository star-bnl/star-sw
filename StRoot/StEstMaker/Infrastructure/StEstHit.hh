/***************************************************************************
 *
 * $Id: StEstHit.hh,v 1.4 2001/07/15 20:31:33 caines Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the StEstHit Class
 *
 ***************************************************************************
 *
 * $Log: StEstHit.hh,v $
 * Revision 1.4  2001/07/15 20:31:33  caines
 * Fixes from Insure++ debugging
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
#ifndef StEstHit_hh
#define StEstHit_hh
#include "StMaker.h"
#include "StThreeVectorD.hh"
class StEstWafer;
class StEstBranch;
class StEstTrack;
class StEstTracker;

class StEstHit {

protected:
  StEstWafer*   mDetector; // pointer to the wafer of the hit
  StEstBranch** mBranch; // list of pointer to the branches using the hit
  StThreeVectorD* mXL;   // local coordinates
  StThreeVectorD* mXG;   // global coordinates
  StThreeVectorD* mEL;   // Errors in local coordinates (Not used now)
  StThreeVectorD* mEG;   // Errors in global coordinates
  long mNShare; // number of tracks sharing the hit
  long mMaxShare; // maximum number of tracks sharing the hit
  long mNBranch; // number of branches sharing the hit
  long mMaxBranches; // maximum number of branches sharing the hit
  int mDebugLevel; //Control the printing level in the class methods
  long mId; // Id of the hit from scs_spt
  long mMcId; // McId of the hit from scs_spt
  int mFlag; // 0=hit available 1=hit already used and frozen -1=problems

public:
  
  StEstHit(long id, StThreeVectorD *xg, StThreeVectorD *xl, long maxbranches, long maxsh, StEstWafer *det);
  StEstHit(long id, StThreeVectorD *xg, StThreeVectorD *xl, StThreeVectorD *eg, StThreeVectorD *el, long maxbranches, long maxsh, StEstWafer *det);
  ~StEstHit();
  
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
  StThreeVectorD* GetGlobE();
  StThreeVectorD* GetLocE();
  StEstWafer*  GetWafer();
  void DetachFromWafer();
  StEstBranch* GetBranch(int i);
  int GetFlag();
  void SetFlag(int fl);
  long GetId();
  void SetId(long i);
  long GetMcId();
  void SetMcId(long i);

  friend class StEstTracker;
};

inline int StEstHit::CheckAvailability() {
  if (mNShare<mMaxShare && mFlag==0) return 1;
  else return 0;
};
inline StThreeVectorD* StEstHit::GetGlobX() {return mXG;};
inline StThreeVectorD* StEstHit::GetLocX()  {return mXL;};
inline StThreeVectorD* StEstHit::GetGlobE() {return mEG;};
inline StThreeVectorD* StEstHit::GetLocE()  {return mEL;};
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





