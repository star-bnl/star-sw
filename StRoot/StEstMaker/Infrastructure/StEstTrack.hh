/***************************************************************************
 *
 * $Id: StEstTrack.hh,v 1.6 2003/09/02 17:58:04 perev Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstTrack Class
 *
 ***************************************************************************
 *
 * $Log: StEstTrack.hh,v $
 * Revision 1.6  2003/09/02 17:58:04  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2001/07/15 20:31:33  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.4  2001/02/23 14:48:32  lmartin
 * cout replaced by gMessMgr.
 *
 * Revision 1.3  2001/01/26 09:36:56  lmartin
 * Minor changes. Useless statements removed. Add a short description of the data members
 *
 * Revision 1.2  2001/01/25 18:20:39  lmartin
 * Destructor completed to prevent memory leak.
 * New method RemoveLastBranch added to force the last branch destruction.
 *
 * Revision 1.1  2000/12/07 11:14:28  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstTrack_hh
#define StEstTrack_hh

#include "StMessMgr.h"
#include "StMaker.h"
#include <Stiostream.h>

class StEstBranch;
class StEstHit;
class StEstTPCTrack;
class StEstTracker;
class StHelix;

class StEstTrack {
protected:

  StEstTPCTrack *mTPCTrack; // pointer to the TPCTrack 
  StEstBranch   **mBranch; // list of pointers to the branches
  StEstBranch    *mIdealBranch; // pointer to the ideal branch
  StEstBranch    *mFindableBranch; // pointer to the findable branch
  long mNBranch; // number of branches in this track
  long mMaxBranch; // max number of branches allowed for this track
  int  mFlag; // 0=track available 1=track already used -1=problems
  int mDoIt;  // The track should/not (1/0) be treated by the tracker during a pass
  int mDone;  // The track should/not (1/0) be treated by the tracker at the end of a superpass
  int mIdealPattern; // Layer pattern for the hits of the ideal branch 
  int mIdealNHits; // Number of hits in the ideal branch
  int mFindablePattern; // Layer pattern for the hits of the findable branch 
  int mFindableNHits; // Number of hits in the findable branch
  StHelix* mHelix; // Pointer to the helix created in TPCInit
  
public :
    
  StEstTrack(long int maxbranch, StEstTPCTrack *tr);
  ~StEstTrack(); 
  void StEstTrackDestructor();
  int AddBranch(StEstBranch *branch);
  int AddFindableBranch(StEstBranch *branch);
  int AddIdealBranch(StEstBranch *branch);
  int SetBranch(long i, StEstBranch *branch);
  int RemoveBranch(long int nbr);
  int RemoveLastBranch();
  inline long int GetNBranches();
  StEstBranch* GetBranch(long int nbr);
  StEstBranch* GetIdealBranch();
  StEstBranch* GetFindableBranch();
  StEstTPCTrack* GetTPCTrack();
  int CheckAvailability();
  void SetFlag(int flag) {mFlag=flag;};
  int  GetFlag() {return mFlag;};
  void SetDoIt(int doit) {mDoIt=doit;};
  int GetDoIt() {return mDoIt;};
  void SetDone(int done) {mDone=done;};
  int GetDone() {return mDone;};
  void SetIdealPattern(int idealpattern) {mIdealPattern=idealpattern;};
  int GetIdealPattern() {return mIdealPattern;};
  void SetIdealNHits(int idealnhits) {mIdealNHits=idealnhits;};
  int GetIdealNHits() {return mIdealNHits;};
  void SetFindablePattern(int findablepattern) {mFindablePattern=findablepattern;};
  int GetFindablePattern() {return mFindablePattern;};
  void SetFindableNHits(int findablenhits) {mFindableNHits=findablenhits;};
  int GetFindableNHits() {return mFindableNHits;};
  void SetHelix(StHelix *hel);
  StHelix* GetHelix();

  friend class StEstTracker;
};

inline long int StEstTrack::GetNBranches() {return mNBranch;};
inline StEstTPCTrack* StEstTrack::GetTPCTrack() {return mTPCTrack;};
inline StEstBranch* StEstTrack::GetIdealBranch() {return mIdealBranch;};
inline StEstBranch* StEstTrack::GetFindableBranch() {return mFindableBranch;};
inline int StEstTrack::CheckAvailability() {
  if (mNBranch<mMaxBranch) return 1;
  else return 0;
}

inline int StEstTrack::AddBranch(StEstBranch *branch) {
  if (mNBranch<mMaxBranch) { 
    mBranch[mNBranch++]=branch;
    return 0;
  }
  else return 1;
};

inline int StEstTrack::AddIdealBranch(StEstBranch *branch) {
  if (branch!=NULL) { 
    mIdealBranch=branch;
    return 0;
  }
  else return 1;
};

inline int StEstTrack::AddFindableBranch(StEstBranch *branch) {
  if (branch!=NULL) { 
    mFindableBranch=branch;
    return 0;
  }
  else return 1;
};

inline int StEstTrack::SetBranch(long i, StEstBranch *branch) {
  if (i<0) {
    gMessMgr->Error()<<"StEstTrack::SetBranch i<0"<<endm;
    return 1;
  }
  if (i>mNBranch) {
    gMessMgr->Error()<<"StEstTrack::SetBranch i>mNBranch"<<endm;
    return 1;
  }
  mBranch[i]=branch;
  return 0;
};

inline StHelix* StEstTrack::GetHelix() {return mHelix;};

#endif









