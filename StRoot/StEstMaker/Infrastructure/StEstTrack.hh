/***************************************************************************
 *
 * $Id: StEstTrack.hh,v 1.1 2000/12/07 11:14:28 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstTrack Class
 *
 ***************************************************************************
 *
 * $Log: StEstTrack.hh,v $
 * Revision 1.1  2000/12/07 11:14:28  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstTrack_hh
#define StEstTrack_hh
#ifndef StMaker_H
#include "StMaker.h"
#endif


class StEstBranch;
class StEstHit;
class StEstTPCTrack;
class StEstMaker;

#include "StHelix.hh"
#include "StEstBranch.hh"
#include "StEstTPCTrack.hh"

class StEstTrack {

protected:

  StEstTPCTrack *mTPCTrack;
  StEstBranch   **mBranch;
  StEstBranch    *mIdealBranch;
  StEstBranch    *mFindableBranch;
  long mNBranch;     // number of branches in this track
  long mMaxBranch;
  int  mFlag; // 0=track available 1=track already used -1=problems
  int mDoIt;  // local flag to know if the track should be (1) or not (0) treated by the tracker during a pass
  int mDone;  // local flag to know if the track should be (1) or not (0) treated by the tracker at the end of a superpass
  int mIdealPattern; 
  int mIdealNHits;
  int mFindablePattern; 
  int mFindableNHits;
  StHelix* mHelix;
  
public :
    
  StEstTrack(long int maxbranch, StEstTPCTrack *tr);
  ~StEstTrack() {
    delete mTPCTrack;
    delete [] mBranch;
    if (mHelix!=NULL) delete mHelix;
  };
  int AddBranch(StEstBranch *branch);
  int AddFindableBranch(StEstBranch *branch);
  int AddIdealBranch(StEstBranch *branch);
  int SetBranch(long i, StEstBranch *branch);
  int RemoveBranch(long int nbr);
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

  friend class StEstMaker;
};

inline long int StEstTrack::GetNBranches() {return mNBranch;};
inline StEstTPCTrack* StEstTrack::GetTPCTrack() {return mTPCTrack;};
inline StEstBranch* StEstTrack::GetIdealBranch() {return mIdealBranch;};
inline StEstBranch* StEstTrack::GetFindableBranch() {return mFindableBranch;};
inline StEstBranch* StEstTrack::GetBranch(long int nbr) {
  if (nbr<0) {
    cerr << "ERROR StEstTrack::GetBranch nbr<0" <<endl;
    cout << "ERROR StEstTrack::GetBranch nbr<0" <<endl;
    return NULL;
  }
  if (mNBranch<nbr) {
    cerr << "ERROR StEstTrack::GetBranch mNBranch<nbr" << endl;
    cout << "ERROR StEstTrack::GetBranch mNBranch<nbr" << endl;
    cout << "mNBranch="<<mNBranch<<" nbr="<<nbr<<" TPC Id = "<<mTPCTrack->GetId()<<endl;
    return NULL;
  }
  return mBranch[nbr];
};

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
    cerr << "ERROR StEstTrack::SetBranch i<0" << endl;
    return 1;
  }
  if (i>mNBranch) {
    cerr << "ERROR StEstTrack::SetBranch i>mNBranch" << endl;
    return 1;
  }
  mBranch[i]=branch;
  return 0;
};

inline void     StEstTrack::SetHelix(StHelix *hel) {
  if (mHelix!=NULL)
    delete mHelix;
  mHelix=hel;
};
inline StHelix* StEstTrack::GetHelix() {return mHelix;};

#endif










