/// \File StvKalmanTrackFitter.h
/// \author Victor Perev 9/2010
#ifndef StvKalmanTrackFitter_HH
#define StvKalmanTrackFitter_HH
#include "StvTrackFitter.h"

/// \class StvKalmanTrackFitter
class StvTrack;
class StvNode;
class THelixTrack_;
class THelixFitter_;
class StvKonst_st;
class StvKalmanTrackFitter : public StvTrackFitter
{
public:
  StvKalmanTrackFitter();
  void SetCons(const StvKonst_st* kons);
  virtual ~StvKalmanTrackFitter(){;}	
  virtual  int RefitLoop(StvTrack *trak,int idir,int ilane, int numb);
  virtual  int Refit(StvTrack *trak,int dir,int lane,int mode);
  virtual  int Fit(const StvTrack *trak,const StvHit *vtx,StvNode *node);
           int Propagate(StvNode  *node,StvNode *preNode,int dir,int lane);
  virtual  int Helix(StvTrack *trak,int mode);
  virtual  int Check(StvTrack *trak);
  virtual  int Check(const StvNodePars &parA,const StvFitErrs &errA,
		     const StvNodePars &parB,const StvFitErrs &errB);
  virtual  THelixTrack_* GetHelix() const;
  virtual void Clear(const char *opt="");
  

protected:
char mBeg[1];
const StvKonst_st *mKons;
THelixFitter_ *mHelx;	//!
char mEnd[1];

ClassDef(StvKalmanTrackFitter,0);
};


#endif
