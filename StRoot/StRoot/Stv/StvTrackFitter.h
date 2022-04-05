/// \File StvTrackFitter.h
/// \author Victor Perev 9/2010
#ifndef StvTrackFitter_HH
#define StvTrackFitter_HH
#include "TNamed.h"

/// \class StvTrackFitter
class THelixTrack;
class StvTrack;
class StvNode;
class StvHit;
class StvNodePars;
class StvFitErrs;
class StvKonst_st;
class StvTrackFitter : public TNamed
{
public:
  StvTrackFitter(const char *name);
  virtual ~StvTrackFitter();
  virtual void SetCons(const StvKonst_st*)=0;
  virtual  int Refit(StvTrack *trak,int dir)=0;
  virtual  int Refit(StvTrack *trak,int dir,int lane, int mode=1)=0;
  virtual void Clear(const char *opt="");	
  virtual  int Fit(const StvTrack *trak,const StvHit *vtx,StvNode *node)=0;
  virtual  int Helix(StvTrack *trak,int mode)=0;
  virtual  int Check(StvTrack *trak) 		{return 0;}
  virtual  int Check(const StvNodePars &parA,const StvFitErrs &errA,
		     const StvNodePars &parB,const StvFitErrs &errB) {return 0;}
  virtual  int Clean(StvTrack *trak)=0;		
  virtual  THelixTrack* GetHelix() const 	{return     0;}
           int GetNDF()  const 			{return mNDF ;}     
        double GetDca3() const 			{return mDca3;}     
        double GetXi2()  const 			{return mXi2 ;}     
           int Failed()  const 			{return mFailed;}     
           int& NHits()   			{return mNHits;}     
 
protected:
char mBeg[1];
char mFailed;
int    mNHits;
int    mNDF;
double mXi2;
double mDca3;
char mEnd[1];
private:

ClassDef(StvTrackFitter,0);
};


#endif
