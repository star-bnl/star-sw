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

class StvTrackFitter : public TNamed
{
public:
  StvTrackFitter(const char *name);
  virtual ~StvTrackFitter();
  virtual  int Refit(StvTrack *trak,int dir,int lane, int mode=1)=0;
  virtual void Clear(const char *opt="");	
  virtual  int Fit(const StvTrack *trak,const StvHit *vtx,StvNode *node)=0;
  virtual  int Helix(StvTrack *trak,int mode)=0;
  virtual  int Check(StvTrack *trak) 		{return 0;}
  virtual  int Check(const StvNodePars &parA,const StvFitErrs &errA,
		     const StvNodePars &parB,const StvFitErrs &errB) {return 0;}
  virtual  THelixTrack* GetHelix() const 	{return     0;}
           int GetNDF() const 			{return mNDF ;}     
        double GetDca3() const 			{return mDca3;}     
        double GetXi2() const 			{return mXi2 ;}     
      StvNode* GetWorstNode() const             {return mWorstNode;}
        double GetWorstXi2()  const             {return mWorstXi2 ;}
static StvTrackFitter *Inst() {return fgInst;}

protected:
char mBeg[1];
int    mNDF;
StvNode *mWorstNode;
double mXi2;
double mWorstXi2;
double mDca3;
char mEnd[1];
private:
static StvTrackFitter *fgInst;


ClassDef(StvTrackFitter,0);
};


#endif
