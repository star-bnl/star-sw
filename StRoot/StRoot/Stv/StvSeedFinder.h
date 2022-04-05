/// \File StvSeedFinder.h
/// \author Victor Perev 01/2010
#ifndef StvSeedFinder_HH
#define StvSeedFinder_HH
#include <assert.h>
#include <vector>
#include "THelixTrack.h"
#include "TNamed.h"
#include "StvStl.h"
#include "StDraw3D.h"

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

//#define KNNDEBUG 1


/// \class StvSeedFinder
class StvDraw;
class StvHit;
class StvTrack;
class StvKonst_st;


class THelixTrack;
class StvSeedFinder : public TNamed
{
public:
  StvSeedFinder(const char *name);
  virtual ~StvSeedFinder(){;}
          void SetCons(const StvKonst_st *kons); 
  virtual const THelixTrack *NextSeed()	=0;
  virtual void      Reset()		=0;
  virtual void      Clear(const char* opt="");
  virtual int       Again(int){return 0;}
  virtual void      FeedBack(const StvTrack *tk);
  virtual void      SetSgn(int sgn=1){fSgn = sgn;}
  virtual void      SetVtx(const float vtx[3]);
          void      SetIdTruth() { fIdTruth = 1; }
  virtual  int      IfVtx() const {return fVtx[2]<1e11;}

virtual const StvHits *GetHits() const 	{return &fSeedHits;}

  virtual void Show();
  virtual void ShowRest(EDraw3DStyle style = kUnusedHit);
  virtual void ShowIn();
  virtual  int Reject(const float *x) 	{return 0;}

        double GetXi2(int i=1) const	{return fXi2[i];}
          void KNNMiMax(double &mi,double &ma);


static    StvDraw *NewDraw();
protected:
  const THelixTrack* Approx();
protected:
int fMinHits;		//Min number of hits accepted
int fMaxHits;		//Max number hits fo seed
int fSgn;
int fIdTruth;
StvHits  fSeedHits;
THelixFitter fHelix;
StvDraw *fDraw;
float fVtx[3];		//Vertex if already known
double fXi2[2];		//Xi2[0] without hit errs,[1] with hit errs
ClassDef(StvSeedFinder,0);
};

class StvSeedFinders : public std::vector<StvSeedFinder*>
{ public:
  void Clear();
  void Reset();
  void Add(StvSeedFinder *sf);
  void SetCons(const StvKonst_st *kons); 
  void SetVtx(const float vtx[3]);
};


#endif
