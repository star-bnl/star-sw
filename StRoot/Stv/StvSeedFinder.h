/// \File StvSeedFinder.h
/// \author Victor Perev 01/2010
#ifndef StvSeedFinder_HH
#define StvSeedFinder_HH
#include <assert.h>
#include <vector>
#include "THelix3d.h"
#include "TNamed.h"
#include "StvStl.h"
#include "StvUtil/StvGrappa.h"

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

//#define KNNDEBUG 1


/// \class StvSeedFinder
class StvGrappa;
class StvHit;
class StvTrack;
class StvKonst_st;


class THelixTrack_;
class StvSeedFinder : public TNamed
{
public:
  StvSeedFinder(const char *name);
  virtual ~StvSeedFinder(){;}
          void SetCons(const StvKonst_st *kons); 
  virtual const THelixTrack_ *NextSeed()	=0;
  virtual void      Reset()		=0;
  virtual void      Clear(const char* opt="");
  virtual int       Again(int)		{return 0;}
  virtual void      FeedBack(const StvTrack *tk);
  virtual void      SetSgn(int sgn=1)	{fSgn = sgn;}
  virtual void      SetVtx(const float vtx[3]);
          void      SetIdTruth() 	{fIdTruth = 1; }
  virtual  int      IfVtx() const {return fVtx[2]<1e11;}


virtual const StvHits *GetHits() const 	{return &fSeedHits;}
virtual void  Init(StvTrack* tk) const; 

  virtual void Show();
  virtual void ShowRest(int style = 0);
  virtual void ShowIn();
  virtual  int Reject(const float *x) 	{return 0;}

        double GetXi2(int i=1) const	{return fXi2[i];}
          void KNNMiMax(double &mi,double &ma);
          void DrawHelix();
  void SetCurrent() 			{fgCurrFinder = this;}
static  StvSeedFinder* GetCurrent() 	{return fgCurrFinder;}
static  StvGrappa *NewDraw();

static StvSeedFinder *fgCurrFinder;
protected:
  const THelixTrack_* Approx();
protected:
int fMinHits;		//Min number of hits accepted
int fMaxHits;		//Max number hits fo seed
int fSgn;
int fIdTruth;
StvHits  fSeedHits;
float fVtx[3];		//Vertex if already known
double fXi2[2];		//Xi2[0] without hit errs,[1] with hit errs
THelixFitter_ fHelix;	//!
StvGrappa *fDraw;
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
