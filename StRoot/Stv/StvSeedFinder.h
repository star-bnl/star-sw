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

/// \class StvSeedFinder
class StvDraw;
class StvHit;



class THelixTrack;
class StvSeedFinder : public TNamed
{
public:
enum {kMinHits=5,kMaxHits = 10};
public:
  StvSeedFinder(const char *name);
  virtual ~StvSeedFinder(){;}
  virtual const THelixTrack *NextSeed()	=0;
  virtual void      Reset()		=0;
  virtual void      Clear(const char* opt="");
  virtual int       Again(int){return 0;}
  virtual void      FeedBack(int success);

virtual const StvHits *GetHits() const 	{return &fSeedHits;}

  virtual void Show();
  virtual void ShowRest(EDraw3DStyle style = kUnusedHit);
        double GetXi2(int i=1) const	{return fXi2[i];}
          void KNNMiMax(double &mi,double &ma);


static    StvDraw *NewDraw();
protected:
  const THelixTrack* Approx();
protected:
int fMinHits;		//Min number of hits accepted
int fGoodHits;		//Good number of hits. Used in first pass
StvHits  fSeedHits;
THelixFitter fHelix;
StvDraw *fDraw;
double   fXi2[2];	//Xi2[0] without hit errs,[1] with hit errs
ClassDef(StvSeedFinder,0);
};

class StvSeedFinders : public std::vector<StvSeedFinder*>
{ public:
  void Clear();
  void Reset();
};


#endif
