/// \File StvSeedFinder.h
/// \author Victor Perev 01/2010
#ifndef StvSeedFinder_HH
#define StvSeedFinder_HH
#include <vector>
#include "THelixTrack.h"
#include "TNamed.h"
#include "StvStl.h"
/// \class StvSeedFinder
class StvDraw;
class StvHit;



class THelixTrack;
class StvSeedFinder : public TNamed
{
public:
  StvSeedFinder(const char *name);
  virtual ~StvSeedFinder(){;}
  virtual const THelixTrack *NextSeed()	=0;
  virtual void      Reset()		=0;
  virtual void      Clear(const char* opt="");

const StvHits *GetHits() const 	{return &fSeedHits;}

static StvSeedFinder* Inst()		{return fgSeedFinder;}
          void Show();
          void ShowRest();
           int DoShow() const 		{return fDoShow;}
          void DoShow(int lev);
static    StvDraw *NewDraw();
protected:
StvHits  fSeedHits;
THelixFitter fHelix;
int fDoShow;
StvDraw *fDraw;
static StvSeedFinder* fgSeedFinder;

ClassDef(StvSeedFinder,0);
};

#endif
