///\file StiKNNSeedFinder.cxx 
///\author Victor Perev (BNL Software) 11/2012
#include <math.h>
#include "StarRoot/THelixTrack.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiKNNSeedFinder2.h"
#include "StiToolkit.h"
#include "StiDetectorContainer.h"
#include "StiSortedHitIterator.h"
#include "StiKalmanTrack.h"
#include "StarRoot/StMultiKeyMap.h"
#include "StiUtilities/StiDebug.h"
#define __NOSTV__

#define StvHit_HH 1
#define ST_TGEOHELPER_H

#include "Stv/StvStl.h"
typedef  StiDetector StHitPlane;
class StvHit: private StiHit
{
public:
  int timesUsed() const 	{ return StiHit::isUsed();}
  void setTimesUsed(int i) 	{ StiHit::setTimesUsed(i);}
  const StiDetector* detector() const {return StiHit::detector();}
  const float *x() const	{ return &_xg            ;}
};


#define StvSeedFinder_HH 1
class StvSeedFinder
{
public:
   StvSeedFinder(const char *name=0)	{if (name){};}
   void Clear() 			{fSeedHits.clear();}
const THelixTrack* Approx() 		{return (THelixTrack*)(-1);}
const StvHits *GetHits() const 		{return &fSeedHits;}
void  FeedBack(int)     {};
   float fXi2[2];
protected:
   StvHits  fSeedHits;
   
};
#define __STVDEBUG_h_
class StvDebug
{
public:
static void Count(const char*,double){;}
static void Count(const char*,double,double){;}
static int  Flag (const char*){return 0;}
static int  iFlag (const char *flagName, int dflt=0){ return StiDebug::iFlag(flagName,dflt);}
};


#include "StvSeed/StvGoneRejector.h"
#include "StvSeed/StvKNSeedSelector.h"
#include "StvSeed/StvKNSeedFinder.h"

class myStvKNSeedFinder: public StvKNSeedFinder
//______________________________________________________________________________
{
  public:
  void Reset();
  void Clear();
};


//______________________________________________________________________________
StiKNNSeedFinder::StiKNNSeedFinder()
{
  fRxyMin=0;
  fStvKNSeedFinder = new myStvKNSeedFinder;
}

//______________________________________________________________________________
StiKNNSeedFinder::~StiKNNSeedFinder()
{
  delete fStvKNSeedFinder;
}

//______________________________________________________________________________
/// Produce the next track seed 
/// Loop through available hits and attempt to form a track seed
/// Only use hits that have not been already used in fully formed
/// tracks. 
//______________________________________________________________________________
StiTrack* StiKNNSeedFinder::findTrack(double rMin)
{
static StiToolkit *kit = StiToolkit::instance();
  fRxyMin = rMin;
  while(1) {
    fTrack = 0;
    const THelixTrack *hel = fStvKNSeedFinder->NextSeed();
    if (!hel) 		return 0;
    const std::vector<StiHit*> *hits = (const std::vector<StiHit*>*)fStvKNSeedFinder->GetHits();
    if (!hits)		continue;
    if (hits->size()<5)	continue;

    fTrack = kit->getTrackFactory()->getInstance();
    fEta=0;
    int ifail = ((StiKalmanTrack*)fTrack)->initialize(*hits);
    if (ifail) 		continue;
    fEta = fTrack->getPseudoRapidity();
    return  fTrack;
  }
}

//______________________________________________________________________________
void myStvKNSeedFinder::Reset()
{
static StiToolkit *kit = StiToolkit::instance();  
assert(!f1stHitMap->size()); 

  StiHitContainer      *hitContainer     = kit->getHitContainer();
  const HitMapToVectorAndEndType& mymap = hitContainer->hits();
  for (HitMapToVectorAndEndType::const_iterator it=mymap.begin()
      ;it != mymap.end()
      ;++it) {
    const vector<StiHit*> &vh = (*it).second.hits();
    for (int ih=0;ih<(int)vh.size();ih++) {
      StvHit *hit = (StvHit*)vh[ih];
      if (hit->timesUsed()) continue;
      const float *x = hit->x();
      double qwe = x[0]*x[0]+x[1]*x[1]+x[2]*x[2];
      f1stHitMap->insert(std::pair<float,StvHit*>(-qwe, hit));
      float xx[6] = {x[0],x[1],x[2],x[0]-x[1],x[1]-x[2],x[2]-x[0]};
      fMultiHits->Add(hit,xx);
  } }

  fMultiHits->MakeTree();
  *f1stHitMapIter = f1stHitMap->begin();

}
//______________________________________________________________________________
void StiKNNSeedFinder::reset()
{
fStvKNSeedFinder->Reset();
}
//______________________________________________________________________________
void StiKNNSeedFinder::clear()
{
fStvKNSeedFinder->Clear();
}
//______________________________________________________________________________
void myStvKNSeedFinder::Clear()
{
fMultiHits->Clear();
f1stHitMap->clear();
}
//______________________________________________________________________________
void StiKNNSeedFinder::FeedBack(int badGood)
{
  if (!fEta) return;
  if (badGood<=0) {
    StiDebug::Count("BadEta",fEta);
  } else {
    StiDebug::Count("GoodEta",fEta);
  }
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
#include "Stv/StvStl.cxx"
#define Cop CopRej
#include "StvSeed/StvGoneRejector.cxx"
#undef Cop
#define Cop CopSel
#include "StvSeed/StvKNSeedSelector.cxx"

#include "TCernLib.h"
#include "TVector3.h"
#include "StMultiKeyMap.h"
#include "THelixTrack.h"
#include "StvSeed/StvSeedConst.h"
#undef Cop
#include "StvSeed/StvKNSeedFinder.cxx"
