// $Id: StvHitLoader.cxx,v 1.8 2012/02/01 19:17:34 perev Exp $
/*!
\author V Perev 2010

A StvHitLoader loads  Stv hits.
<br>
Main tasks:				
<ul>
<li> Loop over StHits;			
<li> Make & fill according StvHits;				
<li> Fill StTGeoHelpr containers by StvHits;		
</ul>
 

*/
#include <Stiostream.h>
#include <math.h>
#include <string>
#include "StvHitLoader.h"
#include "Stv/StvHit.h"
#include "Stv/StvToolkit.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"
#include "StEvent.h"
#include "StHit.h"
#include "StEventUtilities/StEventHelper.h"
#include "StEventUtilities/StEventHitIter.h"
#include "Stv/StvDraw.h"
#include "Stv/StvStl.h"

ClassImp(StvHitLoader)
//_____________________________________________________________________________
StvHitLoader::StvHitLoader(const char *name) : TNamed(name,"")

{
  mHitIter = new StEventHitIter(0);
}

//_____________________________________________________________________________
StvHitLoader::~StvHitLoader() 
{
  delete mHitIter;
}

//_____________________________________________________________________________
void StvHitLoader::Clear(const char*)
{
  StTGeoHelper::Inst()->ClearHits();
}

//_____________________________________________________________________________
Int_t StvHitLoader::Finish()
{
  return 0;
}

//_____________________________________________________________________________
Int_t StvHitLoader::Init()
{
   int nDet=0;
   for (int id=1; id<kMaxDetectorId; id++){
     if (!StTGeoHelper::Inst()->IsActive((StDetectorId)id)) continue;
     mHitIter->AddDetector((StDetectorId)id);
     nDet++;
   }
   return nDet;
}

//_____________________________________________________________________________
int StvHitLoader::LoadHits(const StEvent *stev)
{
static int nCall=0; nCall++;
static int myGraph=0;
StvDraw *myDraw=0;
StvHits *myHits=0;
if (myGraph) { //create canvas
  myDraw = new StvDraw();
  myHits = new StvHits;
}

  mHitIter->Reset(stev);
  StHit *stHit=0;
  StDetectorId didOld = kUnknownId;
  int nTotHits = 0,nTotHitz=0, nHits=0,nHitz=0;

  for (; ; ++(*mHitIter)) {
    stHit=*(*mHitIter);
    StDetectorId did = mHitIter->DetectorId();
    if (did != didOld || !stHit) {
      if (didOld) {
        Info("LoadHits","Loaded  %d good and failed %d %s hits"
	    ,nHits,nHitz,StTGeoHelper::DetName(didOld));
      }
      if (!stHit) break;

      Info("LoadHits","Start %s hits",StTGeoHelper::DetName(did));
      nHits=0; nHitz=0;
    }
    StvHit *stiHit = MakeStvHit(stHit,mHitIter->UPath());

if (myHits) (*myHits)+= stiHit;   

    if (stiHit) {nHits++;nTotHits++;}  
    else 	{nHitz++;nTotHitz++;} 
    didOld = did; 
    if (!stiHit) continue;

  }
  int nIniHits = StTGeoHelper::Inst()->InitHits();
  assert(nTotHits==nIniHits);
  Info("LoadHits","Loaded %d good and failed %d of all hits",nTotHits,nTotHitz);
if (myDraw) {myDraw->Hits(*myHits,kUnusedHit); myDraw->Wait();}

  return nTotHits;
}
//_____________________________________________________________________________
StvHit *StvHitLoader::MakeStvHit(const StHit *stHit,UInt_t upath)
{
   StvHit *stiHit = StvToolkit::Inst()->GetHit();
   StDetectorId did = stHit->detector();
   UInt_t hard = stHit->hardwarePosition();
   StThreeVectorF v3f = stHit->position();
   const float *xyz = v3f.xyz();
   int seed = 1;
   if (did == kTpcId) {
//   enum {zPrompt = 205,rMiddle=124};
     enum {zPrompt = 205,rMiddle=0};
     hard <<=1; hard |= (fabs(xyz[2]) > zPrompt);
     if (xyz[0]*xyz[0]+xyz[1]*xyz[1] >rMiddle*rMiddle) seed = 1;
   }
   hard *= (uint)kMaxDetectorId; hard+=(uint)did;
   
   const StHitPlane *hp = StTGeoHelper::Inst()->AddHit(stiHit,xyz,hard,seed);
   if (!hp) { StvToolkit::Inst()->FreeHit(stiHit);return 0;}
   
   stiHit->set(hp,stHit,xyz);
   return stiHit;
}
