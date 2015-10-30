// $Id: StvHitLoader.cxx,v 1.27 2015/10/30 19:26:57 perev Exp $
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
#include "TGeoManager.h"
#include "StvHitLoader.h"
#include "StvStEventHitSelector.h"
#include "Stv/StvHit.h"
#include "Stv/StvToolkit.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "StEvent.h"
#include "StHit.h"
#include "StIstHit.h"	////????
#include "StPxlHit.h"	////????
#include "StEventUtilities/StEventHelper.h"
#include "StEventUtilities/StEventHitIter.h"
#include "StvUtil/StvDebug.h"
#include "Stv/StvDraw.h"
#include "Stv/StvStl.h"
#include "StvStEventHitSelector.h"
ClassImp(StvHitLoader)
//_____________________________________________________________________________
StvHitLoader::StvHitLoader(const char *name) : TNamed(name,"")

{
  mHitIter = new StEventHitIter();
  mHitSelector = 0; mHitLoadActor = 0;
}

//_____________________________________________________________________________
StvHitLoader::~StvHitLoader() 
{
  delete mHitIter;
}

//_____________________________________________________________________________
void StvHitLoader::Clear(const char*)
{
  StTGeoProxy::Inst()->ClearHits();
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
     if (!StTGeoProxy::Inst()->IsActive((StDetectorId)id)) continue;
     mHitIter->AddDetector((StDetectorId)id);
     nDet++;
   }
   return nDet;
}
//_____________________________________________________________________________
void StvHitLoader::SetHitSelector()
{
   mHitSelector = new StvStEventHitSelector;
}

//_____________________________________________________________________________
int StvHitLoader::LoadHits(const StEvent *stev)
{
enum {kFCF_CHOPPED=256		// 0x100 cluster is chopped from its neighbour: OFFLINE use only
     ,kFCF_SANITY =512};	// 0x200 cluster extents not sane
static int nCall=0; nCall++;
static int myGraph=0;
static StTGeoProxy* tgp = StTGeoProxy::Inst();
       tgp->SetHitLoadActor(mHitLoadActor);

StvDraw *myDraw=0;
StvHits *myHits=0;
if (myGraph) { //create canvas
  myDraw = new StvDraw();
  myHits = new StvHits;
  if (myHits) {/*noopt*/}
}
  mHitIter->Reset(stev);
  int nSel = (mHitSelector)? mHitSelector->Edit(stev):-1;
  if (!nSel) return 0;
  const StHit *stHit=0;
  StDetectorId didOld = kUnknownId;
  int nTotHits = 0,nTotHitz=0,nTotGits=0, nHits=0,nHitz=0,nGits=0;

  for (; ; ++(*mHitIter)) {
    stHit=*(*mHitIter);
//		If hit selector is ON and hit is not marked, ignore it
    StDetectorId did = mHitIter->DetectorId();
    
    if (did != didOld || !stHit) {
      if (didOld) {
        Info("LoadHits","Loaded  %d good, recovered %d and failed %d %s hits"
	    ,nHits,nHits-nGits,nHitz,StTGeoProxy::DetName(didOld));
      }
      didOld = did; 
      
      if (!stHit) break;
      Info("LoadHits","Start %s hits",StTGeoProxy::DetName(did));
      if (mHitLoadActor) mHitLoadActor->SetDetId(did);
      nHits=0; nHitz=0,nGits=0;
    }
    if (nSel> 0 && (!stHit->TestBit(StvStEventHitSelector::kMarked))) 	continue; // ignore not selected hit
    if (stHit->flag() & kFCF_CHOPPED || stHit->flag() & kFCF_SANITY)	continue; // ignore hits marked by AfterBurner as chopped o
    mDetId = did;
    int sure;
    StvHit *stvHit = MakeStvHit(stHit,mHitIter->UPath(),sure);
//     if (!sure && stvHit) { //Non reliable hit
//       double rxy = sqrt(pow(stvHit->x()[0],2)+pow(stvHit->x()[1],2));
//       StvDebug::Count("OrphanHits",stvHit->x()[2],rxy);
//     }

    if (stvHit) {nHits++;nTotHits++;nGits+=sure;nTotGits+=sure;}  
    else 	{nHitz++;nTotHitz++;}
  }
  int nIniHits = tgp->InitHits();
  assert(nTotHits==nIniHits);
  Info("LoadHits","Loaded %d good, recovered %d and failed %d of all hits"
      ,nTotHits,nTotHits-nTotGits,nTotHitz);
  return nTotHits;
}

//_____________________________________________________________________________
StvHit *StvHitLoader::MakeStvHit(const StHit *stHit,UInt_t upath, int &sure)
{
static StTGeoProxy *tgh = StTGeoProxy::Inst();
   assert(stHit);
   StvHit *stvHit = StvToolkit::Inst()->GetHit();
   StDetectorId did = stHit->detector();
   UInt_t hard = stHit->hardwarePosition();
   if (!hard) hard = upath;
   StThreeVectorF v3f = stHit->position();
   const float *xyz = v3f.xyz();
   stvHit->set(stHit,xyz);
   int seed = 1;
   if (did == kTpcId) {	// Special case for TPCHit. Prompt info added
//   enum {zPrompt = 205,rMiddle=124};
     enum {zPrompt = 205,rMiddle=0};
     hard <<=1; hard |= (fabs(xyz[2]) > zPrompt);
     if (xyz[0]*xyz[0]+xyz[1]*xyz[1] >rMiddle*rMiddle) seed = 1;
   }
   hard *= (uint)kMaxDetectorId; hard+=(uint)did;
   
   const StHitPlane *hp = tgh->AddHit(stvHit,mDetId,xyz,hard,seed);
   sure =  tgh->IsGoodHit();
   if (!hp) { StvToolkit::Inst()->FreeHit(stvHit);return 0;}

   if (did == kTpcId && fabs(xyz[2])<200) {// TPC hit check for being in sector
     const float* org = hp->GetOrg(xyz);
     const float* ort = (fabs(org[2])<209)? hp->GetDir(xyz)[0]:hp->GetDir(xyz)[2];
     double art = atan2(ort[1],ort[0])*180/M_PI;
     double arg = atan2(org[1],org[0])*180/M_PI;
     assert(fabs(art-arg)<1.e-3);

     double dang = (atan2(ort[1],ort[0])-atan2(xyz[1],xyz[0]))*57.3;
     if (dang> 180) dang-=360;
     if (dang<-180) dang+=360;
     if (fabs(dang)>17) printf("dang = %g\n",dang);
     assert(fabs(dang)<31);
   }
   stvHit->set(hp);
   return stvHit;
}

//_____________________________________________________________________________
int StvHitLoader::TpcHitTest(const StHit *stHit)
{
  enum {nbpads = 73,maxpads=100};
  int tpads[maxpads]   =   {  1, 1, 1, 2, 2, 2, 3, 3, 3, 4,
			      4, 4, 5, 5, 5, 6, 6, 6, 7, 7,
			      7, 8, 8, 8, 9, 9, 9,10,10,10,
			     11,11,11,12,12,12,13,13,13,14,
			     14,15,16,17,18,19,20,21,22,23,
			     24,25,26,27,28,29,30,31,32,33,
			     34,35,36,37,38,39,40,41,42,43,
			     44,45,45};                    

  int isdets[maxpads]   =  { 1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			     0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
			     2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
			     1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 2};                    


  StThreeVectorF v3f = stHit->position();
  TGeoNode *node = gGeoManager->FindNode(v3f[0],v3f[1],v3f[2]);
  assert(node);
  if (strncmp("TPA",node->GetName(),3)) return -1;
  int numbv[3];
  for (int i=0;i<3;i++) {
    node = gGeoManager->GetMother(i);
    numbv[2-i] = node->GetNumber();
  }
//int tpgv  = numbv[0];
//int tpss  = numbv[1];
//int sector= tpss+12*(tpgv-1) ;
  int tpad  = numbv[2];
  int isdet = 0;

  if (tpad > nbpads) tpad -= nbpads;
  isdet = isdets[tpad-1];
  tpad  = tpads [tpad-1];
  if (isdet) {
    Warning("TpcHitTest","TpcHit(%g,%g,%g) isdet=%d WRONG WRONG WRONG"
           ,v3f[0],v3f[1],v3f[2], isdet);
  }
  return isdet;
}
    
    
    
  
  
