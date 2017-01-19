// $Id: StvHitLoader.cxx,v 1.33 2017/01/19 16:56:01 perev Exp $
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
#include "TCernLib.h"
#include "StEvent/StRnDHit.h"	//???TempoHack???
#include "StvHitLoader.h"
#include "StvStEventHitSelector.h"
#include "Stv/StvHit.h"
#include "Stv/StvToolkit.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "StEvent.h"
#include "StHit.h"
#include "StEventUtilities/StEventHelper.h"
#include "StEventUtilities/StEventHitIter.h"
#include "StvUtil/StvDebug.h"
#include "Stv/StvDraw.h"
#include "Stv/StvStl.h"
#include "StvStEventHitSelector.h"
#include "Stv/StvToolkit.h"
ClassImp(StvHitLoader)

StMatrixF  Hack1to6(const StHit *stHit);

//_____________________________________________________________________________
StvHitLoader::StvHitLoader(const char *name) : TNamed(name,"")

{
  mHitIter = new StEventHitIter();
  mHitSelector = 0; mHitLoadActor = 0; mNDets = 0;
  memset(mMaxTimes,0,sizeof(mMaxTimes));
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
   mNDets=0;
   for (int id=1; id<kMaxDetectorId; id++){
     if (!StTGeoProxy::Inst()->IsActive((StDetectorId)id)) continue;
     mHitIter->AddDetector((StDetectorId)id);
     mNDets++;
   }
   return mNDets;
}
//_____________________________________________________________________________
int StvHitLoader::AddDetector(StDetectorId did)
{
   if (!StTGeoProxy::Inst()->IsActive((StDetectorId)did)) {
     Warning("AddDetector","DetectorId=%d not active, ignored",(int)did);
     return 13;
   }
   mHitIter->AddDetector((StDetectorId)did);
   mNDets++;
   return 0;
}
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
    int sure=0;
    int nStvHits = MakeStvHit(stHit,mHitIter->UPath(),sure);
//     if (!sure && mStvHit) { //Non reliable hit
//       double rxy = sqrt(pow(mStvHit->x()[0],2)+pow(mStvHit->x()[1],2));
//       StvDebug::Count("OrphanHits",mStvHit->x()[2],rxy);
//     }

    if (nStvHits) {
      nHits+=nStvHits;nTotHits+=nStvHits;nGits+=sure;nTotGits+=sure;
      if (mMaxTimes[mDetId]>1) mStvHit->setMaxTimes(mMaxTimes[mDetId]);
    }  
    else          {nHitz++;nTotHitz++;}
  }
  int nIniHits = tgp->InitHits();
  assert(nTotHits==nIniHits);
  Info("LoadHits","Loaded %d good, recovered %d and failed %d of all hits"
      ,nTotHits,nTotHits-nTotGits,nTotHitz);
  return nTotHits;
}

//_____________________________________________________________________________
int StvHitLoader::MakeStvHit(const StHit *stHit,UInt_t upath, int &sure)
{
static StTGeoProxy *tgh = StTGeoProxy::Inst();
static StvToolkit  *kit = StvToolkit::Inst();
   assert(stHit);
   mStvHit = 0;
 //  StDetectorId did = stHit->detector();
   StDetectorId did = mDetId;
   if (!did) {
static int knt=0;knt++;
     printf("StvHitLoader::MakeStvHit(%d) No DETID***\n",knt);
     return 0;
   }
   assert(did);
   do {	//May be errors exists
     if (did==kTpcId) 		break;
     StMatrixF errF(3,3);
     int layer = -1;
     do {
       if (did != kFtsId) 	break;
       layer = ((StRnDHit*)stHit)->layer();
       if (layer>6)		break;
       errF = Hack1to6(stHit);
     } while(0);
     if (errF[0][0]<=0) {
       errF = stHit->covariantMatrix();
     }
     assert(did!=kFtsId || errF[0][0]>1e-8);
     if (errF[0][0]+errF[1][1]+errF[2][2]<1e-8)	break;
     assert(fabs(errF[0][1]-errF[1][0])<1e-8);
     assert(fabs(errF[0][2]-errF[2][0])<1e-8);
     assert(fabs(errF[1][2]-errF[2][1])<1e-8);
     mStvHit = kit->GetHitRr();
     if (layer>6) mStvHit->SetCombo(1);
     float *e = mStvHit->errMtx();
     for (int i=0,li=0;i< 3;li+=++i) {
     for (int j=0;j<=i;j++) { e[li+j] = errF[i][j];}}
     assert(e[0]>1e-8 && e[0]<64);
     assert(e[0]*e[2]> e[1]*e[1] );
     assert(e[2]>1e-8 && e[2]<64);
     assert(e[2]*e[5]>=e[4]*e[4] );
     assert(e[5]>= 0  && e[5]<64);
     assert(e[0]*e[5]>=e[3]*e[3] );
   } while(0);

   if (!mStvHit) mStvHit= kit->GetHit();

   int idTru   = stHit->idTruth(); 
   if (idTru<0 && idTru>10000) idTru=0;
   UInt_t hard = stHit->hardwarePosition();
   if (!hard) hard = upath;
   StThreeVectorF v3f = stHit->position();
   const float *xyz = v3f.xyz();
   mStvHit->set(stHit,xyz);
   mStvHit->setIdTru(idTru);
   int seed = 1;
   if (did == kTpcId) {	// Special case for TPCHit. Prompt info added
//   enum {zPrompt = 205,rMiddle=124};
     enum {zPrompt = 205,rMiddle=0};
     hard <<=1; hard |= (fabs(xyz[2]) > zPrompt);
     if (xyz[0]*xyz[0]+xyz[1]*xyz[1] >rMiddle*rMiddle) seed = 1;
   } else {
//		the hits outside and with small mag field not for seed
     const float* x = mStvHit->x();
     assert(fabs(x[0])+fabs(x[1])>1);
     if (fabs(x[2])>250) {
       static StvToolkit *tk = StvToolkit::Inst();
       if (tk->GetHA(x)<1./1000) seed = 0;
   } }
//VP   if (mStvHit->IsCombo()) seed = 0;
   hard *= (uint)kMaxDetectorId; hard+=(uint)did;
   
   const StHitPlane *hp = tgh->AddHit(mStvHit,mDetId,xyz,hard,seed);
   sure =  tgh->IsGoodHit();
   if (!hp) { StvToolkit::Inst()->FreeHit(mStvHit); mStvHit = 0; return 0;}

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
   mStvHit->set(hp);
   
#if 0
static int nnn=0;nnn++;
printf("%d  *** StvHitLoader::MakeStvHit %g %g %g  ***\n",nnn
      ,mStvHit->x()[0],mStvHit->x()[1],mStvHit->x()[2]);
StvDebug::Count("ZHits",mStvHit->x()[2]);
StvDebug::Count("XYHits",mStvHit->x()[1],mStvHit->x()[1]);
StvDebug::Count("ZXHits",mStvHit->x()[2],mStvHit->x()[0]);
StvDebug::Count("ZYHits",mStvHit->x()[2],mStvHit->x()[1]);
#endif
   return 1;
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
//_____________________________________________________________________________
void  StvHitLoader::SetMaxTimes(int maxTimes,const char *detector)
{
  StDetectorId id = kUnknownId;
  if (detector[0]!='*') {
    id = detectorIdByName(detector);
    assert(id); 
  }
  SetMaxTimes(maxTimes,id);
}
//_____________________________________________________________________________
void  StvHitLoader::SetMaxTimes(int maxTimes,StDetectorId id)
{
  if (id) {mMaxTimes[id] = maxTimes; return; }
  for (int i=1;i<=kMaxDetectorId;i++) { mMaxTimes[i]=maxTimes;}
}

//_____________________________________________________________________________
StMatrixF  Hack1to6(const StHit *stHit)
{
//  X = R*cos(Fi), Y=R*sin(Fi), Z = z   
//   dX/dR  = (    cos(Fi)  ,sin(Fi),0)
//   dX/dFi = (-R*sin(Fi), R*cos(Fi),0)
//   dX/dZ  = (         0,         0,1)

  auto  hiPos = stHit->position();   
  auto  hiErr = stHit->positionError();   
  double Rxy = sqrt(hiPos[0]*hiPos[0]+hiPos[1]*hiPos[1]);
  double cosFi = hiPos[0]/Rxy;   
  double sinFi = hiPos[1]/Rxy;   
  double T[3][3] = {{cosFi,-Rxy*sinFi,0}
                   ,{sinFi, Rxy*cosFi,0}
                   ,{    0,         0,1}};
  double Ginp[6] = { hiErr[0]*hiErr[0]
                   ,                0,hiErr[1]*hiErr[1]
		   ,                0,                0,hiErr[2]*hiErr[2]};
  double Gout[6];		   
		   
  TCL::trasat(T[0],Ginp,Gout,3,3);
  StMatrixF mtxF(3,3);

  for (int i=0,li=0;i< 3;li+=++i) {
     for (int j=0;j<=i;j++) {mtxF[i][j] = Gout[li+j]; mtxF[j][i] = mtxF[i][j];}}
     
  return mtxF;
}
