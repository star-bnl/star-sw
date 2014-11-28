#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "StvTpcActive.h"
#include "StEvent/StEnumerations.h"
#include "TGeoManager.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "TGeoVolume.h"

#include "StDetectorDbMaker/St_tpcRDOMasksC.h"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0BC.h"


//						This is from file g2t_volume_is.g
//              nbpads = 73
//              tpads = { 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
//                        4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 
//                        7, 8, 8, 8, 9, 9, 9,10,10,10, 
//                       11,11,11,12,12,12,13,13,13,14, 
//                       14,15,16,17,18,19,20,21,22,23, 
//                       24,25,26,27,28,29,30,31,32,33, 
//                       34,35,36,37,38,39,40,41,42,43, 
//                       44,45,45};
// 
//              isdets = { 1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
// 	                   0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
// 		           2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
// 		           1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
// 			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
// 			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
// 			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 2 };
enum {kNGPads=73,kNTPads=45};
static const double kZPrompt = 205;

static const int TPADS[kNGPads]={
1, 1, 1, 2, 2, 2, 3, 3, 3, 4,
4, 4, 5, 5, 5, 6, 6, 6, 7, 7,
7, 8, 8, 8, 9, 9, 9,10,10,10,
11,11,11,12,12,12,13,13,13,14,
14,15,16,17,18,19,20,21,22,23,
24,25,26,27,28,29,30,31,32,33,
34,35,36,37,38,39,40,41,42,43,
44,45,45};

static const int RPADS[kNGPads]={
  0, 1, 0, 0, 2, 0, 0, 3, 0, 0
, 4, 0, 0, 5, 0, 0, 6, 0, 0, 7
, 0, 0, 8, 0, 0, 9, 0, 0,10, 0
, 0,11, 0, 0,12, 0, 0,13, 0, 0
,14,15,16,17,18,19,20,21,22,23
,24,25,26,27,28,29,30,31,32,33
,34,35,36,37,38,39,40,41,42,43
,44,45, 0};
static const int GPADS[kNTPads]={
  2, 5, 8,11,14,17,20,23,26,29
,32,35,38,41,42,43,44,45,46,47
,48,49,50,51,52,53,54,55,56,57
,58,59,60,61,62,63,64,65,66,67
,68,69,70,71,72};

static const int ISDETS[kNGPads]={
1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 2};

ClassImp(StvTpcActive)

//______________________________________________________________________________
StvTpcActive::StvTpcActive(const char *name):StActorFunctor(name)
{
 memset(mBeg,0,mEnd-mBeg+1);
}
//______________________________________________________________________________
int StvTpcActive::VoluId()
{

  if (GetDetId() != kTpcId) return 0;
  if (strncmp(GetVolu()->GetName(),"TPA",3)) return 0;
  int numbv[3];
  GetIPath(3,numbv);
  int tpgv  = numbv[2];
  int tpss  = numbv[1];
  mSector= tpss+12*(tpgv-1); 
  mGPad  = numbv[0];

//		tpad >nbpads (73) prompt hits
  mPrompt = 0;
  if (mGPad  > kNGPads) { mGPad -= kNGPads; mPrompt = 1; }
  mIsDet = ISDETS[mGPad-1];
  mTPad  = RPADS [mGPad-1];
  return 100000*mIsDet+100*mSector+mTPad;
}  
//______________________________________________________________________________
int StvTpcActive::operator()( const double *)
{
static St_tpcRDOMasksC   *pRdoMasks      = St_tpcRDOMasksC::instance();
static St_tpcAnodeHVavgC *tpcAnodeHVavgC = St_tpcAnodeHVavgC::instance();
static St_tpcPadGainT0BC *tpcPadGainT0BC = St_tpcPadGainT0BC::instance();


  if (!VoluId()) 	return 0;
  if (!mTPad) 		return 0;
  int iRdo  = pRdoMasks->rdoForPadrow(mTPad);
  int iact  = pRdoMasks->isOn(mSector, iRdo);
  if (!iact) return 0;

  iact = tpcAnodeHVavgC->livePadrow(mSector,mTPad)
      && tpcPadGainT0BC->livePadrow(mSector,mTPad);

  return iact;
}

ClassImp(StvTpcSelector)

//______________________________________________________________________________
StvTpcSelector::StvTpcSelector(const char *name):StvTpcActive(name)
{
  mInOut=-99;
  TString ts(name);
  if (ts.Index("Inner" ,0,TString::kIgnoreCase)>-1) mInOut =0;
  if (ts.Index("Outer" ,0,TString::kIgnoreCase)>-1) mInOut =1;
  if (ts.Index("Prompt",0,TString::kIgnoreCase)>-1) mInOut+=2;
  assert(mInOut>=0);
}
//______________________________________________________________________________
int StvTpcSelector::operator()(const double xyz[3]) 
{

  if (!VoluId()) 			return 0;
  if (!mTPad) 				return 0;
  if ((mTPad>13  ) !=  (mInOut&1    ))	return 0;
  if ((mPrompt==0) != ((mInOut&2)==0))	return 0;
  return 1;
}  
  

ClassImp(StvTpcEdit)

//______________________________________________________________________________
StvTpcEdit::StvTpcEdit():StvTpcActive("TpcEdit")
{
}
//______________________________________________________________________________
int StvTpcEdit::operator()( const double *)
{
static StTGeoProxy *tg = StTGeoProxy::Inst();
  if (!VoluId()) return 0;
  const TGeoVolume *vol = GetVolu();
  assert(vol);
  if (!tg->IsSensitive(vol)) 	return 0;
  StHitPlaneInfo* inf = tg->IsHitPlane(vol);
  if (!inf) 			return 0;
  const char *path = GetPath();
  StHitPlane *hp = inf->GetHitPlane(path);
  if (!hp) 			return 0;
  if (mTPad) 			return 0;	//Not fake volume
  hp=inf->RemHitPlane(path);
  delete hp;
  return 1;
}
//______________________________________________________________________________
int StvTpcPrompt::operator()(const double xyz[3]) 
{

  if (!VoluId()) 	return 0;
  if (!mTPad) 		return 0;
  if (!mPrompt)		return 0;
  return 123;
}  
//______________________________________________________________________________
#include "StEvent/StTpcHit.h"
#include "Stv/StvHit.h"
//		Workaround of bug in StTpcHit::padrow()
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
//		End of workaround

//______________________________________________________________________________
int StvTpcHitActor::operator()(const double xyz[3]) 
{
static int nCall = 0; nCall++;
static const int kMaxRows = St_tpcPadPlanesC::instance()->numberOfRows();
static const int iSect[24] = {23,22,21,20,19,18,17,16,15,14,13,24
                             ,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1,12};
 assert(mHit);
 TString path(GetPath());
 if (mDetId != kTpcId) return 0;
 int jTPCE = path.Index("TPCE");
 if (jTPCE<0) return 0;
 StvHit *hit = (StvHit *)mHit;
 StTpcHit *tpcHit = (StTpcHit*)hit->stHit();

 int sector = tpcHit->sector();
 if ((sector<=12) != (xyz[2]>0)) {
   sector = iSect[sector-1];// pileup tracks with wrong Z
 }
 int tpadrow = tpcHit->padrow();

//		Workaround of bug int StTpcHit::padrow()
 if (tpadrow>kMaxRows) tpadrow &= 0x3F;
//		End of workaround

 int gpadrow = GPADS[tpadrow-1];
 if (fabs(xyz[2]) > kZPrompt) gpadrow+=kNGPads;
 int tpgv = 1; if (sector>12) {tpgv = 2; sector-=12;}
 path.Remove(jTPCE+6,99);
 path+="/TPGV_"; path+=tpgv;
 path+="/TPSS_"; path+=sector;
 if (tpadrow<=13) { path+="/TPAD_";} else { path+="/TPA1_"; }
 path+=gpadrow;
 int ok = gGeoManager->cd(path);
 assert(ok);
 return 1;
}  
