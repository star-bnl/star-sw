#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "StvTpcActive.h"
#include "StEvent/StEnumerations.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "TGeoVolume.h"

#include "StDetectorDbMaker/St_tpcRDOMasksC.h"

ClassImp(StvTpcActive)

//______________________________________________________________________________
StvTpcActive::StvTpcActive(const char *name):StActorFunctor(name)
{
 memset(mBeg,0,mEnd-mBeg+1);
}
//______________________________________________________________________________
int StvTpcActive::VoluId()
{
enum {kNbPads=73};
// static const int tpads[kNbPads]={
//                              1, 1, 1, 2, 2, 2, 3, 3, 3, 4,
// 			     4, 4, 5, 5, 5, 6, 6, 6, 7, 7,
// 			     7, 8, 8, 8, 9, 9, 9,10,10,10,
// 			    11,11,11,12,12,12,13,13,13,14,
// 			    14,15,16,17,18,19,20,21,22,23,
// 			    24,25,26,27,28,29,30,31,32,33,
// 			    34,35,36,37,38,39,40,41,42,43,
// 			    44,45,45};

static const int tpads[kNbPads]={
                             0, 1, 0, 0, 2, 0, 0, 3, 0, 0,
			     4, 0, 0, 5, 0, 0, 6, 0, 0, 7,
			     0, 0, 8, 0, 0, 9, 0, 0,10, 0,
			     0,11, 0,12, 0, 0, 0,13, 0, 0,
			    14,15,16,17,18,19,20,21,22,23,
			    24,25,26,27,28,29,30,31,32,33,
			    34,35,36,37,38,39,40,41,42,43,
			    44,45, 0};
static const int isdets[kNbPads]={
                             1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			     0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
			     2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
			     1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 2};

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
  if (mGPad  > kNbPads) { mGPad -= kNbPads; mPrompt = 1; }
  mIsDet = isdets[mGPad-1];
  mTPad  = tpads [mGPad-1];
  return 100000*mIsDet+100*mSector+mTPad;
}  
//______________________________________________________________________________
int StvTpcActive::operator()( const double *)
{
static St_tpcRDOMasksC *pRdoMasks = St_tpcRDOMasksC::instance();
  if (!VoluId()) 	return 0;
  if (!mTPad) 		return 0;
  int iRdo  = pRdoMasks->rdoForPadrow(mTPad);
  int iact  = pRdoMasks->isOn(mSector, iRdo);
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
