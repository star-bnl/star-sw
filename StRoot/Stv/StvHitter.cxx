#include "StvHitter.h"
#include "Stv/StvHit.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "StvHitter.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "StvUtil/StvNodePars.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StarRoot/StMultiKeyMap.h"

//_____________________________________________________________________________
//_____________________________________________________________________________
StvHitter::StvHitter()
{
  fMultiIter = new StMultiKeyMapIter(0);
}
//_____________________________________________________________________________
StvHitter::~StvHitter()
{
  delete fMultiIter;
}
//_____________________________________________________________________________
void StvHitter::Reset()
{
  mHitPlane=0;
}
//_____________________________________________________________________________
const StvHits *StvHitter::GetHits(const StvNodePars *pars
                                 ,const StvFitErrs  *errs, const float gate[4])
{
enum {kLittleBit = 5};

static int nCall=0; nCall++;
static StTGeoProxy * const myProxy = StTGeoProxy::Inst();
  mHits.clear();
  const StHitPlane *myHitPlane = myProxy->GetCurrentHitPlane();
  if (!myHitPlane) 		return 0;	//no sensitive volume there

  if (mHitPlane == myHitPlane)  return 0;	//hit plane was already used
  int nHits = myHitPlane->GetNHits();
  if (!nHits)	return &mHits;	//it is sensitive but no hits

  mHitPlane = myHitPlane;

  mHitMap  = mHitPlane->GetHitMap();
  float xNode[3]={(float)pars->_x,(float)pars->_y,(float)pars->_z};
  mOrg =  mHitPlane->GetOrg(xNode);
  mDir = &mHitPlane->GetDir(xNode);


  float tau =0,den=0;
  float const *ort =(*mDir)[0];
  float mom[3]={(float)pars->_cosCA,(float)pars->_sinCA,(float)pars->_tanl};

  for (int j=0;j<3;j++) {tau+=(mOrg[j]-xNode[j])*ort[j];
                         den+= mom[j]           *ort[j];}
  if (fabs(den)<1e-2) 		return 0; // track is parallel to hit plane
  tau/=den;
  for (int j=0;j<3;j++) {xNode[j]+=mom[j]*tau;}

  double myGate[2];
//		Obtain track errs
  double cos2 = den*den/(1.+mom[2]*mom[2]);

  double totRr = (errs->mHH+errs->mZZ)/cos2;
  totRr = sqrt(totRr);
  for (int j=0;j<2;j++) {
    myGate[j] = gate[j]*totRr;
    if (myGate[j]>gate[j+2]) myGate[j]=gate[j+2];}

  float lNode[3],lim[2][2];
  mHitPlane->ToLocal(xNode,lNode);
  for (int j=0;j<2;j++) {lim[0][j]=lNode[1+j]-myGate[j];
                         lim[1][j]=lNode[1+j]+myGate[j];}
  
  fMultiIter->Set(mHitMap->GetTop(),lim[0],lim[1]);
  for (StMultiKeyNode *node=0;(node = *(*fMultiIter)) ;++(*fMultiIter)) 
  { 
    StvHit *nexHit = (StvHit*)node->GetObj();
    if (nexHit->isUsed()) continue;
    mHits.push_back(nexHit);
  }
  return &mHits;
}
//_____________________________________________________________________________
const StHitPlane*  StvHitter::GetHitPlane() const {return mHitPlane;}

//_____________________________________________________________________________
StDetectorId StvHitter::GetDetId() const
{ 
  if (!mHitPlane) return (StDetectorId)0;
  return mHitPlane->GetDetId();
}


