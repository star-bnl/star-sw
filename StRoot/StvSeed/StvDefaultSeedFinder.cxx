#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "TCernLib.h"
#include "StvDefaultSeedFinder.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"
#include "Stv/StvHit.h"
#include "THelixTrack.h"
//#define APPROX_DEBUG
#ifdef APPROX_DEBUG
#include "TCanvas.h"
#include "TH1F.h"
#include "TProfile.h"
#endif //APPROX_DEBUG
#include "StvUtil/StvDebug.h"
#include "Stv/StvDraw.h"
void myBreak(int);
enum {kFstAng=88,kErrFakt=5,kLenFakt=5,kStpFakt=3};
static const double kFstTan = tan(kFstAng*M_PI/180);
static const double kMinTan = 0.1;
static const double kImpFakt = 0.5;
static const float  kDeltaR = 40;
static const float  kDeltaZ = 40;

ClassImp(StvDefaultSeedFinder)

//_____________________________________________________________________________
StvDefaultSeedFinder::StvDefaultSeedFinder(const char *name):StvSeedFinder(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
  fMultiHits	= new StMultiKeyMap(3);
  fMultiIter	= new StMultiKeyMapIter(0);
  f1stHitMap 	= new Stv1stHitMap;
  f1stHitMapIter= new Stv1stHitMapIter;
}  
//_____________________________________________________________________________
void StvDefaultSeedFinder::Clear(const char*)
{
  memset(mBeg,0,mMed-mBeg+1);
  f1stHitMap->clear();
  fMultiHits->Clear();
  *f1stHitMapIter = f1stHitMap->end();
  StvSeedFinder::Clear();
}
//_____________________________________________________________________________
void StvDefaultSeedFinder::Reset()
{
  memset(mBeg,0,mMed-mBeg+1);
  const StVoidArr *hitArr =  StTGeoHelper::Inst()->GetSeedHits();
  int nHits =  hitArr->size();
  for (int iHit=0;iHit<nHits;iHit++) {
    StvHit *stiHit = (StvHit*)(*hitArr)[iHit];
    const float *x = stiHit->x();
//    float r2 = x[0]*x[0] + x[1]*x[1]+ x[2]*x[2];
    float r2 = x[0]*x[0] + x[1]*x[1] + x[2]*x[2];
    f1stHitMap->insert(std::pair<float,StvHit*>(-r2, stiHit));
    fMultiHits->Add(stiHit,x);
  } 
  fMultiHits->MakeTree();
  *f1stHitMapIter = f1stHitMap->begin();
}    
//_____________________________________________________________________________
int StvDefaultSeedFinder::Again()
{
  *f1stHitMapIter = f1stHitMap->begin();
   return 1;
}
//_____________________________________________________________________________
//	Start of Local auxiliary routines
inline static void Lagrange3Int (float t,float T1,float T2,float coe[3])
{
   coe[0]=  (t-T1)*(t-T2)/(T1*(T2   )); 
   coe[1]= -(t   )*(t-T2)/(T1*(T2-T1)); 
   coe[2]=  (t   )*(t-T1)/(T2*(T2-T1));
}
//_____________________________________________________________________________
inline static void ZLine3Int (float t,float T1,float T2,float coe[3])
{
  float aT  = (T1   +T2   )/3;
  float aTT = (T1*T1+T2*T2)/3;
  float det = aTT-aT*aT;
  coe[0] = (1. + (  -aT)*(t-aT)/det)/3;
  coe[1] = (1. + (T1-aT)*(t-aT)/det)/3;
  coe[2] = (1. + (T2-aT)*(t-aT)/det)/3;
}  

//_____________________________________________________________________________
inline static void Lagrange3Der (float t,float T1,float T2,float coe[3])
{
   coe[0]=  ((t-T1)+(t-T2))/(T1*(T2   )); 
   coe[1]= -((t   )+(t-T2))/(T1*(T2-T1)); 
   coe[2]=  ((t   )+(t-T1))/(T2*(T2-T1));
}
//_____________________________________________________________________________
inline static void ZLine3Der (float T1,float T2,float coe[3])
{
  float aT  = (T1   +T2   )/3;
  float aTT = (T1*T1+T2*T2)/3;
  float det = aTT-aT*aT;
  coe[0] = (  -aT)/det/3;
  coe[1] = (T1-aT)/det/3;
  coe[2] = (T2-aT)/det/3;
}  
//_____________________________________________________________________________
inline static float Dot(const float dir[3],const float pnt[3])
{
   return dir[0]*pnt[0]+dir[1]*pnt[1]+dir[2]*pnt[2];
}
//_____________________________________________________________________________
inline static float Impact2(const float dir[3],const float pnt[3])
{
   float imp[3];
   imp[0] = dir[1]*pnt[2]-dir[2]*pnt[1];
   imp[1] = dir[2]*pnt[0]-dir[0]*pnt[2];
   imp[2] = dir[0]*pnt[1]-dir[1]*pnt[0];
   return imp[0]*imp[0]+imp[1]*imp[1]+imp[2]*imp[2];
}
//	End of Local auxiliary routines
//_____________________________________________________________________________
const THelixTrack* StvDefaultSeedFinder::NextSeed()
{
static int myDeb = 0;
std::vector<TObject*> mySeedObjs;

  StvHit *fstHit,*selHit=0; 
  mSel.SetXYStep(kDeltaR);
  mSel.SetZStep (kDeltaZ);

  for (;(*f1stHitMapIter)!=f1stHitMap->end();++(*f1stHitMapIter)) {//1st hit loop
    fstHit = (*(*f1stHitMapIter)).second;
    assert(fstHit);
    const float *fstX = fstHit->x();
    int myBreak = StvDebug::Break(fstX[0],fstX[1],fstX[2]); if(myBreak){}

    if (fstHit->timesUsed()) continue;

    fSeedHits.clear();
    mSel.Reset();
if (myDeb>0) {fDraw->Clear();mySeedObjs.clear();}
    selHit = fstHit;
    mSel.SetErr(sqrt(fstHit->err2())*kErrFakt);

    int selJkk = -11;

    while (1) { //Search next hit 
//		Add info from selected hit
      fSeedHits.push_back(selHit); selHit->addTimesUsed();fNUsed[0]++;
if (selJkk>=0) printf("***Selected*** selJkk = %d\n",selJkk);

      mSel.AddHit(selHit->x());
      mSel.Prepare();
      fMultiIter->Set(fMultiHits->GetTop(),mSel.mLim[0],mSel.mLim[1]);
      selHit=0; 
      selJkk = -11;
      float minLen = 1e11;
      TObject *selObj=0;	//This guy for graphics only
      for (StMultiKeyNode *node=0;(node = *(*fMultiIter)) ;++(*fMultiIter)) 
      { 
	StvHit *nexHit = (StvHit*)node->GetObj();
        if (nexHit->timesUsed()) continue;
	int ans = mSel.Reject(nexHit->x());
	if (ans>0) continue;
        if (ans<0) fMultiIter->Update(mSel.mLim[0],mSel.mLim[1]);
	if (minLen>mSel.mHitLen) { //Selecting the best
          delete selObj; minLen=mSel.mHitLen; selHit=nexHit;

        } //endSelecting the best
      } //endMultiIter loop

      if (!selHit) break; //No more hits 

if (myDeb>0) { mySeedObjs.push_back(selObj);}

      if (fSeedHits.size()>=kMaxHits) break;
    }// endNext hit loop

//		Mark hits as unused when seed is created. Only tracker
//		has right to deside to use or not to use
    fSeedHits.unused();
    const THelixTrack *hel = 0;
    if (fSeedHits.size() >= kMinHits) hel = Approx();
    if (hel) { fNSeeds[0]++; ++(*f1stHitMapIter); return hel;}		//Good boy
 //		Bad seed
    fNUsed[0] -= fSeedHits.size();

if (myDeb>0){for (int i=0;i<(int)mySeedObjs.size();i++) {delete mySeedObjs[i];}}

  }// end 1st hit loop
  fNSeeds[1]+=fNSeeds[0]; fNUsed[1]+= fNUsed[0];
  return 0;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
StvConeSelector::StvConeSelector()
{
  memset(mBeg,0,mBeg-mBeg+1);
}
//_____________________________________________________________________________
void StvConeSelector::AddHit(const float *x)
{
  mX[++mJst]=x;
  mHit = x;
  assert(mJst<100);
}
//_____________________________________________________________________________
void StvConeSelector::Prepare()
{
static int nCall=0; nCall++;
StvDebug::Break(nCall);

  float stp=0;
  int kase = mJst; if (kase>2) kase = 2;

  switch(kase) {
  
    case 0: {
      for (int i=0;i<3;i++) {mDir[i]=-mHit[i];}
      stp=0;
      for (int i=0;i<3;i++) {stp+=mDir[i]*mDir[i];}
      stp = sqrt(stp);
      for (int i=0;i<3;i++) {mDir[i]/=stp;}
      mS[0]=0;
      mTan = kFstTan;
    }; break;

    case 1: {
      stp=0;
      for (int i=0;i<3;i++) {mDir[i]=mHit[i]-mX[mJst-1][i]; stp+=mDir[i]*mDir[i];}
      stp = sqrt(stp );
      for (int i=0;i<3;i++) {mDir[i]/=stp;}
      mS[1]=stp;
      mTan = mErr/stp;
    }; break;


    case 2: {
      stp=0;
      for (int i=0;i<3;i++) {float qwe=mHit[i]-mX[mJst-1][i]; stp+=qwe*qwe;}
      stp = sqrt(stp );
      mS[mJst]=stp;
      mTan = mErr/(mS[mJst]+mS[mJst-1])/sqrt(3.);
      float T1 = mS[mJst-1],T2 = mS[mJst]+T1;
      float coe[3];
      Lagrange3Der (T2,T1,T2,coe);

      mDir[0] = coe[0]*mX[mJst-2][0]+coe[1]*mX[mJst-1][0]+coe[2]*mHit[0];
      mDir[1] = coe[0]*mX[mJst-2][1]+coe[1]*mX[mJst-1][1]+coe[2]*mHit[1];
      mDir[2] = coe[0]*mX[mJst-2][2]+coe[1]*mX[mJst-1][2]+coe[2]*mHit[2];
      stp=0;
      for (int i=0;i<3;i++) {stp+=mDir[i]*mDir[i];}
      stp = sqrt(stp );
      for (int i=0;i<3;i++) {mDir[i]/=stp;}
    }; break;

    default: assert(0 && "Wrong case");
  }
  mRxy2 = mHit[0]*mHit[0]+mHit[1]*mHit[1];
   
  mLen=0;
  if (mXYStep>0) {
    float cosLa = sqrt((1.-mDir[2])*(1+mDir[2]));
    if (cosLa<1./kLenFakt) cosLa= 1./kLenFakt;
    mLen = mXYStep/cosLa;
  }
  if (mZStep>0) {
    float sinLa = fabs(mDir[2]);
    if (sinLa<1./kLenFakt) sinLa= 1./kLenFakt;
    double myLen = mZStep/sinLa;
    if (mLen <myLen) mLen = myLen;
  }
  assert(mLen>0);


  UpdateLims();

}   
//_____________________________________________________________________________
void  StvConeSelector::UpdateLims()
{
  for (int i=0;i<3;i++) {
    float qwe = mLen*mDir[i];
    float asd = mLen*mTan*sqrt(fabs(1-mDir[i]*mDir[i]));
    float lim = qwe - asd - mErr;
    mLim[0][i] = (qwe<-mErr) ? qwe:-mErr;
    lim = qwe + asd + mErr;
    mLim[1][i] = (qwe> mErr) ? qwe: mErr;
    mLim[0][i]+=mHit[i]; mLim[1][i]+=mHit[i];
  }

  for (int j=0;j<3;j++) {
    float xx = mHit[j]+mDir[j]*mLen*0.1;
    assert(xx>=mLim[0][j]);
    assert(xx<=mLim[1][j]);
          xx = mHit[j]+mDir[j]*mLen*0.9;
    assert(xx>=mLim[0][j]);
    assert(xx<=mLim[1][j]);
  }

//  for (int j=0;j<3;j++) {mLim[0][j]=-999; mLim[1][j]=999;}



}
//_____________________________________________________________________________
int  StvConeSelector::Reject(const float x[3])
{
   float myRxy2 = x[0]*x[0]+x[1]*x[1];
   if (myRxy2>mRxy2 && fabs(x[2])>fabs(mHit[2])) 	return 1;
   if (myRxy2>mRxy2 ) 					return 2;
   float xx[3] = {x[0]-mHit[0],x[1]-mHit[1],x[2]-mHit[2]};
   float r2xy = xx[0]*xx[0]+xx[1]*xx[1];
   float z2 = xx[2]*xx[2];
   if (r2xy < (kMinTan*kMinTan)*z2) 	return 3;		
   mHitLen = (r2xy+z2);
   if (mHitLen  < 1e-8) 		return 4;
   mHitPrj = Dot(xx,mDir);
   if (mHitPrj<1e-4)			return 5;
   if (mHitPrj>mLen) 			return 6;
   float imp = (Impact2(mDir,xx));
   float lim = (mErr) + mHitPrj*mTan;
   if (imp > lim*lim)          		return 7;
   mHitLen = imp*kImpFakt+ r2xy*(1-kImpFakt);
   return -1;
}
//_____________________________________________________________________________
void StvConeSelector::Update()
{
   mLen = mHitPrj;
   UpdateLims();
}


