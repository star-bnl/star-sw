#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "TCernLib.h"
#include "StvDefaultSeedFinder.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "Stv/StvHit.h"
#include "THelixTrack.h"
//#define APPROX_DEBUG
#ifdef APPROX_DEBUG
#include "TCanvas.h"
#include "TH1F.h"
#include "TProfile.h"
#endif //APPROX_DEBUG
#include "StvSeedConst.h"
#include "StvUtil/StvDebug.h"
#include "Stv/StvDraw.h"

void myBreak(int);
enum {kFstAng=74,kErrFakt=5,kLenFakt=5};
static const double kFstTan = tan(kFstAng*M_PI/180);
static const double kMinTan = 0.1;

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
  const StVoidArr *hitArr =  StTGeoProxy::Inst()->GetSeedHits();
  int nHits =  hitArr->size();
  for (int iHit=0;iHit<nHits;iHit++) {
    StvHit *stiHit = (StvHit*)(*hitArr)[iHit];
    const float *x = stiHit->x();
//    float r2 = x[0]*x[0] + x[1]*x[1]+ x[2]*x[2];
    float r2 = x[0]*x[0] + x[1]*x[1] + 1e-3*x[2]*x[2];
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

  for (;(*f1stHitMapIter)!=f1stHitMap->end();++(*f1stHitMapIter)) {//1st hit loop
    fstHit = (*(*f1stHitMapIter)).second;
    assert(fstHit);
    const float *fstX = fstHit->x();
    int myBreak = StvDebug::Break(fstX[0],fstX[1],fstX[2]); if(myBreak){}

    if (fstHit->timesUsed()) continue;

    fSeedHits.clear();
    mSel.Reset();
    selHit = fstHit;
    mSel.SetErr(sqrt(fstHit->err2())*kErrFakt);

    while (1) { //Search next hit 
//		Add info from selected hit
      fSeedHits.push_back(selHit); selHit->addTimesUsed();fNUsed[0]++;
      if (fSeedHits.size()>=kMaxHits) break;
      const StHitPlane *hp = selHit->detector();
      const float *hd = hp->GetDir(selHit->x())[0];
      mSel.AddHit(selHit->x(),hd,hp->GetLayer());
      mSel.Prepare();
      fMultiIter->Set(fMultiHits->GetTop(),mSel.mLim[0],mSel.mLim[1]);
      selHit=0; 
      TObject *selObj=0;	//This guy for graphics only
      for (StMultiKeyNode *node=0;(node = *(*fMultiIter)) ;++(*fMultiIter)) 
      { 
	StvHit *nexHit = (StvHit*)node->GetObj();
        if (nexHit->timesUsed()) 	continue;
        const StHitPlane *hpNex = nexHit->detector();
        if (hpNex==hp) 	continue;
	int ans = mSel.Reject(nexHit->x(),hpNex);
	if (ans>0) continue;
//			Selecting the best
        selHit=nexHit;
        if (!ans)  continue;
//		Decrease size of searching box
	mSel.Update();
	fMultiIter->Update(mSel.mLim[0],mSel.mLim[1]);
      } //endMultiIter loop

      if (!selHit) break; //No more hits 

if (myDeb>0) { mySeedObjs.push_back(selObj);}

    }// endNext hit loop

//		Mark hits as unused when seed is created. Only tracker
//		has right to deside to use or not to use
    fSeedHits.unused();
    for (int j=0;j<(int)fSeedHits.size();j++) {
      assert(!fSeedHits[j]->timesUsed());
    }


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
void StvConeSelector::AddHit(const float *x,const float *dir,float layer)
{
  mMinPrj = 1.e11; mMinImp = 1.e11; mHp = 0;
  mX[++mJst]=x;
  mHit = x;
  mLayer = layer;
  mHitDir = dir;
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
      

      for (int i=0;i<3;i++) {mDir[i]=-mHitDir[i];}
      float sgn = Dot(mHit,mDir);
      assert(sgn<0);
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
assert(fabs(Dot(mDir,mDir)-1)<1e-5);
  mRxy2 = mHit[0]*mHit[0]+mHit[1]*mHit[1];
  mRxy = sqrt(mRxy2);
  mDelta = SEED_ERR(mRxy);
  mLen= mLayer*kLenFakt/(fabs(Dot(mHitDir,mDir))+1e-10);
  UpdateLims();

}   
//_____________________________________________________________________________
void  StvConeSelector::UpdateLims()
{
  for (int i=0;i<3;i++) {
    float qwe = mLen*mDir[i];
    float asd = mLen*mTan*sqrt(fabs(1-mDir[i]*mDir[i]));
    float lim = qwe - asd - mErr;
    mLim[0][i] = (lim<0)? lim:-mErr;
    lim = qwe + asd + mErr;
    mLim[1][i] = (lim>0)? lim: mErr;
//		Move to global system 
    mLim[0][i]+= mHit[i];
    mLim[1][i]+= mHit[i];
  }

//		Account that all the hits inside of cylinder with Rxy
  for (int i=0;i<2;i++) {
    if (mLim[0][i]<-mRxy) mLim[0][i]=-mRxy;
    if (mLim[1][i]> mRxy) mLim[1][i]= mRxy;
  }

}
//_____________________________________________________________________________
int  StvConeSelector::Reject(const float x[3],const void* hp)
{
   if (x[0]*x[0]+x[1]*x[1]>mRxy2) return 1;

   float xx[3] = {x[0]-mHit[0],x[1]-mHit[1],x[2]-mHit[2]};

   float r2xy = xx[0]*xx[0]+xx[1]*xx[1];
   float z2 = xx[2]*xx[2];
   if (r2xy < (kMinTan*kMinTan)*z2) 	return 3;		
   mHitLen = (r2xy+z2);
   if (mHitLen  < 1e-8) 		return 4;
   mHitPrj = Dot(xx,mDir);
   if (mHitPrj>mLen) 			return 6;	//Outside of cone along
   float imp =mHitLen-mHitPrj*mHitPrj; if (imp<=0) imp = 0;
   float lim = (mErr) + mHitPrj*mTan;
   if (imp > lim*lim)          		return 7;	//Outside of cone aside
   int ans = 99;
   if (mHp != hp) { 					//different layers, only prj is important
     if (mHitPrj>mMinPrj) 		return 8;	//more far than best,along
     ans = -1;
   } else         {					//same layer, only impact is important
     if (imp>mMinImp) 			return 9;	//same plane but impact bigger
     ans = 0;
   }
   mMinPrj= mHitPrj; mMinImp=imp; mHp = hp;
   return ans;						//impact best but cone the same
}
//_____________________________________________________________________________
void StvConeSelector::Update()
{
   mLen = mHitPrj;
   UpdateLims();
}


