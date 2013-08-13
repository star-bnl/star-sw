#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "TCernLib.h"
#include "StiDefaultSeedFinder.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"
#include "Sti/StiHit.h"
#include "THelixTrack.h"
//#define APPROX_DEBUG
#ifdef APPROX_DEBUG
#include "TCanvas.h"
#include "TH1F.h"
#include "TProfile.h"
#endif //APPROX_DEBUG
#include "Sti/StiDraw.h"
void myBreak(int);
enum {kMinHits=5,kMaxHits = 115,kFstAng=80,kErrFakt=5,kLenFakt=5,kStpFakt=3};
static const double kMinCos = 0.1;

ClassImp(StiDefaultSeedFinder)
static const float TpcOuterDeltaR = 10, kTpcHitErr = 0.2;

//_____________________________________________________________________________
StiDefaultSeedFinder::StiDefaultSeedFinder(const char *name):StiSeedFinder(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
  fMultiHits	= new StMultiKeyMap(3);
  fMultiIter	= new StMultiKeyMapIter(0);
  f1stHitMap 	= new Sti1stHitMap;
  f1stHitMapIter= new Sti1stHitMapIter;
}  
//_____________________________________________________________________________
void StiDefaultSeedFinder::Clear(const char*)
{
  memset(mBeg,0,mMed-mBeg+1);
  f1stHitMap->clear();
  fMultiHits->Clear();
  *f1stHitMapIter = f1stHitMap->end();
  StiSeedFinder::Clear();
}
//_____________________________________________________________________________
void StiDefaultSeedFinder::Reset()
{
  memset(mBeg,0,mMed-mBeg+1);
  const StVoidArr *hitArr =  StTGeoHelper::Inst()->GetSeedHits();
  int nHits =  hitArr->size();
  for (int iHit=0;iHit<nHits;iHit++) {
    StiHit *stiHit = (StiHit*)(*hitArr)[iHit];
    const float *x = stiHit->x_g();
//    float r2 = x[0]*x[0] + x[1]*x[1]+ x[2]*x[2];
    float r2 = x[0]*x[0] + x[1]*x[1];
    f1stHitMap->insert(pair<float,StiHit*>(-r2, stiHit));
    fMultiHits->Add(stiHit,x);
  } 
  fMultiHits->MakeTree();
  *f1stHitMapIter = f1stHitMap->begin();
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
const THelixTrack* StiDefaultSeedFinder::NextSeed()
{
static int myDeb = 0;
std::vector<TObject*> mySeedObjs;

  StiHit *fstHit,*lstHit,*selHit=0; 
  float deltaR = TpcOuterDeltaR;
  for (;(*f1stHitMapIter)!=f1stHitMap->end();++(*f1stHitMapIter)) {//1st hit loop
    fstHit = (*(*f1stHitMapIter)).second;
    assert(fstHit);


    if (fstHit->timesUsed()) continue;

    fSeedHits.clear();
if (myDeb>0) {fDraw->Clear();mySeedObjs.clear();}
    float  L[100];L[0]=0; 
    float step=0,stepSum=0,stepXY;
    const float *X[100];
    selHit = fstHit;

if(myDeb>0) { 
const float *p=X[0];
TObject* fstObj =fDraw->Point(p[0],p[1],p[2],kUsedHit);
mySeedObjs.push_back(fstObj);
fDraw->UpdateModified();StiDraw::Wait();
}
    float *D = mSel.mDir; 
    while (1) { //Search next hit 
//		Add info from selected hit
      fSeedHits.push_back(selHit); selHit->addTimesUsed();fNUsed[0]++;
      lstHit=selHit;
      int jst = fSeedHits.size()-1;
      X[jst] = lstHit->x_g();
      mSel.mX = X[jst];
      mSel.mRxy2 = mSel.mX[0]*mSel.mX[0] + mSel.mX[1]*mSel.mX[1];
      if (jst) {
	const float *x0 = X[jst-1];
	const float *x1 = X[jst  ];
	for (int i=0;i<3;i++) { D[i]=x1[i]-x0[i];}
        stepXY = D[0]*D[0]+D[1]*D[1];
        step = sqrt(stepXY+D[2]*D[2]); stepXY=sqrt(stepXY);
        stepSum += step;
        L[jst] = L[jst-1]+stepXY;	    
      }


      switch (fSeedHits.size()) {

        case 1: {//1 hit. Looking second Hit
	  float r = sqrt(mSel.mRxy2+mSel.mX[2]*mSel.mX[2]);
     	  D[0]= -mSel.mX[0]/r;
     	  D[1]= -mSel.mX[1]/r;
     	  D[2]= -mSel.mX[2]/r;
     	  mSel.mLen[0] = deltaR/sqrt((1.-D[2])*(1+D[2]));
     	  mSel.mLen[1] = mSel.mLen[0]*3;
     	  mSel.mAng = M_PI/180	*kFstAng;
	} ;break;

        case 2: {//2 hits. Looking 3rd  one
     	  for (int i=0;i<3;i++) { D[i]/=step;}
          float cosLa = sqrt((1.-D[2])*(1+D[2]));
          assert (cosLa > 0.9*kMinCos);
	  mSel.mLen[0] = deltaR/cosLa;
     	  mSel.mLen[1] = mSel.mLen[0]	*kLenFakt;
	  float bigStep = step*kStpFakt;
          if (mSel.mLen[1]>bigStep) mSel.mLen[1]=bigStep;
     	  mSel.mAng = kTpcHitErr/step*kErrFakt;
	} ;break;

        default: {//3+ hits. Looking one more

          float T1 = L[jst-1],T2 = L[jst];
          float coe[3];
	  Lagrange3Der (T2,T1,T2,coe);
          D[0] = coe[0]*X[jst-2][0]+coe[1]*X[jst-1][0]+coe[2]*X[jst][0];
          D[1] = coe[0]*X[jst-2][1]+coe[1]*X[jst-1][1]+coe[2]*X[jst][1];
          ZLine3Der    (   T1,T2,coe);
          D[2] = coe[0]*X[jst-2][2]+coe[1]*X[jst-1][2]+coe[2]*X[jst][2];
          float nor = sqrt(D[0]*D[0]+D[1]*D[1]+D[2]*D[2]);
          D[0]/=nor;D[1]/=nor;D[2]/=nor;
     	  float cosLa = sqrt((1.-D[2])*(1+D[2]));
          assert (cosLa > 0.9*kMinCos) ;
	  mSel.mLen[0] = deltaR/cosLa;
     	  mSel.mLen[1] = mSel.mLen[0]	*kLenFakt;
	  float bigStep = (stepSum/jst)*kStpFakt;
          if (mSel.mLen[1]>bigStep) mSel.mLen[1]=bigStep;
     	  mSel.mAng = kTpcHitErr/stepSum*kErrFakt;

        }
      }//end switch

      mSel.Prepare();
      fMultiIter->Set(fMultiHits->GetTop(),mSel.mLim[0],mSel.mLim[1]);
      selHit=0; 
      float minLen = 1e11;
      TObject *selObj=0;
      for (StMultiKeyNode *node=0;(node = *(*fMultiIter)) ;++(*fMultiIter)) 
      { 
	StiHit *nexHit = (StiHit*)node->GetObj();
        if (nexHit->timesUsed()) continue;
	int ans = mSel.Reject(nexHit->x_g());
	if (ans>0) continue;
	if (ans<0) fMultiIter->Update(mSel.mLim[0],mSel.mLim[1]);
	if (minLen>mSel.mHitLen) { //Selecting the best
          delete selObj; minLen=mSel.mHitLen; selHit=nexHit;
if(myDeb>0) {
const float *p=nexHit->x_g();
selObj = fDraw->Point(p[0],p[1],p[2],kUsedHit);
fDraw->UpdateModified();fDraw->Wait();
} 
        } //endSelecting the best
      } //endMultiIter loop

      if (!selHit) break; //No more hits 

if (myDeb>0) { mySeedObjs.push_back(selObj);}

      if (fSeedHits.size()>=kMaxHits) break;
    }// endNext hit loop

    const THelixTrack *hel = 0;
    if (fSeedHits.size() >= kMinHits) hel = Approx();
    if (hel) { fNSeeds[0]++; ++(*f1stHitMapIter); return hel;}		//Good boy
 //		Bad seed
    fSeedHits.unused();
    fNUsed[0] -= fSeedHits.size();

if (myDeb>0){for (int i=0;i<(int)mySeedObjs.size();i++) {delete mySeedObjs[i];}}

  }// end 1st hit loop
  fNSeeds[1]+=fNSeeds[0]; fNUsed[1]+= fNUsed[0];
  return 0;
}
//_____________________________________________________________________________
const THelixTrack *StiDefaultSeedFinder::Approx()
{
static int nCall=0; nCall++;

#ifdef APPROX_DEBUG
static TCanvas *myCanvas=0;
static TH1  *H[2];
if(!myCanvas) {
   myCanvas=new TCanvas("Approx","",600,800);
   H[0] = new TH1F("Xi2","Xi2", 100,0,5);
   H[1] = new TProfile("nHits ","nHits",  30,0,30);
   myCanvas->Divide(1,2);
   for (int i=0;i<2;i++) {myCanvas->cd(i+1); H[i]->Draw();}
}
#endif // APPROX_DEBUG

const double BAD_XI2=333*kTpcHitErr*kTpcHitErr;
const double BAD_RHO=0.1;

//		Loop over nodes and collect global xyz

  THelixFitter &circ = fHelix;
  circ.Clear();
  int nNode=fSeedHits.size();
  for (int iNode = 0; iNode<nNode;++iNode) {
    const StiHit * hit = fSeedHits[iNode];
    circ.Add(hit->x_g()[0],hit->x_g()[1],hit->x_g()[2]);
  }  
  double Xi2 =circ.Fit();
//   float myDir[3];
//   TCL::vsub(fSeedHits[nNode-1]->x_g(),fSeedHits[0]->x_g(),myDir,3);
//   assert(myDir[0]*circ.Dir()[0]+myDir[1]*circ.Dir()[1]>0);

#ifdef APPROX_DEBUG
  H[0]->Fill(log(Xi2)/log(10.));
  H[1]->Fill(nNode,Xi2);
#endif // APPROX_DEBUG
  if (Xi2>BAD_XI2) return 0; //Xi2 too bad, no updates
  if (fabs(circ.GetRho()) >BAD_RHO) return 0;

  const float *fx = fSeedHits[0]->x_g();
  const double dx[3]={fx[0],fx[1],fx[2]};
  double l = fHelix.Path(dx); fHelix.Move(l);
  return &fHelix;
}    
//_____________________________________________________________________________
//_____________________________________________________________________________
StiDefaultSelector::StiDefaultSelector()
{
  memset(mBeg,0,mBeg-mBeg+1);
}
//_____________________________________________________________________________
void StiDefaultSelector::Prepare()
{
  memset(mLim[0],0,sizeof(mLim));
  if (mAng <0.3) {mSin = mAng     ;mCos = 1-mAng*mAng/2;}
  else           {mSin = sin(mAng);mCos = sqrt((1.-mSin)*(1.+mSin));}

  for (int i=0;i<3;i++) {
    mDelta[i] = sqrt(mDir[(i+1)%3]*mDir[(i+1)%3]+mDir[(i+2)%3]*mDir[(i+2)%3]);
    mDelta[i] *=mSin;
  }
  UpdateLims();

}   
//_____________________________________________________________________________
void  StiDefaultSelector::UpdateLims()
{
  memset(mLim[0],0,sizeof(mLim));
  for (int i=0;i<3;i++) {
    float bnd = (mDir[i]+mDelta[i])*mLen[1];
    if (mLim[0][i]>bnd) mLim[0][i]=bnd;
    if (mLim[1][i]<bnd) mLim[1][i]=bnd;

    bnd = (mDir[i]-mDelta[i])*mLen[1];
    if (mLim[0][i]>bnd) mLim[0][i]=bnd;
    if (mLim[1][i]<bnd) mLim[1][i]=bnd;

    mLim[0][i]+=mX[i]; mLim[1][i]+=mX[i];
  }
}
//_____________________________________________________________________________
int  StiDefaultSelector::Reject(const float x[3])
{
   float myRxy2 = x[0]*x[0]+x[1]*x[1];
   if (myRxy2>mRxy2 && fabs(x[2])>fabs(mX[2])) 	return 1;
   if (myRxy2>mRxy2                          ) 	return 2;
   float xx[3] = {x[0]-mX[0],x[1]-mX[1],x[2]-mX[2]};
   float r2xy = xx[0]*xx[0]+xx[1]*xx[1];
   float z2 = xx[2]*xx[2];
   if (r2xy < (kMinCos*kMinCos)*z2) 		return 3;		
   mHitLen = (r2xy+z2);
   if (mHitLen  < 1e-8) 			return 4;
   float imp2 = Impact2(mDir,xx);
   float lim2 = (kTpcHitErr*kErrFakt)*(kTpcHitErr*kErrFakt);
   lim2 += mHitLen*mSin*mSin;
   if (imp2 > lim2)          	return 5;
   mHitLen = sqrt(mHitLen);
   if (mHitLen>mLen[1]) 	return 6;
   if (mLen[1]<=mLen[0]) 	return 0;

   mLen[1] = (mHitLen>mLen[0])? mHitLen:mLen[0];
   UpdateLims();
   return -1;
}
