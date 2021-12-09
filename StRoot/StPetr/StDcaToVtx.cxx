#include <assert.h>
#include "StDcaToVtx.h"
#include "THelixTrack.h"
#include "TGeoMaterial.h"
#include "StvELossTrak.h"

//_____________________________________________________________________________
 StDcaToVtx::StDcaToVtx()
{
  mHlx = new THelixTrack();
  mELoss = new StvELossTrak();
}
//_____________________________________________________________________________
 StDcaToVtx::~StDcaToVtx()
{
  delete mHlx;
  delete mELoss;
}
//_____________________________________________________________________________
void StDcaToVtx::Set(double M
                    ,const TGeoMaterial *matA,double RxyA
	            ,const TGeoMaterial *matB,double RxyB
		    ,const TGeoMaterial *matC,double RxyC)
{
  mM = M;
  mRxy[0] = RxyA;
  mMat[0] = matA;
  mRxy[1] = RxyB;
  mMat[1] = matB;
  mRxy[2] = RxyC;
  mMat[2] = matC;
}
//_____________________________________________________________________________
void StDcaToVtx::Set(Float_t Vtx[3],Float_t *DcaPar)
{
  for (int i=0;i<3;i++) {mVtx[i]=Vtx[i];};
  memcpy(&mDcaPar,DcaPar,sizeof(mDcaPar));
  mP = fabs(1./mDcaPar.mPti);
  mHz = mDcaPar.mCurv/mDcaPar.mPti;

  mHlxPar.mPos[0] = -mDcaPar.mImp*sin(mDcaPar.mPsi);
  mHlxPar.mPos[1] =  mDcaPar.mImp*cos(mDcaPar.mPsi);
  mHlxPar.mPos[2] =  mDcaPar.mZ;
  double cosL = cos(atan(mDcaPar.mTan));
  double sinL = sin(atan(mDcaPar.mTan));
  mHlxPar.mDir[0] =  cosL*cos(mDcaPar.mPsi);
  mHlxPar.mDir[1] =  cosL*sin(mDcaPar.mPsi);
  mHlxPar.mDir[2] =  sinL;
  mHlxPar.mCur    =  mDcaPar.mCurv;

   int iNonDia=0;
   for (int i=0,li=0;i< 5;li+=++i) {
     mDcaEmx[li+i] = mDcaPar.mSigma[i];
     for (int j=0;j<i;j++) {
       mDcaEmx[li+j] = mDcaPar.mCorr[iNonDia++];
   } }
static const int idx[] = {
  0,
  6, 9,
  1, 7,  2,
  3, 8,  4, 5,
  10,13,11,12,14
};

  for (int i=0;i<15;i++) {mHlxEmx[idx[i]] = mDcaEmx[i];}
  double dif[5]={1,1,mHz,1,1+mDcaPar.mTan*mDcaPar.mTan};;
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<i;j++) {
      mHlxEmx[li+j]*= dif[i]*dif[j];
  } }


  mHlx->Set(mHlxPar.mPos, mHlxPar.mDir,mHlxPar.mCur);
  mHlx->SetEmx(mHlxEmx);

}
//_____________________________________________________________________________
void StDcaToVtx::Shooter()
{

  for (int idx=0; idx<3;idx++) {
    double len = 0;
    if (idx<2) { //Step to cylinder
      double cyl[] = { -mRxy[idx]*mRxy[idx],0,0,0,1,1};
      len = mHlx->Step(222.,cyl, 6);
    } else {
      len = mHlx->Path(mVtx);
    }
    mHlx->Move(len);
    UpdateELoss(len,mMat[idx]);

  }
}
#if 0
double
0 mHH,
1 mHA, mAA,
3 mHC, mAC, mCC,
6 mHZ, mAZ, mCZ, mZZ,
10 mHL, mAL, mCL, mZL, mLL;
#endif

//_____________________________________________________________________________
void StDcaToVtx::UpdateELoss(double len, const TGeoMaterial *mat)
{
// C == Curvature
// k == dC/dl
// l == track length 
// 
// dX = cos(C*l + k*l*l/2)*dl
// dY = sin(C*l + k*l*l/2)*dl
// 
// k is small (E loss is small)
// 
// dX = (cos(C*l) - sin(C*l)*k*l*l/2)*dl
// dY = (sin(C*l) + cos(C*l)*k*l*l/2)*dl
// 
// Normal vector to track in xy plane(-sin(C*l),cos(C*l))
// 
// dPerp = ( -cos(C*l)*sin(C*l) + cos(C*l)*sin(C*l) + sin(C*l)**2*k*l*l/2+cos(C*l)**2) *k*l*l/2
// dPerp = k*l*l/2
//  Perp = k*L**3/6
//  
//  dP/P = -dC/C
//  dC = -dP*C/P
//  dC/dl  = -dP*C/P/l
//  dP == -Ploss
//  
//  dC/dl  = (Ploss/P)*C/L
//  
//  Perp = (Ploss/P)*C*L**2/6
//  Dang = (Ploss/P)*C*L/2
// ==================================================
// 
// T = ( cosL*cosP,cosL*sinP ,sinL)
// U = (-     cosP,     sinP ,   0)
// V = (-sinL*cosP,-sinL*cosP,cosL)
// 
// U*du + V*dv = (cosL*U)*dPhi  + V*dL
// 
// Hence:
// dPhi = du/cosL
// dLam = dv
// 
 int sgn = (len<0)? -1:1;
 double *pos = mHlx->Pos();       
 double *dir = mHlx->Dir(); 
 mCurv = mHlx->GetRho();
 THEmx_t *emx = mHlx->Emx();
 mELoss->Reset(1);
 mELoss->Set(mat,mP,mCurv);
 mELoss->Add(len);
//		Change curvature
 double dCur = (mELoss->PLoss()/mP)*mCurv;
 assert(sgn*mELoss->PLoss()>0);

 mP -=mELoss->PLoss();
 mCurv+=dCur;
//		Chane direction
 dir[0] += -dir[1]*mELoss->Dang();
 dir[1] +=  dir[0]*mELoss->Dang();
//
 double cosL = mHlx->GetCos();
 double cosP = dir[0]/cosL;
 double sinP = dir[1]/cosL;
 pos[0] += -sinP*mELoss->Perp();
 pos[1] +=  cosP*mELoss->Perp();

// 		Update helix pars
 mHlx->Set(pos,dir,mCurv);

//		Update 
 double theta2 = mELoss->GetTheta2();
 emx->mAA -= sgn*theta2/(cosL*cosL);
 emx->mLL -= sgn*theta2;

 printf("%s:\t Len = %g Ploss = %g dCur = %g Perp = %g\n"
       ,mat->GetName(),len, mELoss->PLoss(),dCur,mELoss->Perp());
 printf("%s:\t P=%g Curv=%g Pos= %g %g %g\n\n"
       ,mat->GetName(),mP,mCurv,pos[0],pos[1],pos[2]);

}
//_____________________________________________________________________________
//_____________________________________________________________________________
THelixTrack gTestHlxV;


//_____________________________________________________________________________
//_____________________________________________________________________________
void StDcaToVtx::SetTest()
{
static const double PiMASS=0.13956995;
class MyMat_t {public: const char* Material; int No; double A,Z,Density,RadLen;}; 
static MyMat_t myMat[] = {
{"Hydrogen",	1,	1.010,		 1.000,	0.071,		865.000},
{"Beryllium",	5,	9.010,		 4.000,	1.848,		35.300 },
{"Vacuum",	16,	1e-16,		 1e-16,	1e-16,		1e16   }};

 TGeoMaterial *matHyd = new TGeoMaterial (myMat[0].Material
                                         ,myMat[0].A
		  		         ,myMat[0].Z
				         ,myMat[0].Density
				         ,myMat[0].RadLen);

 TGeoMaterial *matBer = new TGeoMaterial (myMat[1].Material
                                         ,myMat[1].A
		  		         ,myMat[1].Z
				         ,myMat[1].Density
				         ,myMat[1].RadLen);
 TGeoMaterial *matVac = new TGeoMaterial (myMat[2].Material
                                         ,myMat[2].A
		  		         ,myMat[2].Z
				         ,myMat[2].Density
				         ,myMat[2].RadLen);

  Set(PiMASS,matVac,4.,matBer,4.2,matHyd,10.);
  double pos[3]={1.,0.,1},dir[3]={0.,100.,20};
  mP = 0.5;
  mCurv = 1./669./mP;

  mHlx->Set(pos,dir,mCurv);
  mHlx->Move(100.);
  memcpy(pos,mHlx->Pos(),sizeof(pos));
  memcpy(dir,mHlx->Dir(),sizeof(dir));
  mMat[3] = mMat[2];
  mRxy[3] = 100;

  mHlx->SetEmx(0);
  THEmx_t *emx = mHlx->Emx();
  emx->mHH = pow(0.1     ,2);
  emx->mAA = pow(3.14/180,2);
  emx->mCC = pow(mCurv/10,2);
  emx->mZZ = pow(0.1     ,2);
  emx->mLL = pow(3.14/180,2);
  mHlx->SetEmx(*emx);
}
//_____________________________________________________________________________
void StDcaToVtx::InitTest()
{
  SetTest();
  
  double len = 0;
  for (int idx=3; idx>=0;idx--) {
    if (idx == 0) {
      len = mHlx->Path(0.,0.);
    } else {
      mHlx->Backward();
      double cyl[] = { -mRxy[idx-1]*mRxy[idx-1],0,0,0,1,1};
      len = mHlx->Step(222.,cyl, 6);
      mHlx->Backward(); len = -len;
    }
    mHlx->Move(len);
    UpdateELoss(len,mMat[idx]);
    if (idx!=3) continue;
    gTestHlxV = *mHlx;
    memcpy(mVtx,mHlx->Pos(),sizeof(mVtx));
  }
}
//_____________________________________________________________________________
void StDcaToVtx::Test()
{
  StDcaToVtx dv;
  dv.SetTest();   
  dv.InitTest();
  dv.Shooter();
//  gTestHlxV.Print("InitHlx");
  dv.GetHlx()->Print("EndHlx");

}
