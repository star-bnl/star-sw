#ifndef STDCATOVTX_H
#define STDCATOVTX_H
#include "TObject.h"
#include "THelixTrack.h"
#include "StDcaGeometry.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"

class TGeoMaterial;
class THelixTrack;
class StvELossTrak;
class THPar_t
{
 public:
  double mPos[3];
  double mDir[3];
  double mCur;
};

class StDcaToVtx : public TObject
{
 public:
  StDcaToVtx();
  virtual ~StDcaToVtx();
  void Set(Int_t pdg) {mPdg = pdg; mM = 0; TParticlePDG *p = TDatabasePDG::Instance()->GetParticle(mPdg); if (p) mM = p->Mass();}
  void Set(Double_t M) {mM = M;}
  void Add(Double_t R, TGeoMaterial *mat) {mRxy[mN] = R; mMat[mN] = mat; mN++;}
  void Set(double M,const TGeoMaterial *matA,double RxyA,
	   const TGeoMaterial *matB,double RxyB,
	   const TGeoMaterial *matC,double RxyC=0);
  void Set(Float_t Vtx[3],StDcaGeometry *dcaG);
  void UpdateELoss(double len,const TGeoMaterial *mat);
  void UpdateDca();
  void Shooter();
  virtual void        Print(Option_t *option="") const;
  THEmx_t      *GetEmx() {return mHlx->Emx();};
  THelixTrack  *GetHlx() {return mHlx       ;};
  StvELossTrak *ELoss()  {return mELoss     ;};
  double       *GetRxy() {return mRxy       ;};  
  const StDcaGeometry &Dca() {return *&mDca;}
  const KFParticle    &Particle() {return *&mParticle;}
  void SetTest();
  void InitTest();
  static void Test();
  static void TestDCA();
 protected:
  double mM;
  Int_t mN;
  double mRxy[20];
  const TGeoMaterial *mMat[20];
  double mP;
  double mCurv;
  double mHz;
  double mVtx[3];
  THPar_t mHlxPar;
  THEmx_t mHlxEmx;
  THelixTrack *mHlx;
  StvELossTrak *mELoss;
  Int_t         mPdg;
  StDcaGeometry mDca;
  KFParticle    mParticle;
  ClassDef(StDcaToVtx,0)
};
#endif
