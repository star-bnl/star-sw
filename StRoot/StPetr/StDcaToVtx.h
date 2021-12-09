#ifndef STDCATOVTX_H
#define STDCATOVTX_H
#include "Rtypes.h"
#include "THelixTrack.h"

class TGeoMaterial;
class THelixTrack;
class StvELossTrak;
//..............................................................................
class DcaPar_t
{ 
public:
  /// Signed impact parameter; Signed in such a way that:
  /// x =  -impact*sin(Psi), y =   impact*cos(Psi)
  void operator=(const Float_t *arr) { memcpy(&mImp,arr,sizeof(DcaPar_t));}
  Float16_t mImp;
  /// Z position of the track fitted to (0,0,z)
  Float16_t mZ;
  /// Psi angle of the track
  Float16_t mPsi;        //[-pi,pi,20]
  /// Pti of the track ( 1/pT )
  Float16_t mPti;
  /// Tangent of the track momentum dip angle
  Float16_t mTan;        //[-10,10,20]
  /// Curvature
  Float16_t mCurv;
  /// Diagonal elements
  Float16_t mSigma[5];
  /// Off-diagonal elements
  Float16_t mCorr[10];   //[-1,1,20] 
};


class DcaEmx_t
{
public:
  void operator=(const Float_t *arr) { memcpy(&mImpImp,arr,sizeof(DcaEmx_t));}
  Float_t& operator[](int i) { return (&mImpImp)[i];};
                                                          //    j    0     1     2     3     4
    Float_t  mImpImp;                                     //  i 0  0(0) 
    Float_t  mZImp,   mZZ;                                //    1  1(0)  2(1)
    Float_t  mPsiImp, mPsiZ, mPsiPsi;                     //    2  3(1)  4(2)  5(2)
    Float_t  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;            //    3  6(3)  7(4)  8(5)  9(3)
    Float_t  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;   //    4 10(6) 11(7) 12(8) 13(9) 14(4)
};

class THPar_t
{
public:
double mPos[3];
double mDir[3];
double mCur;
};

class StDcaToVtx
{
public:
  StDcaToVtx();
  virtual ~StDcaToVtx();
  void Set(double M,const TGeoMaterial *matA,double RxyA,
                    const TGeoMaterial *matB,double RxyB,
		    const TGeoMaterial *matC,double RxyC=0);
  void Set(Float_t Vtx[3],Float_t *dcaPar);
  void UpdateELoss(double len,const TGeoMaterial *mat);
  void Shooter();

THEmx_t      *GetEmx() {return mHlx->Emx();};
THelixTrack  *GetHlx() {return mHlx       ;};
StvELossTrak *ELoss()  {return mELoss     ;};
double       *GetRxy() {return mRxy       ;};  

  void SetTest();
  void InitTest();
  static void Test();
protected:
double mM;
double mRxy[4];
const TGeoMaterial *mMat[4];
DcaPar_t mDcaPar;
DcaEmx_t mDcaEmx;
double mP;
double mCurv;
double mHz;
double mVtx[3];
THPar_t mHlxPar;
THEmx_t mHlxEmx;
THelixTrack *mHlx;
StvELossTrak *mELoss;
ClassDef(StDcaToVtx,0)
};

#endif
