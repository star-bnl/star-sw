 /*
 * \class StDcaGenFit
 * \author Victor Perevoztchikov, 2006
 */
/***************************************************************************
 *
 * $Id: genFitDca.h,v 1.1 2020/05/23 23:25:06 perev Exp $
 *
 * Author: Victor Perevoztchikov
 ***************************************************************************
 *
 * Description:
 * 
 * class StDcaGenFit it is almost copy of StEvent/StDcaGeometry
 * class GFull is a converter from GenFit format into DCA representation in StDcaGenFit
 * 
 *   GFull gf;
 * //	Pos[3]    - Position of track
 * //	UVN[3][3] - coordinate system in which Fit is made
 *   gf.SetGlob(Pos,UVN);
 *   gf.SetPars(icharge,PosV,PbegV);  gf.SetBigPars(icharge,PosV,PbegV);
 *   gf.SetErrs(gfiErrs);
 * //	pars[5]  - qPinv = charge/P
 * //               - uc - cos(Track direction * UVN[0]
 * //		 - vc - cos(Track direction * UVN[1]
 * //		 - u  - posiotion along UVN[0]
 * //		 - v  - posiotion along UVN[1]
 *   gf.SetPars(Pars,sign
 * 
 * // 		emx[15] - triangle error matrix
 *   gf.SetErrs(emx);
 * 
 *   StDcaGenFit dca,dca1;
 *   gf.FillDcaPars(dca);
 *   gf.FillDcaErrs(dca);
 * // now dca is filled
 *
 ***************************************************************************
 *
 * $Log: genFitDca.h,v $
 * Revision 1.1  2020/05/23 23:25:06  perev
 * GenFit errors conversion into Dca
 *
 * Revision 2.6  2012/03/28 13:39:46  fisyak
 * Add default parameter for Print
 *
 * Revision 2.5  2010/01/26 20:34:39  fisyak
 * Add print out and  conversion from DCA to x,y,z,px,py,pz
 *
 * Revision 2.4  2009/10/28 13:54:35  fisyak
 * Forgot one more set
 *
 * Revision 2.3  2008/03/04 01:03:14  perev
 * remove redundant mHz
 *
 **************************************************************************/
#ifndef StDcaGenFit_hh
#define StDcaGenFit_hh
#include "TVector3.h"
#include "TVectorD.h"
//		All enums
enum {kNMaxPars = 6, kNMaxErrs= (kNMaxPars*(kNMaxPars+1))/2};
enum {kNMinPars = 5, kNMinErrs= (kNMinPars*(kNMinPars+1))/2};
enum {kNBigPars = 7, kNBigErrs= (kNBigPars*(kNBigPars+1))/2};
enum {
  kqPinv  = 0,	// charge/momentum
  kUc,		// cos to Uvector
  kVc,		// cos to Vvector
  kNc,		// cos to Vvector
  kU_,		// along Uvector
  kV_		// along Vvector
};
enum {
  kqPtInv = 0,	// charge/Pt
  kDirX,
  kDirY,
  kDirZ,
  kPosX,
  kPosY,
  kPosZ
};
enum {
  kImp,
  kZ,		/// Z-coordinate of this track (reference plane)
  kPsi,    	/// Psi angle of the track
  kPti,		/// signed invert pt [sign = sign(-qB)]
  kTan		/// tangent of the track momentum dip angle
};

class StDcaGenFit;

class GFGlob {
public:
GFGlob();
double mH[3];		//Mag field in global sys
double mPos[3];		//Origin in global sys
double mUVN[3][3];	//Directions of local coord sys
};


class GFitPars
{
public:
GFitPars();
operator       double* ()       



{return &mqPinv;}
operator const double* () const {return &mqPinv;}
void SetPars(double qpinv, double uc, double vc, double u, double v,int sig=1); 
public:
double mqPinv;		// charge/momentum
double mUc;		// cos to Uvector
double mVc;		// cos to Vvector
double mNc;		// cos to Vvector
double mU;		// along Uvector
double mV;		// along Vvector
double mN;		// along Nvector
int    mSig;		//+1,-1 track direction
};

class GFitErrs
{
public:
GFitErrs();
public:
operator const double* () const {return &qPqP;}
operator       double* ()       {return &qPqP;}





public:
double 
qPqP,
qPUc,UcUc,
qPVc,UcVc,VcVc,
qPNc,UcNc,VcNc,NcNc,
qPU, UcU, VcU, NcU, UU,
qPV, UcV, VcV, NcV, UV, VV;
};

class GFull 
{
public:
GFull(){mBigPars.ResizeTo(kNBigPars);}
void SetMag (const double h[3]); 
void SetGlob (const double pos[3],const double uvn[3][3]);
void SetPars(double qpinv, double uc, double vc, double u, double v,int sig=1); 
void SetPars(const double pars[5] ,int sig);
void SetPars(int icharge,const TVector3 pos,const TVector3 mom);
TVectorD GetPars(int* iSig=0) const;


void SetBigPars(int icharge,const TVector3 pos,const TVector3 mom);
const TVectorD &GetBigPars() const {return mBigPars;};
const double   *GetBigErrs() const {return mBigErrs;};
void MakeTrak();
void SetErrs(const double emx[15]);
TVector3 Pos()  const;
TVector3 Dir()  const;
TVector3 Mom()  const { return Dir()*(1./fabs(Pinv()));}
double   SinL() const;
double   CosL() const;
double   TanL() const;
double   Lam()  const;
double   Psi()  const;
double   Pti()  const;		//Perpendicular Z axis invered&signed
double   Pinv() const;		//Perpendicular Z axis invered&signed
double   Imp()  const;
const double  *BigErrs() const { return mBigErrs;}
const double  *XtdPars() const { return mPars;   }
const double  *XtdErrs() const { return mErrs;   }
void FillDcaPars(StDcaGenFit &dca);
void FillDcaErrs(StDcaGenFit &dca);
static void TestConvertErrs();

TVectorD BigVal() const ;
double   BigDer(int ib,int iu);



public:
GFGlob   mGlob;
GFitPars mPars;
GFitErrs mErrs;

TVector3 mPos;
TVector3 mDir;
double mqPinv;
TVectorD mBigPars;
double mBigErrs[kNBigErrs];
};



class StDcaGenFit {
public:
    StDcaGenFit();
    virtual ~StDcaGenFit();

    Int_t          charge()    const {return (mPti>0)? -1:1;}  // synchro with StiTrackNode charge definition
    double         impact()    const {return mImp;}		  
    double         curvature() const {return mCurv;}		  
    double         psi()       const {return mPsi ;}		  
    double         dipAngle()  const {return atan(mTan);}	  
    double         tanDip()    const {return mTan ;}		  
    double         pt()        const {return 1./fabs(mPti);}	  
    double         z()         const {return mZ   ;}		  
    double         hz()        const {return mCurv/mPti;}	  
    TVector3   origin()    const;         
    TVector3   momentum()  const;        
    const float*     params()    const {return &mImp;}	  
    const float*     errMatrix() const {return &mImpImp;} 
    virtual void     Print(const char *option = "") const;
    //
    // Experts only set function
    //
    void set(const float  pars[6], const float  errs[15]);
    void set(const double pars[6], const double errs[15]);

public:
    char mBeg[1];//!
    /// signed impact parameter; Signed in such a way that:
    ///     x =  -impact*sin(Psi)
    ///     y =   impact*cos(Psi)
    float  mImp;
    ///  Z-coordinate of this track (reference plane)
    float  mZ;
    ///  Psi angle of the track
    float  mPsi;
    /// signed invert pt [sign = sign(-qB)]
    float  mPti;
    /// tangent of the track momentum dip angle
    float  mTan;
    /// signed curvature
    float  mCurv;
    
    /// pars errors
    float  mImpImp;
    float  mZImp,   mZZ;
    float  mPsiImp, mPsiZ, mPsiPsi;
    float  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;
    float  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;

    char   mEnd[1];//!
    
};

#endif

