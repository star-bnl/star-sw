/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : ThPair wich return :
 *               RealMomentum : HiddenInfo Momentum or Particle Momentum
 *                              according to 
 *                              UseHiddenMomentum()/ UseParticleMomentum()
 *               EmPoint : EmPoint is distributed randomly in  a Gaussian
 *                         distribution.Size is fixed by SetSize() and 
 *                         Ref Frame is fixed by SetRCMS()...
 *               Pid : to be fixed
 *               MeasMomentum : Particle Momentum
 *
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef ST_HBT_THPAIR_GAUSS_HH
#define ST_HBT_THPAIR_GAUSS_HH

#include "TRandom.h"

#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"

enum RefFrame{RCMS,LCMS,PRF};

class StHbtThPairGauss : public StHbtThPair {

public:
  StHbtThPairGauss() ;
  virtual ~StHbtThPairGauss();
  virtual void Set(const StHbtPair* aPair);

  void SetSize(double aXYZ,double aT );
  void SetSize(double aX,double aY,double aZ, double aT);
  void UseHiddenMomentum();
  void UseParticleMomentum();

  void UseHiddenPid();
  void UseFixedPid( int const tPid1, double const tMass1);  
  void UseFixedPid( int const tPid1,double const tMass1, int const tPid2,double const tMass2 ); 
 
  void SetBoostRCMS(double aPlab,double aMBeam, double aMTarget);
  
  void SetRCMS();
  void SetLCMS();
  void SetPRF();


 protected:
  TRandom       mRand;
  bool          mUseHidMom;
  bool          mUseHidPid;
  double        mSizeX,mSizeY,mSizeZ;
  double        mTime;
  RefFrame           mRef;
  double  mBetaRCMS;
  double mGammaRCMS;

  StHbtLorentzVector mPos1;
  StHbtLorentzVector mPos2;

  StHbtLorentzVector mMom1;  //because StHbtParticle.FourMomentum() is not a ref
  StHbtLorentzVector mMom2;

  double mMassSq1,mMassSq2;

  // 3 protected Step called from  Set(...) public function member
  void SetMomentum_PID( const StHbtPair* );
  void SetPosition();
  void BoostPosition();

#ifdef __ROOT__
  ClassDef(StHbtThPairGauss,1)
#endif
};

#endif
