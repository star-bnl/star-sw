#ifndef StPicoTrackCovMatrix_h
#define StPicoTrackCovMatrix_h

// ROOT headers
#include "TObject.h"

//_________________
class StPicoTrackCovMatrix : public TObject {
  
 public:
  
  /// Default constructor
  StPicoTrackCovMatrix();
  /// Copy constructor
  StPicoTrackCovMatrix(const StPicoTrackCovMatrix &matrix);
  /// Destructor
  virtual ~StPicoTrackCovMatrix();
  /// Print option
  virtual void Print(Char_t const* option = "") const;

  /**
   * Getters
   */

  /// Return address to the first parameter
  Float_t* params();
  /// Return address to the first parameter
  const Float_t* params() const;
  /// Return pointer to the sigma array
  const Float_t* sigmas() const;
  /// Return pointer to the correlation array
  const Float_t* correlations() const;

  /// Return impact
  Float_t imp() const;
  /// Return z
  Float_t z() const;
  /// Return psi
  Float_t psi() const;
  /// Return pti
  Float_t pti() const;
  /// Return tangent
  Float_t tan() const;
  /// Return curvature
  Float_t curv() const;

  /// Return true, if all values 0. It corresponds to
  /// the case when track did not have a covariance
  /// matrix in MuDst
  Bool_t isBadCovMatrix();

  /**
   * Setters
   */

  /// Set 6 values (main values)
  void setParams(Float_t params[6]);
  /// Set 5 sigma parameters
  void setSigmas(Float_t sigmas[5]);
  /// Set 10 correlation parameters
  void setCorrelations(Float_t corr[10]);

  /// Set impact
  void setImp(Float_t imp);
  /// Set z
  void setZ(Float_t z);
  /// Set psi
  void setPsi(Float_t psi);
  /// Set pti
  void setPti(Float_t pti);
  /// Set tangent
  void setTan(Float_t tan);
  /// Set curvature
  void setCurv(Float_t curv);
  
 private:

    /*                                                          j    0     1     2     3     4
    Float_t  mImpImp;                                       i 0  0(0) 
    Float_t  mZImp,   mZZ;                                    1  1(0)  2(1)
    Float_t  mPsiImp, mPsiZ, mPsiPsi;                         2  3(1)  4(2)  5(2)
    Float_t  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;                3  6(3)  7(4)  8(5)  9(3)
    Float_t  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;       4 10(6) 11(7) 12(8) 13(9) 14(4)
   */

  /// Signed impact parameter; Signed in such a way that:
  /// x =  -impact*sin(Psi), y =   impact*cos(Psi)
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

  ClassDef(StPicoTrackCovMatrix, 1)
};

/**
 * Getters
 */
inline Float_t* StPicoTrackCovMatrix::params() { return &mImp; }
inline const Float_t* StPicoTrackCovMatrix::params() const { return &mImp; }
inline const Float_t* StPicoTrackCovMatrix::sigmas() const { return mSigma; }
inline const Float_t* StPicoTrackCovMatrix::correlations() const { return mCorr; }
inline Float_t StPicoTrackCovMatrix::imp() const { return mImp; }
inline Float_t StPicoTrackCovMatrix::z() const { return mZ; }
inline Float_t StPicoTrackCovMatrix::psi() const { return mPsi; }
inline Float_t StPicoTrackCovMatrix::pti() const { return mPti; }
inline Float_t StPicoTrackCovMatrix::tan() const { return mTan; }
inline Float_t StPicoTrackCovMatrix::curv() const { return mCurv; }

/**
 * Setters
 */
inline void StPicoTrackCovMatrix::setImp(Float_t imp) { mImp = (Float16_t)imp; }
inline void StPicoTrackCovMatrix::setZ(Float_t z) { mZ = (Float16_t)z; }
inline void StPicoTrackCovMatrix::setPsi(Float_t psi) { mPsi = (Float16_t)psi; }
inline void StPicoTrackCovMatrix::setPti(Float_t pti) { mPti = (Float16_t)pti; }
inline void StPicoTrackCovMatrix::setTan(Float_t tan) { mTan = (Float16_t)tan; }
inline void StPicoTrackCovMatrix::setCurv(Float_t curv) { mCurv = (Float16_t)curv; }
inline void StPicoTrackCovMatrix::setParams(Float_t params[6]) {
  mImp = params[0]; mZ = params[1]; mPsi = params[2]; mPti = params[3];
  mTan = params[4]; mCurv = params[5];
}
inline void StPicoTrackCovMatrix::setSigmas(Float_t sigmas[5]) {
  for(Int_t iIter=0; iIter<5; iIter++) { mSigma[iIter] = sigmas[iIter]; }
}
inline void StPicoTrackCovMatrix::setCorrelations(Float_t corr[10]) {
  for(Int_t iIter=0; iIter<10; iIter++) { mCorr[iIter] = corr[iIter]; }
}

#endif
