/**********************************************************************
 *
 * $Id: StEStructTrack.h,v 1.11 2012/11/16 21:24:38 prindle Exp $
 *
 * Author: Jeff Porter merge of work from Aya Ishihara and Jeff Reid
 *
 **********************************************************************
 *
 * Description:  Estruct Track
 *
 ***********************************************************************/

#ifndef _StEStructTrack
#define _StEStructTrack

#include <math.h>
#include "TObject.h"
#include "StHelixD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVectorF.hh"
#include "StHelix.hh"
#include "StPhysicalHelixD.hh"
#include "StPhysicalHelix.hh"

class StEStructTrack : public TObject {

private:

  Float_t       mPx;
  Float_t       mPy;
  Float_t       mPz;

  Float_t       mEta;
  Float_t       mPhi;

  Float_t       mBxPrimary;
  Float_t       mByPrimary;
  Float_t       mBzPrimary;

  Float_t       mBxGlobal;
  Float_t       mByGlobal;
  Float_t       mBzGlobal;

  Int_t         mPID;
  Int_t         mPID_dEdx;
  Int_t         mPID_ToF;
  Float_t       mDedx;
  Float_t       mPIDe_dEdx;
  Float_t       mPIDpi_dEdx;
  Float_t       mPIDp_dEdx;
  Float_t       mPIDk_dEdx;
  Float_t       mPIDd_dEdx;
  Float_t       mPIDe_ToF;
  Float_t       mPIDpi_ToF;
  Float_t       mPIDp_ToF;
  Float_t       mPIDk_ToF;
  Float_t       mPIDd_ToF;
  Float_t       mBeta;
  Float_t       mMass;

  Float_t       mChi2;

  Int_t         mNFitPoints;
  Int_t         mNFoundPoints;
  Int_t         mNMaxPoints;

  Int_t         mDetectorID;
  Int_t         mFlag;

  Short_t       mCharge;
  ULong_t       mMap[2]; 
  UInt_t        mTPCNHits;

  //-> From old StEbye2ptTrack.... note none are persistent

  Bool_t            mIsComplete; //!
  StPhysicalHelixD  mHelix;  //! Recalculate helix so we can make persistent StEStructEvent smaller.
  Float_t           mPt; //!
  Float_t           mPtot; //!
  Float_t           mYt; //!
  Float_t           mXt; //!
  Float_t           mCurvature; //!
  Float_t           mAssignedMass; //!
  Float_t           mMidTPCRadius;    //|
  Float_t           mOuterMidTPCRadius;    //|
  Float_t           mMaxRadius;    //|
  Float_t           mEndCapRadius; //|
  Int_t             mEndCapOuterMid; //!
  Int_t             mEndCapOuter; //!
  StLorentzVectorF  mFourMomentum; //!
  StThreeVectorF    mStartPos; //!
  StThreeVectorF    mNominalTpcExitPoint; //!
  StThreeVectorF    mNominalTpcEntrancePoint; //!
  StThreeVectorF    mMidTpcPoint; //!
  StThreeVectorF    mOuterMidTpcPoint; //!
  int               mytbin; //!


public:

  StEStructTrack() : mIsComplete(false), mAssignedMass(0.1396) {};
  StEStructTrack(StEStructTrack* track);
  virtual ~StEStructTrack() {};

  static StThreeVectorD PrimVertex;
  static Float_t        BField;  //!
  void FillTransientData();
  void evalPt();
  void evalPtot();
  void evalYt();
  void evalXt();
  void evalCurvature();
  void evalFourMomentum(float mass=0);
  void evalPID();
  void evalMass();
  void evalTrajectory(float primvx, float primvy, float primvz, double bfield);
  void FillTpcReferencePoints();

  // functions which simply return data members
  Float_t Px() const { return mPx; }
  Float_t Py() const { return mPy; }
  Float_t Pz() const { return mPz; }

  Float_t Eta() const { return mEta; }
  Float_t Phi() const { return mPhi; }

  Float_t Bx() const { return mBxPrimary; }
  Float_t By() const { return mByPrimary; }
  Float_t Bz() const { return mBzPrimary; }

  Float_t BxPrimary() const { return mBxPrimary; }
  Float_t ByPrimary() const { return mByPrimary; }
  Float_t BzPrimary() const { return mBzPrimary; }

  Float_t BxGlobal() const { return mBxGlobal; }
  Float_t ByGlobal() const { return mByGlobal; }
  Float_t BzGlobal() const { return mBzGlobal; }

  Int_t   PID()      const { return mPID; }
  Int_t   PID_dEdx() const { return mPID_dEdx; }
  Int_t   PID_ToF()  const { return mPID_ToF; }

  Float_t PIDe_dEdx() const { return mPIDe_dEdx; }
  Float_t PIDpi_dEdx() const { return mPIDpi_dEdx; }
  Float_t PIDp_dEdx() const { return mPIDp_dEdx; }
  Float_t PIDk_dEdx() const { return mPIDk_dEdx; }
  Float_t PIDd_dEdx() const { return mPIDd_dEdx; }

  Float_t PIDe_ToF() const { return mPIDe_ToF; }
  Float_t PIDpi_ToF() const { return mPIDpi_ToF; }
  Float_t PIDp_ToF() const { return mPIDp_ToF; }
  Float_t PIDk_ToF() const { return mPIDk_ToF; }
  Float_t PIDd_ToF() const { return mPIDd_ToF; }

  Float_t beta() const { return mBeta; }
  Float_t Dedx() const { return mDedx; }
  Float_t Mass() const { return mMass; }
  Float_t Chi2() const { return mChi2; }
  Float_t AssignedMass() const { return mAssignedMass; };
  Float_t MidTPCRadius() const { return mMidTPCRadius; };
  Float_t OuterMidTPCRadius() const { return mOuterMidTPCRadius; };
  Float_t MaxRadius() const { return mMaxRadius; };
  Float_t EndCapRadius() const { return mEndCapRadius; };
  Int_t EndCapOuter() const { return mEndCapOuter; };
  Int_t EndCapOuterMid() const { return mEndCapOuterMid; };

  Int_t NFitPoints() const { return mNFitPoints; }
  Int_t NFoundPoints() const { return mNFoundPoints; }
  Int_t NMaxPoints() const { return mNMaxPoints; }

  Int_t DetectorID() const { return mDetectorID; }
  Int_t Flag() const { return mFlag; }

  Short_t Charge() const { return mCharge; }

  ULong_t TopologyMapData(const Int_t word) const { return mMap[word];} 
  UInt_t  TopologyMapTPCNHits() const {return mTPCNHits;}
  int      getYtBin() const;

  // functions which do some simple calculations
  //  using information contained in data members
  Float_t	Pt() const;
  Float_t	Ptot() const;
  Float_t	Mt(Float_t mass) const;
  Float_t	E(Float_t mass) const;
  Float_t	Eta(Float_t mass) const;
  Float_t	Rapidity(Float_t mass) const;


  Float_t       Dca() const;
  Float_t       DcaPrimary() const;
  Float_t       DcaGlobal() const;

  // accessors to transient data ...
  Bool_t        isComplete() const { return mIsComplete; };
  const StThreeVectorF&   NominalTpcExitPoint()        const;
  const StThreeVectorF&   NominalTpcEntrancePoint()    const;
  const StThreeVectorF&   MidTpcPoint()                const;
  const StThreeVectorF&   OuterMidTpcPoint()           const;
  const StThreeVectorF&   StartPos()                   const;
  const StLorentzVectorF& FourMomentum()               const;
  const StPhysicalHelixD& Helix()                      const;

  Float_t  Xt() const;
  Float_t  Yt() const;
  Float_t  Yt(Float_t mass) const;
  Float_t  Curvature() const;

  // functions used to set data members
  void SetPx(Float_t px) { mPx = px; }
  void SetPy(Float_t py) { mPy = py; }
  void SetPz(Float_t pz) { mPz = pz; }

  void SetEta(Float_t eta) { mEta = eta; }
  void SetPhi(Float_t phi) { mPhi = phi; }

  void SetBx(Float_t bx) { mBxPrimary = bx; }
  void SetBy(Float_t by) { mByPrimary = by; }
  void SetBz(Float_t bz) { mBzPrimary = bz; }

  void SetBxPrimary(Float_t bxp) { mBxPrimary = bxp; }
  void SetByPrimary(Float_t byp) { mByPrimary = byp; }
  void SetBzPrimary(Float_t bzp) { mBzPrimary = bzp; }

  void SetBxGlobal(Float_t bxg) { mBxGlobal = bxg; }
  void SetByGlobal(Float_t byg) { mByGlobal = byg; }
  void SetBzGlobal(Float_t bzg) { mBzGlobal = bzg; }

  void SetPIDe_dEdx(Float_t pide) { mPIDe_dEdx = pide; }
  void SetPIDpi_dEdx(Float_t pidpi) { mPIDpi_dEdx = pidpi; }
  void SetPIDp_dEdx(Float_t pidp) { mPIDp_dEdx = pidp; }
  void SetPIDk_dEdx(Float_t pidk) { mPIDk_dEdx = pidk; }
  void SetPIDd_dEdx(Float_t pidd) { mPIDd_dEdx = pidd; }

  void SetPIDe_ToF(Float_t pide) { mPIDe_ToF = pide; }
  void SetPIDpi_ToF(Float_t pidpi) { mPIDpi_ToF = pidpi; }
  void SetPIDp_ToF(Float_t pidp) { mPIDp_ToF = pidp; }
  void SetPIDk_ToF(Float_t pidk) { mPIDk_ToF = pidk; }
  void SetPIDd_ToF(Float_t pidd) { mPIDd_ToF = pidd; }

  void SetBeta(Float_t beta) { mBeta = beta; }
  void SetDedx(Float_t dedx) { mDedx = dedx; }
  void SetMass(Float_t mass) { mMass = mass; }
  void SetChi2(Float_t chi2) { mChi2 = chi2; }
  void SetMassAssignment(float mass){ mAssignedMass=mass; };
  void SetNFitPoints(Int_t nfit) { mNFitPoints = nfit; }
  void SetNFoundPoints(Int_t nfound) { mNFoundPoints = nfound; }
  void SetNMaxPoints(Int_t nmax) { mNMaxPoints = nmax; }

  void SetDetectorID(Int_t did) { mDetectorID = did; }
  void SetFlag(Int_t flag) { mFlag = flag; }

  void SetCharge(Short_t charge) { mCharge = charge; }
  void SetTopologyMapData(const int word, const ULong_t map){ mMap[word] = map;}
  void SetTopologyMapTPCNHits(Int_t nhits) {mTPCNHits = nhits;}

  void SetComplete() { mIsComplete=true; };
  void SetInComplete() { mIsComplete=false; };

  void SetHelix(StPhysicalHelixD h) {mHelix=h;} 

  ClassDef(StEStructTrack, 2)   // macro for rootcint
};

inline void  StEStructTrack::evalPt(){ mPt=sqrt((mPx*mPx)+(mPy*mPy)); }
inline void  StEStructTrack::evalPtot(){ mPtot=sqrt((mPx*mPx)+(mPy*mPy)+(mPz*mPz)); }
inline void  StEStructTrack::evalMass() {
    if (mBeta >= 1.0) {
        mMass = -mPtot * sqrt( 1- pow(mBeta,-2));
    } else if (mBeta > 0) {
        mMass = mPtot * sqrt( pow(mBeta,-2) - 1);
    } else {
        mMass = 0;
    }
}
inline const StThreeVectorF& StEStructTrack::NominalTpcExitPoint() const { return mNominalTpcExitPoint; }
inline const StThreeVectorF& StEStructTrack::NominalTpcEntrancePoint() const { return mNominalTpcEntrancePoint; };     
inline const StThreeVectorF& StEStructTrack::MidTpcPoint() const{ return mMidTpcPoint; }; 
inline const StThreeVectorF& StEStructTrack::OuterMidTpcPoint() const{ return mOuterMidTpcPoint; }; 
inline const StThreeVectorF& StEStructTrack::StartPos() const{ return mStartPos; };
inline const StLorentzVectorF& StEStructTrack::FourMomentum() const { return mFourMomentum;};
inline const StPhysicalHelixD& StEStructTrack::Helix() const{ return mHelix;}; 

inline Float_t  StEStructTrack::Xt() const { return mXt;};
inline Float_t  StEStructTrack::Yt() const { return mYt;};
inline Float_t  StEStructTrack::Curvature() const { return mCurvature;};
inline int      StEStructTrack::getYtBin() const { return mytbin; };

#endif


/***********************************************************************
 *
 * $Log: StEStructTrack.h,v $
 * Revision 1.11  2012/11/16 21:24:38  prindle
 * Changes to support reading/writing of EStructEvent. Fill helix as transient and
 * get BField from file (?).
 *
 * Revision 1.10  2011/08/02 20:36:57  prindle
 *   Event: modifications for ZDCCoincidence
 *   Track: big changes in evalPID. These should be superseded when TOF-dEdx
 *          space is understood better.
 *
 * Revision 1.9  2010/09/02 21:26:29  prindle
 *   Track: Added ToF pid information, modify dEdx, add combined pid code.
 *
 * Revision 1.8  2010/03/02 21:47:18  prindle
 *   Support to retrieve track radius when it crosses endplate
 *   Add way to retrieve centrality
 *
 * Revision 1.7  2008/12/02 23:45:49  prindle
 * Added curvature and calculation of OuterMidTpcPoint.
 *
 * Revision 1.6  2006/02/22 22:06:09  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.5  2005/09/14 17:21:20  msd
 * Simplified helix fitting by taking helix from mudst instead of calculating from scratch
 *
 * Revision 1.4  2005/07/07 19:31:13  fisyak
 * Add default for mHelix
 *
 * Revision 1.3  2005/03/03 01:32:03  porter
 * fixed a bug setting 4-momentum and added data (+accessors)
 * to the track class
 *
 * Revision 1.2  2004/06/28 23:23:14  chunhuih
 *
 * add 'const' specification to a set of member functions, including some of
 * the return types, so that they can be used by a const StEStructTrack object.
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
