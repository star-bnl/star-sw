/***************************************************************************
 *
 * $Id: StHbtPair.hh,v 1.16 2002/02/28 14:18:36 rcwells Exp $
 *
 * Author: Brian Laziuk, Yale University
 *         slightly modified by Mike Lisa
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    the Pair object is passed to the PairCuts for verification, and
 *    then to the AddRealPair and AddMixedPair methods of the
 *    Correlation Functions
 *
 ***************************************************************************
 *
 * $Log: StHbtPair.hh,v $
 * Revision 1.16  2002/02/28 14:18:36  rcwells
 * Added emissionAngle function to StHbtPair
 *
 * Revision 1.15  2001/12/14 23:11:30  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 * Revision 1.14  2001/04/03 21:04:36  kisiel
 *
 *
 *   Changes needed to make the Theoretical code
 *   work. The main code is the ThCorrFctn directory.
 *   The most visible change is the addition of the
 *   HiddenInfo to StHbtPair.
 *
 * Revision 1.13  2001/03/28 22:35:23  flierl
 * changes and bugfixes in qYKP*
 * add pairrapidity
 *
 * Revision 1.12  2001/01/22 22:56:40  laue
 * Yano-Koonin-Podgoretskii Parametrisation added
 *
 * Revision 1.11  2000/10/26 16:09:16  lisa
 * Added OpeningAngle PairCut class and method to StHbtPair
 *
 * Revision 1.10  2000/10/05 23:09:05  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 * Revision 1.9  2000/07/17 20:03:17  lisa
 * Implemented tools for addressing and assessing trackmerging
 *
 * Revision 1.8  2000/04/04 16:13:09  lisa
 * StHbtPair:quality() now returns normalized value (and so is double) and add a CorrFctn which looks at quality()
 *
 * Revision 1.7  2000/04/03 22:09:12  rcwells
 * Add member function ... quality().
 *
 * Revision 1.6  2000/02/13 21:13:34  lisa
 * changed ambiguous StHbtPair::fourMomentum() to fourMomentumSum() and fourMomentumDiff() and fixed related bug in QvecCorrFctn
 *
 * Revision 1.5  2000/01/25 17:35:17  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.4  1999/07/29 16:16:34  lisa
 * Selemons upgrade of StHbtPair class
 *
 * Revision 1.3  1999/07/22 18:49:10  lisa
 * Implement idea of Fabrice to not create and delete StHbtPair all the time
 *
 * Revision 1.2  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef ST_HBT_PAIR_HH
#define ST_HBT_PAIR_HH

#include <utility>

#include "StHbtMaker/Infrastructure/StHbtParticle.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtPair {
public:
  StHbtPair();
  StHbtPair(StHbtParticle*, StHbtParticle*);
  

  ~StHbtPair();
  //StHbtPair(const StHbtPair&);
  //StHbtPair& operator=(const StHbtPair&);

  // track Gets:
  StHbtParticle* track1() const;
  StHbtParticle* track2() const;
  // track Sets:
  void SetTrack1(const StHbtParticle* trkPtr);
  void SetTrack2(const StHbtParticle* trkPtr);

  StHbtLorentzVector fourMomentumDiff() const;
  StHbtLorentzVector fourMomentumSum() const;
  double qInv() const;
  double kT()   const;
  double mInv() const;
  // pair rapidity
  double rap() const;
  double emissionAngle();

  // Bertsch-Pratt momentum components in Pair Frame - written by Bekele/Humanic
  double qSidePf() const;
  double qOutPf() const;
  double qLongPf() const;
   
  // Bertsch-Pratt momentum components in Local CMS (longitudinally comoving) frame
  // - written by Bekele/Humanic
  double qSideCMS() const;
  double qOutCMS() const;
  double qLongCMS() const;

  double dKSide() const;
  double dKOut() const;
  double dKLong() const;

  // Bertsch-Pratt momentum components in a longitudinally boosted frame
  // the argument is the beta of the longitudinal boost (default is 0.0, meaning lab frame)
  // - written by Bekele/Humanic
  double qSideBf(double beta=0.0) const;
  double qOutBf(double beta=0.0) const;
  double qLongBf(double beta=0.0) const;

  // Yano-Koonin-Podgoretskii Parametrisation 
  // source rest frame (usually lab frame)
  void qYKPCMS(double& qP, double& qT, double& q0) const ;
  // longitudinal comoving frame
  void qYKPLCMS(double& qP, double& qT, double& q0) const ;
  // pair rest frame
  void qYKPPF(double& qP, double& qT, double& q0) const ;


  double quality() const;

  // the following two methods calculate the "nominal" separation of the tracks 
  // at the inner field cage (EntranceSeparation) and when they exit the TPC,
  // which may be at the outer field cage, or at the endcaps.
  // "nominal" means that the tracks are assumed to start at (0,0,0).  Making this
  // assumption is important for the Event Mixing-- it is not a mistake. - MALisa
  double NominalTpcExitSeparation() const;
  double NominalTpcEntranceSeparation() const;

  double NominalTpcAverageSeparation() const;


  double pInv() const;
  double KStar() const;
  double KStarFlipped() const;
  double CVK() const;
  double CVKFlipped() const;
  double qInvFlippedXY() const;

  double OpeningAngle() const;

  // Fabrice Private <<<
  double KStarSide() const;
  double KStarOut() const;
  double KStarLong() const;

  float PionPairProbability() const;
  float ElectronPairProbability() const;
  float KaonPairProbability() const;
  float ProtonPairProbability() const;
  float KaonPionPairProbability() const;

  double dcaInsideTpc() const;
  double quality2() const;

  double KStarGlobal() const;
  double CVKGlobal() const;
  double KStarSideGlobal() const;
  double KStarOutGlobal() const;
  double KStarLongGlobal() const;

  void setMergingPar(double aMaxDuInner, double aMaxDzInner,
		     double aMaxDuOuter, double aMaxDzOuter);
  void setDefaultHalfFieldMergingPar();
  void setDefaultFullFieldMergingPar();
  double getFracOfMergedRow() const;
  double getClosestRowAtDCA() const;
  double getWeightedAvSep() const;
  // >>>

private:
  StHbtParticle* mTrack1;
  StHbtParticle* mTrack2;

  mutable short mNonIdParNotCalculated;
  mutable double mDKSide;
  mutable double mDKOut;
  mutable double mDKLong;
  mutable double mCVK;
  mutable double kStarCalc;
  void calcNonIdPar() const;

  mutable short mNonIdParNotCalculatedGlobal;
  mutable double mDKSideGlobal;
  mutable double mDKOutGlobal;
  mutable double mDKLongGlobal;
  mutable double kStarCalcGlobal;
  mutable double mCVKGlobal;
  void calcNonIdParGlobal() const;

  mutable short mMergingParNotCalculated;
  mutable double mWeightedAvSep;
  mutable double mFracOfMergedRow;
  mutable short mClosestRowAtDCA;
  static double mMaxDuInner;
  static double mMaxDzInner;
  static double mMaxDuOuter;
  static double mMaxDzOuter;
  void calcMergingPar() const;

  void resetParCalculated();

};

inline void StHbtPair::resetParCalculated(){
  mNonIdParNotCalculated=1;
  mNonIdParNotCalculatedGlobal=1;
  mMergingParNotCalculated=1;
}

inline void StHbtPair::SetTrack1(const StHbtParticle* trkPtr){
  mTrack1=(StHbtParticle*)trkPtr;
  resetParCalculated();
}
inline void StHbtPair::SetTrack2(const StHbtParticle* trkPtr){
  mTrack2=(StHbtParticle*)trkPtr;
  resetParCalculated();
}

inline StHbtParticle* StHbtPair::track1() const {return mTrack1;}
inline StHbtParticle* StHbtPair::track2() const {return mTrack2;}

inline double StHbtPair::dKSide() const{
  if(mNonIdParNotCalculated) calcNonIdPar();
  return mDKSide;
}
inline double StHbtPair::dKOut() const{
  if(mNonIdParNotCalculated) calcNonIdPar();
  return mDKOut;
}
inline double StHbtPair::dKLong() const{
  if(mNonIdParNotCalculated) calcNonIdPar();
  return mDKLong;
}
inline double StHbtPair::KStar() const{
  if(mNonIdParNotCalculated) calcNonIdPar();
  return kStarCalc;
}
inline double StHbtPair::qInv() const {
  StHbtLorentzVector tDiff = (mTrack1->FourMomentum()-mTrack2->FourMomentum());
  return ( -1.* tDiff.m());
}

// Fabrice private <<<
inline double StHbtPair::KStarSide() const{
  if(mNonIdParNotCalculated) calcNonIdPar();
  return mDKSide;//mKStarSide;
}
inline double StHbtPair::KStarOut() const{
  if(mNonIdParNotCalculated) calcNonIdPar();
  return mDKOut;//mKStarOut;
}
inline double StHbtPair::KStarLong() const{
  if(mNonIdParNotCalculated) calcNonIdPar();
  return mDKLong;//mKStarLong;
}
inline double StHbtPair::CVK() const{
  if(mNonIdParNotCalculated) calcNonIdPar();
  return mCVK;
}

inline double StHbtPair::KStarGlobal() const{
  if(mNonIdParNotCalculatedGlobal) calcNonIdParGlobal();
  return kStarCalcGlobal;
}
inline double StHbtPair::KStarSideGlobal() const{
  if(mNonIdParNotCalculatedGlobal) calcNonIdParGlobal();
  return mDKSideGlobal;//mKStarSide;
}
inline double StHbtPair::KStarOutGlobal() const{
  if(mNonIdParNotCalculatedGlobal) calcNonIdParGlobal();
  return mDKOutGlobal;//mKStarOut;
}
inline double StHbtPair::KStarLongGlobal() const{
  if(mNonIdParNotCalculatedGlobal) calcNonIdParGlobal();
  return mDKLongGlobal;//mKStarLong;
}
inline double StHbtPair::CVKGlobal() const{
  if(mNonIdParNotCalculatedGlobal) calcNonIdParGlobal();
  return mCVKGlobal;
}


inline float StHbtPair::PionPairProbability() const{
  return (mTrack1->Track()->PidProbPion()) * 
         (mTrack2->Track()->PidProbPion());
}
inline float StHbtPair::ElectronPairProbability() const{
  return (mTrack1->Track()->PidProbElectron()) * 
         (mTrack2->Track()->PidProbElectron());
}
inline float StHbtPair::KaonPairProbability() const{
  return (mTrack1->Track()->PidProbKaon()) * 
         (mTrack2->Track()->PidProbKaon());
}
inline float StHbtPair::ProtonPairProbability() const{
  return (mTrack1->Track()->PidProbProton()) * 
         (mTrack2->Track()->PidProbProton());
}
inline float StHbtPair::KaonPionPairProbability() const{
  return (mTrack1->Track()->PidProbKaon()) * 
         (mTrack2->Track()->PidProbPion());
}

inline double StHbtPair::getFracOfMergedRow() const{
  if(mMergingParNotCalculated) calcMergingPar();
  return mFracOfMergedRow;
}
inline double StHbtPair::getClosestRowAtDCA() const { 
  if(mMergingParNotCalculated) calcMergingPar();
  return mClosestRowAtDCA;
}
inline double StHbtPair::getWeightedAvSep() const {
  if(mMergingParNotCalculated) calcMergingPar();
  return mWeightedAvSep;
}
#endif
