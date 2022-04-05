/*!
 * \class StV0I
 * \author Gene Van Buren, BNL, 24-Apr-2001
 *
 *               V0 micro dst object interface class
 *               Used for StV0MuDst (reconstructed), StV0Mc (Monte Carlo),
 *               and StXiI (which is used for StXiMuDst and StXiMc).
 *
 */

#ifndef StV0I_hh
#define StV0I_hh
#include "phys_constants.h"
#include <math.h>
#include "StStrangeEvMuDst.hh"
#include "StDecayAngle.hh"

#ifndef StTrackTopologyMap_hh
#include "StEvent/StTrackTopologyMap.h"
#endif

class StStrangeEvMuDst;
R__EXTERN StTrackTopologyMap* gFakeTopoPtr;
static const Float_t M_LAMBDA_2 = ::pow(M_LAMBDA,2);
static const Float_t M_KAON_0_SHORT_2 = ::pow(M_KAON_0_SHORT,2);
static const Float_t M_PROTON_2 = ::pow(M_PROTON,2);
static const Float_t M_ANTIPROTON_2 = ::pow(M_ANTIPROTON,2);
static const Float_t M_PION_PLUS_2 = ::pow(M_PION_PLUS,2);
static const Float_t M_PION_MINUS_2 = ::pow(M_PION_MINUS,2);

class StV0I {
public:
  StV0I() {}
  virtual ~StV0I() {}

// ************************************************************************
/// @name _________________FUNCTIONS_USEFUL_IN_ALL_V0_TYPES_________________
// ************************************************************************

  //@{
  virtual void Clear();
  /// Pointer to event information
          StStrangeEvMuDst* event();
  /// Set pointer to event information
  virtual void SetEvent(StStrangeEvMuDst*);
  //@}

  /// @name Decay properties
  //@{
  /// 3-d decay distance
  virtual Float_t decayLengthV0() const;
  virtual Float_t decayVertexV0X() const=0;
  virtual Float_t decayVertexV0Y() const=0; /// Coordinates of decay vertex
  virtual Float_t decayVertexV0Z() const=0;
          Float_t alphaV0(); /// Armenteros-Podolanski variables
          Float_t ptArmV0();
  /// Lifetime (ctau) assuming (anti)lambda
          Float_t cTauLambda();
  /// Lifetime (ctau) assuming k-short
          Float_t cTauK0Short();
  //@}

  /// @name Momenta
  //@{
  /// Momentum of pos. daughter
          TVector3 momPos();
  virtual Float_t momPosX() const=0;
  virtual Float_t momPosY() const=0; /// Momentum components of pos. daughter
  virtual Float_t momPosZ() const=0;
  /// Momentum of neg. daughter
          TVector3 momNeg();
  virtual Float_t momNegX() const=0;
  virtual Float_t momNegY() const=0; /// Momentum components of neg. daughter
  virtual Float_t momNegZ() const=0;
  /// Momentum of V0
          TVector3 momV0();
  virtual Float_t momV0X() const=0;
  virtual Float_t momV0Y() const=0; /// Momentum components of V0
  virtual Float_t momV0Z() const=0;
  /// Transverse momentum
          Float_t ptV0();
  /// Transverse momentum of pos. daughter
          Float_t ptPos();
  /// Transverse momentum of neg. daughter
          Float_t ptNeg();
  /// Total momentum
          Float_t ptotV0();
  /// Total momentum of pos. daughter
          Float_t ptotPos();
  /// Total momentum of neg. daughter  
          Float_t ptotNeg();
  //@}

  /// @name Mass-dependent properties
  //@{
  /// Energy assuming lambda hypothesis
          Float_t eLambda();
  /// Energy assuming k-short hypothesis
          Float_t eK0Short();
  /// Energy of pos. daughter assuming proton
          Float_t ePosProton();
  /// Energy of pos. daughter assuming pion
          Float_t ePosPion();
  /// Energy of neg. daughter assuming antiproton
          Float_t eNegProton();
  /// Energy of neg. daughter assuming pion
          Float_t eNegPion();
  /// Mass assuming lambda hypothesis
          Float_t massLambda();
  /// Mass assuming antilambda hypothesis
          Float_t massAntiLambda();
  /// Mass assuming k-short hypothesis
          Float_t massK0Short();
  /// Rapidity assuming (anti)lambda
          Float_t rapLambda();
  /// Rapidity assuming k-short
          Float_t rapK0Short();
  //@}

  /// @name Mass-independent properties
  //@{
  /// Polar angle theta
          Float_t thetaV0();
  /// Polar angle theta of pos. daughter
          Float_t thetaPos();
  /// Polar angle theta of neg. daughter
          Float_t thetaNeg();
  /// Pseudorapidity
          Float_t pseudoRapV0();
  /// Pseudorapidity of pos. daughter
          Float_t pseudoRapPos();
  /// Pseudorapidity of neg. daughter
          Float_t pseudoRapNeg();
  //@}
  
  /// @name Transverse mass formulae
  //@{
  /// Transverse mass assuming lambda
          Float_t mtLambda();
  /// Transverse mass assuming k-short
          Float_t mtK0Short();
  /// mt-m0 assuming lambda
          Float_t mtm0Lambda();
  /// mt-m0 assuming k-short
          Float_t mtm0K0Short();
  //@}
  
  /// @name Energy and invariant mass formulae for hypothetical masses
  //@{
  /// Energy of pos. daughter assuming its mass
          Float_t ePosHyp(Float_t mass);
  /// Energy of neg. daughter
          Float_t eNegHyp(Float_t mass);
          Float_t massHypV0(Float_t massPos, Float_t massNeg);
          Float_t eHypV0(Float_t mass);
          Float_t rapHypV0(Float_t mass);
          Float_t mtHypV0(Float_t mass);
  /// Energy, rapidity, mt, mt-m0 of V0 assuming its mass
          Float_t mtm0HypV0(Float_t mass);
  //@}

  /// @name Cosines of decay and polarization angles for different particle hypotheses
  //@{
          Float_t decayCosThetaK0Short();       // k-short    - pos. daughter
          Float_t decayCosThetaLambda();        // lambda     - pos. daughter
          Float_t decayCosThetaAntiLambda();    // antilambda - neg. daughter
          Float_t polCosThetaLambda();          // lambda     - pos. daughter
          Float_t polCosThetaAntiLambda();      // antilambda - neg. daughter
          Float_t decayCosThetaPosLambda();     // lambda     - pos. daughter
          Float_t decayCosThetaNegLambda();     // lambda     - neg. daughter
          Float_t decayCosThetaPosAntiLambda(); // antilambda - pos. daughter
          Float_t decayCosThetaNegAntiLambda(); // antilambda - neg. daughter
  /// This helper function can be used for decayCosTheta of any hypothesis
  /** m1,m2 = parent,daughter mass; charge = positive/negative daughter */
          Float_t dCTV0(Float_t m1, Float_t m2, StChargeSign charge);
  //@}

  /// @name Momenta of daughters in V0 rest frame for different particle hypotheses
  //@{
          TVector3 momPosK0Short();             // k-short    - pos. daughter
          TVector3 momNegK0Short();             // k-short    - neg. daughter
          TVector3 momPosLambda();              // lambda     - neg. daughter
          TVector3 momNegLambda();              // lambda     - neg. daughter
          TVector3 momPosAntiLambda();          // antilambda - pos. daughter
          TVector3 momNegAntiLambda();          // antilambda - neg. daughter
  /// This helper function can be used for the momentum of daughters
  /// in the V0 rest frame of any hypothesis
  /** m1,m2 = parent,daughter mass; charge = positive/negative daughter */
          TVector3 momV0Frame(Float_t m1, Float_t m2, StChargeSign charge);
  //@}

// ************************************************************************
/// @name _______________FUNCTIONS_PRESENTLY_USED_ONLY_BY_MC_______________
// ************************************************************************

  //@{
  /// Returns the decay process for the vertex,
  /// where the decay modes are enumerated in ::decayModeType
  virtual Int_t decayMode() const {return -1;}
  virtual Int_t geantIdParent() const   {return -1;}
  virtual Int_t geantIdPositive() const {return -1;}
  virtual Int_t geantIdNegative() const {return -1;}
  virtual Int_t positiveSimTpcHits() const    {return -1;}
  virtual Int_t positiveCommonTpcHits() const {return -1;}
  virtual Int_t negativeSimTpcHits() const    {return -1;}
  virtual Int_t negativeCommonTpcHits() const {return -1;}
  //@}

// ************************************************************************
/// @name ______________FUNCTIONS_PRESENTLY_USED_ONLY_BY_MuDst______________
// ************************************************************************

  //@{
  /// Pos. daughter track key
  virtual Int_t keyPos() const {return 0;}
  /// Neg. daughter track key
  virtual Int_t keyNeg() const {return 0;}
  /// Pos. daughter track topology map
  virtual StTrackTopologyMap& topologyMapPos() {return (*gFakeTopoPtr);}
  /// Neg. daughter track topology map
  virtual StTrackTopologyMap& topologyMapNeg() {return (*gFakeTopoPtr);}
  /// dE/dX of pos. daughter
  virtual Float_t dedxPos() const {return 0;}
  /// dE/dX of neg. daughter
  virtual Float_t dedxNeg() const {return 0;}
  /// Error on mean of dE/dX of pos. daughter
  virtual Float_t errDedxPos() const {return 0;}
  /// Error on mean of dE/dX of neg. daughter
  virtual Float_t errDedxNeg() const {return 0;}
  /// Number of dE/dX points for pos. daughter
  virtual UShort_t numDedxPos() const {return 0;}
  /// Number of dE/dX points for neg. daughter
  virtual UShort_t numDedxNeg() const {return 0;}
  /// Length of dE/dX track of pos. daughter
  virtual Float_t lenDedxPos() const {return 0;}
  /// Length of dE/dX track of neg. daughter
  virtual Float_t lenDedxNeg() const {return 0;}
  //@}

  /// @name DCAs
  //@{
  /// DCA of v0 daughters at decay vertex
  virtual Float_t dcaV0Daughters() const {return 0;}
  /// DCA of v0 to primary vertex
  virtual Float_t dcaV0ToPrimVertex()  const {return -1;}
  /// DCA of pos v0 daughter to pri vertex
  virtual Float_t dcaPosToPrimVertex() const {return -1;}
  /// DCA of neg v0 daughter to pri vertex
  virtual Float_t dcaNegToPrimVertex() const {return -1;}
  //@}
  
  /// @name Fit/Finding properties
  //@{
  /// Chi square of V0
  virtual Float_t chi2V0()  const {return 0;}
  /// Confidence level of V0
  virtual Float_t clV0()    const {return 0;}
  /// Chi square of pos. daughter
  virtual Float_t chi2Pos() const {return 0;}
  /// Confidence level of pos. daughter
  virtual Float_t clPos()   const {return 0;}
  /// Chi square of neg. daughter
  virtual Float_t chi2Neg() const {return 0;}
  /// Confidence level of neg. daughter
  virtual Float_t clNeg()   const {return 0;}
  /// Detector ID for V0 Vertex
  virtual Long_t  detectorIdV0() {return 0;}
  /// Detector ID for pars used in V0 finder
  virtual Long_t detectorIdPars() {return 0;}
  /// Set the pos. daughter as bad
  virtual void setPosBad() {}
  /// Set the neg. daughter as bad
  virtual void setNegBad() {}
  /// Test whether either daughter is bad
  virtual Bool_t bad() const {return (chi2Pos()<0 || chi2Neg()<0);}
  //@}


protected:
          Float_t Ptot2Pos();
          Float_t Ptot2Neg();
          Float_t Ptot2V0();
          Float_t Pt2V0();
          Float_t MomPosAlongV0();
          Float_t MomNegAlongV0();

  StStrangeEvMuDst *mEvent;       //!                                           
};

inline void StV0I::SetEvent(StStrangeEvMuDst* ev) {
     mEvent = ev;
}

inline StStrangeEvMuDst *StV0I::event() {
     return mEvent;
}

inline Float_t StV0I::decayLengthV0() const {
     if (mEvent)
       return ::sqrt(::pow(decayVertexV0X() - mEvent->primaryVertexX(),2) +
                     ::pow(decayVertexV0Y() - mEvent->primaryVertexY(),2) +
                     ::pow(decayVertexV0Z() - mEvent->primaryVertexZ(),2));
     return 0.;
}

inline TVector3 StV0I::momPos() {
     return TVector3(momPosX(), momPosY(), momPosZ());
}

inline TVector3 StV0I::momNeg() {
     return TVector3(momNegX(), momNegY(), momNegZ());
}

inline TVector3 StV0I::momV0() {
     return TVector3(momV0X(), momV0Y(), momV0Z());
}

inline Float_t StV0I::Ptot2Pos() {
     return (::pow(momPosX(),2) + ::pow(momPosY(),2) + ::pow(momPosZ(),2));
}

inline Float_t StV0I::Ptot2Neg() {
     return (::pow(momNegX(),2) + ::pow(momNegY(),2) + ::pow(momNegZ(),2));
}

inline Float_t StV0I::Pt2V0() {
     return (::pow(momV0X(),2) + ::pow(momV0Y(),2));
}

inline Float_t StV0I::Ptot2V0() {
     return (Pt2V0() + ::pow(momV0Z(),2));
}

inline Float_t StV0I::MomPosAlongV0() {
     Float_t mPtot2V0 = Ptot2V0();
     if (mPtot2V0)
       return (momPosX()*momV0X() + 
               momPosY()*momV0Y() +
               momPosZ()*momV0Z()) / ::sqrt(mPtot2V0);
     return 0.;
}

inline Float_t StV0I::MomNegAlongV0() {
     Float_t mPtot2V0 = Ptot2V0();
     if (mPtot2V0)
       return (momNegX()*momV0X() + 
               momNegY()*momV0Y() +
               momNegZ()*momV0Z()) / ::sqrt(mPtot2V0);
     return 0.;
}

inline Float_t StV0I::alphaV0() {
  Float_t mMomPosAlongV0 = MomPosAlongV0();
  Float_t mMomNegAlongV0 = MomNegAlongV0();
  return (mMomPosAlongV0-mMomNegAlongV0)/
         (mMomPosAlongV0+mMomNegAlongV0);
}

inline Float_t StV0I::ptArmV0() {
  Float_t ptarmsq = Ptot2Pos() - ::pow(MomPosAlongV0(),2);
  return ((ptarmsq > 0.) ? ::sqrt(ptarmsq) : 0.);
}

inline Float_t StV0I::eHypV0(Float_t mass) {
  return ::sqrt(Ptot2V0()+::pow(mass,2));
}

inline Float_t StV0I::eLambda() {
  return ::sqrt(Ptot2V0()+M_LAMBDA_2);
}

inline Float_t StV0I::eK0Short() {
  return ::sqrt(Ptot2V0()+M_KAON_0_SHORT_2);
}

inline Float_t StV0I::ePosHyp(Float_t mass) {
  return ::sqrt(Ptot2Pos()+::pow(mass,2));
}

inline Float_t StV0I::eNegHyp(Float_t mass) {
  return ::sqrt(Ptot2Neg()+::pow(mass,2));
}

inline Float_t StV0I::ePosProton() {
  return ::sqrt(Ptot2Pos()+M_PROTON_2);
}

inline Float_t StV0I::eNegProton() {
  return ::sqrt(Ptot2Neg()+M_ANTIPROTON_2);
}

inline Float_t StV0I::ePosPion() {
  return ::sqrt(Ptot2Pos()+M_PION_PLUS_2);
}

inline Float_t StV0I::eNegPion() {
  return ::sqrt(Ptot2Neg()+M_PION_MINUS_2);
}

inline Float_t StV0I::massHypV0(Float_t massPos, Float_t massNeg) {
  Float_t msq = ::pow(ePosHyp(massPos)+eNegHyp(massNeg),2)-Ptot2V0();
  return ((msq > 0.) ? ::sqrt(msq) : 0.);
}

inline Float_t StV0I::massLambda() {
  return ::sqrt(::pow(ePosProton()+eNegPion(),2)-Ptot2V0());
}

inline Float_t StV0I::massAntiLambda() {
  return ::sqrt(::pow(eNegProton()+ePosPion(),2)-Ptot2V0());
}

inline Float_t StV0I::massK0Short() {
  return ::sqrt(::pow(ePosPion()+eNegPion(),2)-Ptot2V0());
}

inline Float_t StV0I::rapHypV0(Float_t mass) {
  Float_t ehyp = eHypV0(mass);
  Float_t mMomV0Z = momV0Z();
  return 0.5*::log((ehyp+mMomV0Z)/(ehyp-mMomV0Z));
}

inline Float_t StV0I::rapLambda() {
  Float_t ela = eLambda();
  Float_t mMomV0Z = momV0Z();
  return 0.5*::log((ela+mMomV0Z)/(ela-mMomV0Z));
}

inline Float_t StV0I::rapK0Short() {
  Float_t ek0 = eK0Short();
  Float_t mMomV0Z = momV0Z();
  return 0.5*::log((ek0+mMomV0Z)/(ek0-mMomV0Z));
}

inline Float_t StV0I::cTauLambda() {
  return M_LAMBDA*decayLengthV0()/::sqrt(Ptot2V0());
}

inline Float_t StV0I::cTauK0Short() {
  return M_KAON_0_SHORT*decayLengthV0()/::sqrt(Ptot2V0());
}

inline Float_t StV0I::ptPos() {
  return ::sqrt(Ptot2Pos()-::pow(momPosZ(),2));
}

inline Float_t StV0I::ptotPos() {
  return ::sqrt(Ptot2Pos());
}

inline Float_t StV0I::ptNeg() {
  return ::sqrt(Ptot2Neg()-::pow(momNegZ(),2));
}

inline Float_t StV0I::ptotNeg() {
  return ::sqrt(Ptot2Neg());
}

inline Float_t StV0I::ptV0() {
  return ::sqrt(Pt2V0());
}

inline Float_t StV0I::ptotV0() {
  return ::sqrt(Ptot2V0());
}

inline Float_t StV0I::thetaV0() {
  return acos(momV0Z()/ptotV0());
}

inline Float_t StV0I::pseudoRapV0() {
  return (-::log(tan(thetaV0()/2.)));
}

inline Float_t StV0I::thetaPos() {
  return acos(momPosZ()/ptotPos());
}

inline Float_t StV0I::pseudoRapPos() {
  return (-::log(tan(thetaPos()/2.)));
}

inline Float_t StV0I::thetaNeg() {
  return acos(momNegZ()/ptotNeg());
}

inline Float_t StV0I::pseudoRapNeg() {
  return (-::log(tan(thetaNeg()/2.)));
}

inline Float_t StV0I::mtHypV0(Float_t mass) {
  return ::sqrt(Pt2V0()+::pow(mass,2));
}

inline Float_t StV0I::mtLambda() {
  return ::sqrt(Pt2V0()+M_LAMBDA_2);
}

inline Float_t StV0I::mtK0Short() {
  return ::sqrt(Pt2V0()+M_KAON_0_SHORT_2);
}

inline Float_t StV0I::mtm0HypV0(Float_t mass) {
  return (mtHypV0(mass)-mass);
}

inline Float_t StV0I::mtm0Lambda() {
  return (mtK0Short()-M_LAMBDA);
}

inline Float_t StV0I::mtm0K0Short() {
  return (mtK0Short()-M_KAON_0_SHORT);
}

inline Float_t StV0I::decayCosThetaK0Short() {    // default: use pos. daughter
  return dCTV0(M_KAON_0_SHORT,M_PION_PLUS,positive);
}

inline Float_t StV0I::decayCosThetaLambda() {     // default: use pos. daughter
  return decayCosThetaPosLambda();
}

inline Float_t StV0I::decayCosThetaAntiLambda() { // default: use neg. daughter
  return decayCosThetaNegAntiLambda();
}

inline Float_t StV0I::polCosThetaLambda() {       // default: use pos. daughter
  return StDecayAngle::polarityCosTheta(
    momV0X(),momV0Y(),momV0Z(),M_LAMBDA,
    momPosX(),momPosY(),momPosZ(),M_PROTON);
}

inline Float_t StV0I::polCosThetaAntiLambda() {   // default: use neg. daughter
  return StDecayAngle::polarityCosTheta(
    momV0X(),momV0Y(),momV0Z(),M_LAMBDA,
    momNegX(),momNegY(),momNegZ(),M_PROTON);
}

inline Float_t StV0I::decayCosThetaPosLambda() {
  return dCTV0(M_LAMBDA,M_PROTON,positive);
}

inline Float_t StV0I::decayCosThetaNegLambda() {
  return dCTV0(M_LAMBDA,M_PION_MINUS,negative);
}

inline Float_t StV0I::decayCosThetaNegAntiLambda() {
  return dCTV0(M_ANTILAMBDA,M_ANTIPROTON,negative);
}

inline Float_t StV0I::decayCosThetaPosAntiLambda() {
  return dCTV0(M_ANTILAMBDA,M_PION_PLUS,positive);
}

inline Float_t StV0I::dCTV0(Float_t m1, Float_t m2, StChargeSign charge) {
  return ( (charge == positive) ?
    StDecayAngle::decayCosTheta(momV0X() ,momV0Y() ,momV0Z() ,m1,
                                momPosX(),momPosY(),momPosZ(),m2) :
    StDecayAngle::decayCosTheta(momV0X() ,momV0Y() ,momV0Z() ,m1,
                                momNegX(),momNegY(),momNegZ(),m2) );
}

inline TVector3 StV0I::momPosK0Short() {
  return momV0Frame(M_KAON_0_SHORT,M_PION_PLUS,positive);
}

inline TVector3 StV0I::momNegK0Short() {
  return momV0Frame(M_KAON_0_SHORT,M_PION_MINUS,negative);
}

inline TVector3 StV0I::momPosLambda() {
  return momV0Frame(M_LAMBDA,M_PROTON,positive);
}

inline TVector3 StV0I::momNegLambda() {
  return momV0Frame(M_LAMBDA,M_PION_MINUS,negative);
}

inline TVector3 StV0I::momPosAntiLambda() {
  return momV0Frame(M_ANTILAMBDA,M_PION_PLUS,positive);
}

inline TVector3 StV0I::momNegAntiLambda() {
  return momV0Frame(M_ANTILAMBDA,M_ANTIPROTON,negative);
}

inline TVector3 StV0I::momV0Frame(Float_t m1, Float_t m2, StChargeSign charge) {
  return ( (charge == positive) ?
    StDecayAngle::getShiftedDaughter(momV0X() ,momV0Y() ,momV0Z() ,m1,
                                     momPosX(),momPosY(),momPosZ(),m2) :
    StDecayAngle::getShiftedDaughter(momV0X() ,momV0Y() ,momV0Z() ,m1,
                                     momNegX(),momNegY(),momNegZ(),m2) );
}

inline void StV0I::Clear() {
  mEvent = 0;
}

#endif


/***********************************************************************
 * $Id: StV0I.hh,v 3.13 2011/05/27 18:25:32 genevb Exp $
 * $Log: StV0I.hh,v $
 * Revision 3.13  2011/05/27 18:25:32  genevb
 * Propagate StTrack::key => Int_t to other codes
 *
 * Revision 3.12  2008/07/10 16:16:55  genevb
 * Allow for marking of bad tracks -> bad secondary vertices
 *
 * Revision 3.11  2003/10/26 06:06:01  genevb
 * Added checks for sqrt of neg. numbers
 *
 * Revision 3.10  2003/09/02 17:59:04  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 3.9  2003/05/30 21:20:19  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.8  2003/02/10 15:47:14  genevb
 * Fixed mt calculations
 *
 * Revision 3.7  2002/07/30 20:07:52  genevb
 * Better cTau calcs
 *
 * Revision 3.6  2002/04/30 16:02:48  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.5  2002/02/10 15:29:09  genevb
 * Additional functions for momenta of decay daughters in CM frame
 *
 * Revision 3.4  2001/11/28 05:14:59  genevb
 * Additional decay angle functions
 *
 * Revision 3.3  2001/11/05 23:41:06  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.2  2001/08/23 13:20:58  genevb
 * Many bug workarounds...
 *
 * Revision 3.1  2001/05/04 20:15:14  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 *
 ***********************************************************************/
