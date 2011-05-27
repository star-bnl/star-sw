/*!
 * \class StXiI
 * \author Gene Van Buren, BNL, 24-Apr-2001
 *
 *               Xi (cascade) micro dst object interface class
 *               Used for StXiMuDst (reconstructed), StXiMc (Monte Carlo)
 *
 */

/*! \file StXiI.hh */

#ifndef StXiI_hh
#define StXiI_hh

#include "StV0I.hh"
static const Float_t M_OMEGA_2 = ::pow(M_OMEGA_MINUS,2);
static const Float_t M_XI_2 = ::pow(M_XI_MINUS,2);
static const Float_t M_KAON_MINUS_2 = ::pow(M_KAON_MINUS,2);

/// Enumeration of Xi daughter types
enum StXiDaughter {bachelor, v0};


class StXiI : public virtual StV0I {
public:
  StXiI();
  virtual ~StXiI() {}

// ************************************************************************
/// @name _________________FUNCTIONS_USEFUL_IN_ALL_XI_TYPES_________________
// ************************************************************************

  //@{
  /// Particle charge
  virtual Int_t   charge() const=0;
  //@}

  /// @name Decay properties
  //@{
  /// 3-d distance of V0 decay from primary vertex
          Float_t decayDistanceV0() const;
  /// 3-d decay distance of V0 from Xi
  virtual Float_t decayLengthV0() const;
  /// 3-d decay distance of Xi
          Float_t decayLengthXi() const;
  virtual Float_t decayVertexXiX() const=0;
  virtual Float_t decayVertexXiY() const=0; /// Coordinates of decay vertex
  virtual Float_t decayVertexXiZ() const=0;
          Float_t alphaXi(); /// Armenteros-Podolanski variables
          Float_t ptArmXi();
  /// Lifetime (ctau) assuming (anti)Omega
          Float_t cTauOmega();
  /// Lifetime (ctau) assuming (anti)Xi
          Float_t cTauXi();
  //@}

  /// @name Momenta
  //@{
  /// Momentum of bachelor
          TVector3 momBachelor();
  virtual Float_t momBachelorX() const=0;
  virtual Float_t momBachelorY() const=0; /// Momentum components of bachelor
  virtual Float_t momBachelorZ() const=0;

  /// Momentum of Xi/Omega at decay vertex
  virtual TVector3 momXi()=0;
  virtual Float_t momXiX()=0;
  virtual Float_t momXiY()=0; /// Momentum components of Xi/Omega at decay vertex
  virtual Float_t momXiZ()=0;

  /// Momentum of Xi/Omega at primary vertex
  virtual TVector3 momXiAtPrimVertex()=0;
  virtual Float_t momXiAtPrimVertexX()=0;
  virtual Float_t momXiAtPrimVertexY()=0; /// Momentum components of Xi/Omega at primary vertex
  virtual Float_t momXiAtPrimVertexZ()=0;

  /// Transverse momentum of Xi/Omega
          Float_t ptXi();
  /// Transverse momentum of bachelor
          Float_t ptBachelor();
  /// Total momentum of Xi/Omega
          Float_t ptotXi();
  /// Total momentum of bachelor
          Float_t ptotBachelor();
  //@}

  /// @name Mass-dependent properties
  //@{
  /// Energy assuming Omega hypothesis
          Float_t eOmega();
  /// Energy assuming Xi hypothesis
          Float_t eXi();
  /// Energy of bachelor assuming pion
          Float_t eBachelorPion();
  /// Energy of bachelor assuming kaon
          Float_t eBachelorKaon();
  /// Mass assuming Omega hypothesis
          Float_t massOmega();
  /// Mass assuming Xi hypothesis
          Float_t massXi();
  /// Rapidity assuming (anti)Omega
          Float_t rapOmega();
  /// Rapidity assuming (anti)Xi
          Float_t rapXi();
  //@}

  /// @name Mass-independent properties
  //@{
  /// Polar angle theta of Xi/Omega
          Float_t thetaXi();
  /// Polar angle theta of bachelor
          Float_t thetaBachelor();
  /// Pseudorapidity of Xi/Omega
          Float_t pseudoRapXi();
  /// Pseudorapidity of bachelor
          Float_t pseudoRapBachelor();
  //@}

  /// @name Transverse mass formulae
  //@{
  /// Transverse mass assuming (anti)Omega
          Float_t mtOmega();
  /// Transverse mass assuming (anti)Xi
          Float_t mtXi();
  /// mt-m0 assuming (anti)Omega
          Float_t mtm0Omega();
  /// mt-m0 assuming (anti)Xi
          Float_t mtm0Xi();
  //@}

  /// @name Energy and invariant mass formulae for hypothetical masses
  //@{
  /// Energy of bachelor assuming its mass
          Float_t eBachelorHyp(Float_t mass);
  /// Invariant mass assuming daughter masses
          Float_t massHypXi(Float_t massV0, Float_t massBachelor);
          Float_t eHypXi(Float_t mass);
          Float_t rapHypXi(Float_t mass);
          Float_t mtHypXi(Float_t mass);
  /// Energy, rapidity, mt, mt-m0 of Xi assuming its mass
          Float_t mtm0HypXi(Float_t mass);
  //@}

  /// @name Cosines of decay and polarization angles for different particle hypotheses
  //@{
          Float_t decayCosThetaBachelorXi();    // xi    - bachelor
          Float_t decayCosThetaV0Xi();          // xi    - V0 
          Float_t decayCosThetaBachelorOmega(); // omega - bachelor
          Float_t decayCosThetaV0Omega();       // omega - V0
  /// This helper function can be used for decayCosTheta of any hypothesis
  /** m1,m2 = parent,daughter mass; type = bachelor/v0 daughter */
          Float_t dCTXi(Float_t m1, Float_t m2, StXiDaughter type);
  //@}

  /// @name Momenta of daughters in Xi rest frame for different particle hypotheses
  //@{
          TVector3 momBachelorXi();             // xi    - bachelor
          TVector3 momV0Xi();                   // xi    - V0
          TVector3 momBachelorOmega();          // omega - bachelor
          TVector3 momV0Omega();                // omega - V0
  /// This helper function can be used for the momentum of daughters
  /// in the Xi rest frame of any hypothesis
  /** m1,m2 = parent,daughter mass; type = bachelor/v0 daughter */
          TVector3 momXiFrame(Float_t m1, Float_t m2, StXiDaughter type);
  //@}

// ************************************************************************
/// @name _______________FUNCTIONS_PRESENTLY_USED_ONLY_BY_MC_______________
// ************************************************************************

  //@{
  virtual Int_t V0Index() {return -1;}
  //@}
  
// ************************************************************************
/// @name ______________FUNCTIONS_PRESENTLY_USED_ONLY_BY_MuDst______________
// ************************************************************************

  //@{
  /// Bachelor track key
  virtual Int_t keyBachelor() const {return 0;}
  /// Bachelor track topology map
  virtual StTrackTopologyMap& topologyMapBachelor() {return (*gFakeTopoPtr);}
  /// dE/dX of bachelor
  virtual Float_t dedxBachelor() const {return 0;}
  /// Error on mean of dE/dX of bachelor
  virtual Float_t errDedxBachelor() const {return 0;}
  /// Number of dE/dX points for bachelor
  virtual UShort_t numDedxBachelor() const {return 0;}
  /// Length of dE/dX track for bachelor
  virtual Float_t lenDedxBachelor() const {return 0;}
  //@}

  /// @name DCAs
  //@{
  /// DCA of xi daughters at decay vertex
  virtual Float_t dcaXiDaughters() const {return 0;}
  /// DCA of bachelor to primary vertex
  virtual Float_t dcaBachelorToPrimVertex() const {return -1;}
  /// DCA of xi to primary vertex
  virtual Float_t dcaXiToPrimVertex() const {return -1;}
  //@}

  /// @name Fit/Finding properties
  //@{
  /// Chi square of Xi (used only by MuDst)
  virtual Float_t chi2Xi() const {return 0;}
  /// Confidence level of Xi
  virtual Float_t clXi()   const {return 0;}
  /// Chi square of bachelor
  virtual Float_t chi2Bachelor() const {return 0;}
  /// Confidence level of bachelor
  virtual Float_t clBachelor()   const {return 0;}
  /// Detector ID for Xi vertex
  virtual Long_t  detectorIdXi() {return 0;}
  /// Detector ID for pars used in Xi finder
  virtual Long_t detectorIdPars() {return 0;}
  /// Set the bachelor as bad
  virtual void setBachelorBad() {}
  /// Test whether any child is bad
  virtual Bool_t bad() const {return (chi2Bachelor()<0 || StV0I::bad());}
  //@}


protected:
          Float_t Ptot2Bachelor();
          Float_t Ptot2Xi();
          Float_t Pt2Xi();
          Float_t MomBachelorAlongXi();
          Float_t MomV0AlongXi();
};

inline StXiI::StXiI() : StV0I() {}

inline Float_t StXiI::decayDistanceV0() const {
  return StV0I::decayLengthV0();
}

inline Float_t StXiI::decayLengthV0() const {
  return ::sqrt(::pow(decayVertexV0X() - decayVertexXiX(),2) +
                ::pow(decayVertexV0Y() - decayVertexXiY(),2) +
                ::pow(decayVertexV0Z() - decayVertexXiZ(),2));
}

inline Float_t StXiI::decayLengthXi() const {
  return ::sqrt(::pow(decayVertexXiX() - mEvent->primaryVertexX(),2) +
                ::pow(decayVertexXiY() - mEvent->primaryVertexY(),2) +
                ::pow(decayVertexXiZ() - mEvent->primaryVertexZ(),2));
}

inline TVector3 StXiI::momBachelor() {
     return TVector3(momBachelorX(), momBachelorY(), momBachelorZ());
}

inline Float_t StXiI::alphaXi() {
  Float_t mMomBachelorAlongXi = MomBachelorAlongXi();
  Float_t mMomV0AlongXi = MomV0AlongXi();
  return (((Float_t) charge()) * (mMomBachelorAlongXi-mMomV0AlongXi)/
                                 (mMomBachelorAlongXi+mMomV0AlongXi));
}

inline Float_t StXiI::ptArmXi() {
  Float_t ptarmsq = Ptot2V0() - ::pow(MomV0AlongXi(),2);
  return ((ptarmsq > 0.) ? ::sqrt(ptarmsq) : 0.);
}

inline Float_t StXiI::eOmega() {
  return ::sqrt(Ptot2Xi()+M_OMEGA_2);
}

inline Float_t StXiI::eXi() {
  return ::sqrt(Ptot2Xi()+M_XI_2);
}

inline Float_t StXiI::eHypXi(Float_t mass) {
  return ::sqrt(Ptot2Xi()+::pow(mass,2));
}

inline Float_t StXiI::eBachelorHyp(Float_t mass) {
  return ::sqrt(Ptot2Bachelor()+::pow(mass,2));
}

inline Float_t StXiI::eBachelorPion() {
  return ::sqrt(Ptot2Bachelor()+M_PION_MINUS_2);
}

inline Float_t StXiI::eBachelorKaon() {
  return ::sqrt(Ptot2Bachelor()+M_KAON_MINUS_2);
}

inline Float_t StXiI::massHypXi(Float_t massV0, Float_t massBachelor) {
  Float_t msq = ::pow(eHypV0(massV0)+eBachelorHyp(massBachelor),2)-Ptot2Xi();
  return ((msq > 0.) ? ::sqrt(msq) : 0.);
}

inline Float_t StXiI::massOmega() {
  return ::sqrt(::pow(eLambda()+eBachelorKaon(),2)-Ptot2Xi());
}

inline Float_t StXiI::massXi() {
  return ::sqrt(::pow(eLambda()+eBachelorPion(),2)-Ptot2Xi());
}

inline Float_t StXiI::rapHypXi(Float_t mass) {
  Float_t mMomXiZ = momXiZ();
  Float_t eX = eHypXi(mass);
  return 0.5*::log((eX+mMomXiZ)/(eX-mMomXiZ));
}

inline Float_t StXiI::rapOmega() {
  Float_t mMomXiZ = momXiZ();
  Float_t eom = eOmega();
  return 0.5*::log((eom+mMomXiZ)/(eom-mMomXiZ));
}

inline Float_t StXiI::rapXi() {
  Float_t mMomXiZ = momXiZ();
  Float_t exi = eXi();
  return 0.5*::log((exi+mMomXiZ)/(exi-mMomXiZ));
}

inline Float_t StXiI::cTauOmega() {
  return M_OMEGA_MINUS*decayLengthXi()/::sqrt(Ptot2Xi());
}

inline Float_t StXiI::cTauXi() {
  return M_XI_MINUS*decayLengthXi()/::sqrt(Ptot2Xi());
}

inline Float_t StXiI::ptBachelor() {
  return ::sqrt(Ptot2Bachelor()-::pow(momBachelorZ(),2));
}

inline Float_t StXiI::ptotBachelor() {
  return ::sqrt(Ptot2Bachelor());
}

inline Float_t StXiI::ptXi() {
  return ::sqrt(Pt2Xi());
}

inline Float_t StXiI::ptotXi() {
  return ::sqrt(Ptot2Xi());
}

inline Float_t StXiI::thetaXi() {
  return acos(momXiZ()/ptotXi());
}

inline Float_t StXiI::pseudoRapXi() {
  return (-::log(tan(thetaXi()/2.)));
}

inline Float_t StXiI::thetaBachelor() {
  return acos(momBachelorZ()/ptotBachelor());
}

inline Float_t StXiI::pseudoRapBachelor() {
  return (-::log(tan(thetaBachelor()/2.)));
}

inline Float_t StXiI::mtHypXi(Float_t mass) {
  return ::sqrt(Pt2Xi() + ::pow(mass,2));
}

inline Float_t StXiI::mtOmega() {
  return ::sqrt(Pt2Xi() + M_OMEGA_2);
}

inline Float_t StXiI::mtXi() {
  return ::sqrt(Pt2Xi() + M_XI_2);
}

inline Float_t StXiI::mtm0HypXi(Float_t mass) {
  return (mtHypXi(mass) - mass);
}

inline Float_t StXiI::mtm0Omega() {
  return (mtOmega() - M_OMEGA_MINUS);
}

inline Float_t StXiI::mtm0Xi() {
  return (mtXi() - M_XI_MINUS);
}

inline Float_t StXiI::Ptot2Bachelor () {
  return (::pow(momBachelorX(),2) +
          ::pow(momBachelorY(),2) + ::pow(momBachelorZ(),2));
}

inline Float_t StXiI::Pt2Xi() {
  return (::pow(momXiX(),2) + ::pow(momXiY(),2));
}

inline Float_t StXiI::Ptot2Xi() {
  return (Pt2Xi() + ::pow(momXiZ(),2));
}

inline Float_t StXiI::MomBachelorAlongXi() {
  Float_t mPtot2Xi = Ptot2Xi();
  if (mPtot2Xi)
    return (momBachelorX()*momXiX() + 
            momBachelorY()*momXiY() +
            momBachelorZ()*momXiZ()) / ::sqrt(mPtot2Xi);
  return 0.;
}

inline Float_t StXiI::MomV0AlongXi() {
  Float_t mPtot2Xi = Ptot2Xi();
  if (mPtot2Xi)
    return (momV0X()*momXiX() + 
            momV0Y()*momXiY() + 
            momV0Z()*momXiZ()) / ::sqrt(mPtot2Xi);
  return 0.;
}

inline Float_t StXiI::decayCosThetaBachelorXi() {
  return dCTXi(M_XI_MINUS,M_PION_MINUS,bachelor);
}

inline Float_t StXiI::decayCosThetaV0Xi() {
  return dCTXi(M_XI_MINUS,M_LAMBDA,v0);
}

inline Float_t StXiI::decayCosThetaBachelorOmega() {
  return dCTXi(M_OMEGA_MINUS,M_KAON_MINUS,bachelor);
}

inline Float_t StXiI::decayCosThetaV0Omega() {
  return dCTXi(M_OMEGA_MINUS,M_LAMBDA,v0);
}

inline Float_t StXiI::dCTXi(Float_t m1, Float_t m2, StXiDaughter type) {
  return ( (type == bachelor) ?
    StDecayAngle::decayCosTheta(momXiX(),momXiY(),momXiZ(),m1,
              momBachelorX(),momBachelorY(),momBachelorZ(),m2) :
    StDecayAngle::decayCosTheta(momXiX(),momXiY(),momXiZ(),m1,
                                momV0X(),momV0Y(),momV0Z(),m2) );
}

inline TVector3 StXiI::momBachelorXi() {
  return momXiFrame(M_XI_MINUS,M_PION_MINUS,bachelor);
}

inline TVector3 StXiI::momV0Xi() {
  return momXiFrame(M_XI_MINUS,M_LAMBDA,v0);
}

inline TVector3 StXiI::momBachelorOmega() {
  return momXiFrame(M_OMEGA_MINUS,M_KAON_MINUS,bachelor);
}

inline TVector3 StXiI::momV0Omega() {
  return momXiFrame(M_OMEGA_MINUS,M_LAMBDA,v0);
}

inline TVector3 StXiI::momXiFrame(Float_t m1, Float_t m2, StXiDaughter type) {
  return ( (type == bachelor) ?
    StDecayAngle::getShiftedDaughter(momXiX(),momXiY(),momXiZ(),m1,
                   momBachelorX(),momBachelorY(),momBachelorZ(),m2) :
    StDecayAngle::getShiftedDaughter(momXiX(),momXiY(),momXiZ(),m1,
                                     momV0X(),momV0Y(),momV0Z(),m2) );
}

#endif


/***********************************************************************
 * $Id: StXiI.hh,v 3.14 2011/05/27 18:25:32 genevb Exp $
 * $Log: StXiI.hh,v $
 * Revision 3.14  2011/05/27 18:25:32  genevb
 * Propagate StTrack::key => Int_t to other codes
 *
 * Revision 3.13  2008/07/10 16:16:55  genevb
 * Allow for marking of bad tracks -> bad secondary vertices
 *
 * Revision 3.12  2003/10/26 06:06:01  genevb
 * Added checks for sqrt of neg. numbers
 *
 * Revision 3.11  2003/09/07 03:49:05  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 3.10  2003/09/02 17:59:04  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 3.9  2003/08/26 22:36:28  genevb
 * Calculate Xi momenta at/near primary vertex
 *
 * Revision 3.8  2003/05/30 21:20:20  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
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
 * Revision 3.3  2001/11/05 23:41:07  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.2  2001/08/23 13:20:59  genevb
 * Many bug workarounds...
 *
 * Revision 3.1  2001/05/04 20:15:15  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 ***********************************************************************/
