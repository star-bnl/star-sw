/***********************************************************************
 *
 * $Id: StXiI.hh,v 3.6 2002/04/30 16:02:48 genevb Exp $
 *
 * Author: Gene Van Buren, BNL, 24-Apr-2001
 *
 ***********************************************************************
 *
 * Description: Xi (cascade) micro dst object interface class
 *              Used for StXiMuDst (reconstructed), StXiMc (Monte Carlo)
 *
 ***********************************************************************
 *
 * $Log: StXiI.hh,v $
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
 *
 ***********************************************************************/
#ifndef StXiI_hh
#define StXiI_hh

#include "StV0I.hh"
static const Float_t M_OMEGA_2 = pow(M_OMEGA_MINUS,2);
static const Float_t M_XI_2 = pow(M_XI_MINUS,2);
static const Float_t M_KAON_MINUS_2 = pow(M_KAON_MINUS,2);

enum StXiDaughter {bachelor, v0};


class StXiI : public virtual StV0I {
public:
  StXiI();
  virtual ~StXiI() {}

// ************************************************************************
// The following functions are useful in all Xi types
// ************************************************************************

  virtual Int_t   charge() const=0;         // Particle charge
  virtual Float_t decayLengthV0() const;    // 3-d decay distance
  virtual Float_t decayLengthXi() const;    // 3-d decay distance
  virtual Float_t decayVertexXiX() const=0; // Coordinate of decay vertex
  virtual Float_t decayVertexXiY() const=0;
  virtual Float_t decayVertexXiZ() const=0;

  virtual TVector3 momBachelor();           // Momentum of bachelor
  virtual Float_t momBachelorX() const=0;   // Momentum components of bachelor
  virtual Float_t momBachelorY() const=0;
  virtual Float_t momBachelorZ() const=0;
  virtual TVector3 momXi();                 // Momentum of Xi/Omega
  virtual Float_t momXiX() const=0;         // Momentum components of Xi/Omega
  virtual Float_t momXiY() const=0;
  virtual Float_t momXiZ() const=0;
  virtual Float_t alphaXi();                // Armenteros-Podolanski variable
  virtual Float_t ptArmXi();                // Armenteros-Podolanski variable

  virtual Float_t eOmega();                 // Energy assuming Omega hypothesis
  virtual Float_t eXi();                    // Energy assuming Xi hypothesis
  virtual Float_t eBachelorPion();          // Energy of bachelor assuming pion
  virtual Float_t eBachelorKaon();          // Energy of bachelor assuming kaon
  virtual Float_t massOmega();              // Mass assuming Omega hypothesis
  virtual Float_t massXi();                 // Mass assuming Xi hypothesis
  virtual Float_t rapOmega();               // Rapidity assuming (anti)Omega
  virtual Float_t rapXi();                  // Rapidity assuming (anti)Xi
  virtual Float_t cTauOmega();              // Lifetime (ctau) assuming (anti)Omega
  virtual Float_t cTauXi();                 // Lifetime (ctau) assuming (anti)Xi

  virtual Float_t ptXi();                   // Transverse momentum of Xi/Omega
  virtual Float_t ptBachelor();             // Transverse momentum of bachelor
  virtual Float_t ptotXi();                 // Total momentum of Xi/Omega
  virtual Float_t ptotBachelor();           // Total momentum of bachelor
  virtual Float_t thetaXi();                // Polar angle theta of Xi/Omega
  virtual Float_t thetaBachelor();          // Polar angle theta of bachelor
  virtual Float_t pseudoRapXi();            // Pseudorapidity of Xi/Omega
  virtual Float_t pseudoRapBachelor();      // Pseudorapidity of bachelor

  virtual Float_t mtOmega();                // Transverse mass assuming (anti)Omega
  virtual Float_t mtXi();                   // Transverse mass assuming (anti)Xi
  virtual Float_t mtm0Omega();              // mt-m0 assuming (anti)Omega
  virtual Float_t mtm0Xi();                 // mt-m0 assuming (anti)Xi

  // Energy and mass formulae for hypothetical masses
  virtual Float_t eBachelorHyp(Float_t mass);  // Energy of bachelor
  virtual Float_t massHypXi(Float_t massV0, Float_t massBachelor);
  virtual Float_t eHypXi(Float_t mass);     // Energy of Xi assuming its mass
  virtual Float_t rapHypXi(Float_t mass);
  virtual Float_t mtHypXi(Float_t mass);
  virtual Float_t mtm0HypXi(Float_t mass);

  // Cosines of decay and polarization angles for different particle hypotheses
  virtual Float_t decayCosThetaBachelorXi();    // xi    - bachelor
  virtual Float_t decayCosThetaV0Xi();          // xi    - V0 
  virtual Float_t decayCosThetaBachelorOmega(); // omega - bachelor
  virtual Float_t decayCosThetaV0Omega();       // omega - V0
  // This helper function can be used for decayCosTheta of any hypothesis:
  // m1 = parent mass, m2 = daughter mass, type = bachelor/v0 daughter
  virtual Float_t dCTXi(Float_t m1, Float_t m2, StXiDaughter type);

  // Momenta of daughters in Xi rest frame for different particle hypotheses
  virtual TVector3 momBachelorXi();             // xi    - bachelor
  virtual TVector3 momV0Xi();                   // xi    - V0
  virtual TVector3 momBachelorOmega();          // omega - bachelor
  virtual TVector3 momV0Omega();                // omega - V0
  // This helper function can be used for momentum of daughter in
  // Xi rest frame of any hypothesis:
  // m1 = parent mass, m2 = daughter mass, charge = positive/negative daughter
  virtual TVector3 momXiFrame(Float_t m1, Float_t m2, StXiDaughter type);

// ************************************************************************
// The next few functions are presently used only by MC
// ************************************************************************
                                                                                  // Index of V0 (used only by MC)
  virtual Int_t V0Index() {return -1;}
  
// ************************************************************************
// All of the functions from this point on are presently used only by MuDst
// ************************************************************************

  // DCA of xi daughters at decay vertex
  virtual Float_t dcaXiDaughters() {return 0;}
  // DCA of bachelor to primary vertex
  virtual Float_t dcaBachelorToPrimVertex() {return -1;}
  // DCA of xi to primary vertex
  virtual Float_t dcaXiToPrimVertex() {return -1;}

  // Bachelor track key
  virtual UShort_t keyBachelor() const {return 0;}
  // Bachelor track topology map
  virtual StTrackTopologyMap& topologyMapBachelor() {return (*gFakeTopoPtr);}

  // Chi square of Xi (used only by MuDst)
  virtual Float_t chi2Xi() const {return 0;}
  // Confidence level of Xi
  virtual Float_t clXi()   const {return 0;}
  // Chi square of bachelor
  virtual Float_t chi2Bachelor() const {return 0;}
  // Confidence level of bachelor
  virtual Float_t clBachelor()   const {return 0;}
  // Detector ID for Xi vertex
  virtual Long_t  detectorIdXi() {return 0;}
  // Detector ID for pars used in Xi finder
  virtual Long_t detectorIdPars() {return 0;}
  // dE/dX of bachelor
  virtual Float_t dedxBachelor() const {return 0;}
  // Error on mean of dE/dX of bachelor
  virtual Float_t errDedxBachelor() const {return 0;}
  // Number of dE/dX points for bachelor
  virtual UShort_t numDedxBachelor() const {return 0;}
  // Length of dE/dX track for bachelor
  virtual Float_t lenDedxBachelor() const {return 0;}

protected:
  virtual Float_t Ptot2Bachelor();
  virtual Float_t Ptot2Xi();
  virtual Float_t Pt2Xi();
  virtual Float_t MomBachelorAlongXi();
  virtual Float_t MomV0AlongXi();
};

inline StXiI::StXiI() : StV0I() {}

inline Float_t StXiI::decayLengthV0() const {
  return sqrt(pow(decayVertexV0X() - decayVertexXiX(),2) +
              pow(decayVertexV0Y() - decayVertexXiY(),2) +
              pow(decayVertexV0Z() - decayVertexXiZ(),2));
}

inline Float_t StXiI::decayLengthXi() const {
  return sqrt(pow(decayVertexXiX() - mEvent->primaryVertexX(),2) +
              pow(decayVertexXiY() - mEvent->primaryVertexY(),2) +
              pow(decayVertexXiZ() - mEvent->primaryVertexZ(),2));
}

inline TVector3 StXiI::momBachelor() {
     return TVector3(momBachelorX(), momBachelorY(), momBachelorZ());
}

inline TVector3 StXiI::momXi() {
     return TVector3(momXiX(), momXiY(), momXiZ());
}

inline Float_t StXiI::alphaXi() {
  Float_t mMomBachelorAlongXi = MomBachelorAlongXi();
  Float_t mMomV0AlongXi = MomV0AlongXi();
  return (((Float_t) charge()) * (mMomBachelorAlongXi-mMomV0AlongXi)/
                                 (mMomBachelorAlongXi+mMomV0AlongXi));
}

inline Float_t StXiI::ptArmXi() {
  return sqrt(Ptot2V0() - pow(MomV0AlongXi(),2));
}

inline Float_t StXiI::eOmega() {
  return sqrt(Ptot2Xi()+M_OMEGA_2);
}

inline Float_t StXiI::eXi() {
  return sqrt(Ptot2Xi()+M_XI_2);
}

inline Float_t StXiI::eHypXi(Float_t mass) {
  return sqrt(Ptot2Xi()+pow(mass,2));
}

inline Float_t StXiI::eBachelorHyp(Float_t mass) {
  return sqrt(Ptot2Bachelor()+pow(mass,2));
}

inline Float_t StXiI::eBachelorPion() {
  return sqrt(Ptot2Bachelor()+M_PION_MINUS_2);
}

inline Float_t StXiI::eBachelorKaon() {
  return sqrt(Ptot2Bachelor()+M_KAON_MINUS_2);
}

inline Float_t StXiI::massHypXi(Float_t massV0, Float_t massBachelor) {
  return sqrt(pow(eHypV0(massV0)+eBachelorHyp(massBachelor),2)-Ptot2Xi());
}

inline Float_t StXiI::massOmega() {
  return sqrt(pow(eLambda()+eBachelorKaon(),2)-Ptot2Xi());
}

inline Float_t StXiI::massXi() {
  return sqrt(pow(eLambda()+eBachelorPion(),2)-Ptot2Xi());
}

inline Float_t StXiI::rapHypXi(Float_t mass) {
  Float_t mMomXiZ = momXiZ();
  Float_t eX = eHypXi(mass);
  return 0.5*log((eX+mMomXiZ)/(eX-mMomXiZ));
}

inline Float_t StXiI::rapOmega() {
  Float_t mMomXiZ = momXiZ();
  Float_t eom = eOmega();
  return 0.5*log((eom+mMomXiZ)/(eom-mMomXiZ));
}

inline Float_t StXiI::rapXi() {
  Float_t mMomXiZ = momXiZ();
  Float_t exi = eXi();
  return 0.5*log((exi+mMomXiZ)/(exi-mMomXiZ));
}

inline Float_t StXiI::cTauOmega() {
  return massOmega()*decayLengthXi()/sqrt(Ptot2Xi());
}

inline Float_t StXiI::cTauXi() {
  return massXi()*decayLengthXi()/sqrt(Ptot2Xi());
}

inline Float_t StXiI::ptBachelor() {
  return sqrt(Ptot2Bachelor()-pow(momBachelorZ(),2));
}

inline Float_t StXiI::ptotBachelor() {
  return sqrt(Ptot2Bachelor());
}

inline Float_t StXiI::ptXi() {
  return sqrt(Pt2Xi());
}

inline Float_t StXiI::ptotXi() {
  return sqrt(Ptot2Xi());
}

inline Float_t StXiI::thetaXi() {
  return acos(momXiZ()/ptotXi());
}

inline Float_t StXiI::pseudoRapXi() {
  return (-log(tan(thetaXi()/2.)));
}

inline Float_t StXiI::thetaBachelor() {
  return acos(momBachelorZ()/ptotBachelor());
}

inline Float_t StXiI::pseudoRapBachelor() {
  return (-log(tan(thetaBachelor()/2.)));
}

inline Float_t StXiI::mtHypXi(Float_t mass) {
  return sqrt(Pt2Xi() + pow(mass,2));
}

inline Float_t StXiI::mtOmega() {
  return sqrt(Pt2Xi() + M_OMEGA_2);
}

inline Float_t StXiI::mtXi() {
  return sqrt(Pt2Xi() + M_XI_2);
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
  return (pow(momBachelorX(),2) +
          pow(momBachelorY(),2) + pow(momBachelorZ(),2));
}

inline Float_t StXiI::Pt2Xi() {
  return (pow(momXiX(),2) + pow(momXiY(),2));
}

inline Float_t StXiI::Ptot2Xi() {
  return (Pt2Xi() + pow(momXiZ(),2));
}

inline Float_t StXiI::MomBachelorAlongXi() {
  Float_t mPtot2Xi = Ptot2Xi();
  if (mPtot2Xi)
    return (momBachelorX()*momXiX() + 
            momBachelorY()*momXiY() +
            momBachelorZ()*momXiZ()) / sqrt(mPtot2Xi);
  return 0.;
}

inline Float_t StXiI::MomV0AlongXi() {
  Float_t mPtot2Xi = Ptot2Xi();
  if (mPtot2Xi)
    return (momV0X()*momXiX() + 
            momV0Y()*momXiY() + 
            momV0Z()*momXiZ()) / sqrt(mPtot2Xi);
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
