/***********************************************************************
 *
 * $Id: StV0I.hh,v 3.1 2001/05/04 20:15:14 genevb Exp $
 *
 * Author: Gene Van Buren, BNL, 24-Apr-2001
 *
 ***********************************************************************
 *
 * Description: V0 micro dst object interface class
 *              Used for StV0I (reconstructed), StV0Mc (Monte Carlo),
 *              and StXiI (which is used for StXiMuDst and StXiMc).
 *
 ***********************************************************************
 *
 * $Log: StV0I.hh,v $
 * Revision 3.1  2001/05/04 20:15:14  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 *
 ***********************************************************************/
#ifndef StV0I_hh
#define StV0I_hh
#include "phys_constants.h"
#include <math.h>
#include "StStrangeEvMuDst.hh"

#ifndef StTrackTopologyMap_hh
#include "StEvent/StTrackTopologyMap.h"
#endif

class StStrangeEvMuDst;
R__EXTERN StTrackTopologyMap* gFakeTopoPtr;

class StV0I {
public:
  StV0I() {}
  virtual ~StV0I() {}
  virtual void Clear();

// ************************************************************************
// The following functions are useful in all V0 types
// ************************************************************************

  // Pointer to event information
  virtual StStrangeEvMuDst* event();
  // Set pointer to event information
  virtual void SetEvent(StStrangeEvMuDst*);
                                                                                  // Chi square of V0 (used only by MuDst)
  virtual Float_t decayLengthV0() const;         // 3-d decay distance
  virtual Float_t decayVertexV0X() const=0;      // Coordinates of decay vertex
  virtual Float_t decayVertexV0Y() const=0;
  virtual Float_t decayVertexV0Z() const=0;

  virtual Float_t momPosX() const=0;             // Momentum components of pos. daughter
  virtual Float_t momPosY() const=0;
  virtual Float_t momPosZ() const=0;
  virtual Float_t momNegX() const=0;             // Momentum components of neg. daughter
  virtual Float_t momNegY() const=0;
  virtual Float_t momNegZ() const=0;
  virtual Float_t momV0X() const=0;         // Momentum components of V0
  virtual Float_t momV0Y() const=0;
  virtual Float_t momV0Z() const=0;
  virtual Float_t alphaV0();              // Armenteros-Podolanski variable
  virtual Float_t ptArmV0();              // Armenteros-Podolanski variable
  virtual Float_t eLambda();              // Energy assuming lambda hypothesis
  virtual Float_t eK0Short();             // Energy assuming k-short hypothesis
  virtual Float_t ePosProton();           // Energy of pos. daughter assuming proton
  virtual Float_t ePosPion();             // Energy of pos. daughter assuming pion
  virtual Float_t eNegProton();           // Energy of neg. daughter assuming antiproton
  virtual Float_t eNegPion();             // Energy of neg. daughter assuming pion
  virtual Float_t massLambda();           // Mass assuming lambda hypothesis
  virtual Float_t massAntiLambda();       // Mass assuming antilambda hypothesis
  virtual Float_t massK0Short();          // Mass assuming k-short hypothesis
  virtual Float_t rapLambda();            // Rapidity assuming (anti)lambda
  virtual Float_t rapK0Short();           // Rapidity assuming k-short
  virtual Float_t cTauLambda();           // Lifetime (ctau) assuming (anti)lambda
  virtual Float_t cTauK0Short();          // Lifetime (ctau) assuming k-short
  virtual Float_t ptV0();                 // Transverse momentum
  virtual Float_t ptotV0();               // Total momentum
  virtual Float_t ptPos();                // Transverse momentum of pos. daughter
  virtual Float_t ptotPos();              // Total momentum of pos. daughter
  virtual Float_t ptNeg();                // Transverse momentum of neg. daughter
  virtual Float_t ptotNeg();              // Total momentum of neg. daughter  

// ************************************************************************
// The next few functions are presently used only by MC
// ************************************************************************

  // Index of decay mode (see tables in MC header files)
  virtual Int_t decayMode() const {return -1;}
  virtual Int_t geantIdParent() const   {return -1;}
  virtual Int_t geantIdPositive() const {return -1;}
  virtual Int_t geantIdNegative() const {return -1;}
  virtual Int_t positiveSimTpcHits() const    {return -1;}
  virtual Int_t positiveCommonTpcHits() const {return -1;}
  virtual Int_t negativeSimTpcHits() const    {return -1;}
  virtual Int_t negativeCommonTpcHits() const {return -1;}

// ************************************************************************
// All of the functions from this point on are presently used only by MuDst
// ************************************************************************

  // DCA of v0 daughters at decay vertex
  virtual Float_t dcaV0Daughters() const {return 0;}
  // DCA of v0 to primary vertex
  virtual Float_t dcaV0ToPrimVertex()  const {return -1;}
  // DCA of pos v0 daughter to pri vertex
  virtual Float_t dcaPosToPrimVertex() const {return -1;}
  // DCA of neg v0 daughter to pri vertex
  virtual Float_t dcaNegToPrimVertex() const {return -1;}

  // Pos. daughter track topology map
  virtual StTrackTopologyMap& topologyMapPos() {return (*gFakeTopoPtr);}
  // Neg. daughter track topology map
  virtual StTrackTopologyMap& topologyMapNeg() {return (*gFakeTopoPtr);}
  virtual UShort_t keyPos() const {return 0;}    // Track id v0 daughters
  virtual UShort_t keyNeg() const {return 0;}    // Track id v0 daughters

  virtual Float_t chi2V0()  const {return 0;}
  // Confidence level of V0
  virtual Float_t clV0()    const {return 0;}
  // Chi square of pos. daughter
  virtual Float_t chi2Pos() const {return 0;}
  // Confidence level of pos. daughter
  virtual Float_t clPos()   const {return 0;}
  // Chi square of neg. daughter
  virtual Float_t chi2Neg() const {return 0;}
  // Confidence level of neg. daughter
  virtual Float_t clNeg()   const {return 0;}
  // Detector ID for V0 Vertex
  virtual Long_t  detectorIdV0() {return 0;}
  // Detector ID for pars used in V0 finder
  virtual Long_t detectorIdPars() {return 0;}
  // dE/dX of pos. daughter
  virtual Float_t dedxPos() const {return 0;}
  // dE/dX of neg. daughter
  virtual Float_t dedxNeg() const {return 0;}
  // Number of dE/dX points for pos. daughter
  virtual UShort_t numDedxPos() const {return 0;}
  // Number of dE/dX points for neg. daughter
  virtual UShort_t numDedxNeg() const {return 0;}

protected:

  virtual Float_t Ptot2Pos();          
  virtual Float_t Ptot2Neg();             
  virtual Float_t Ptot2V0();            
  virtual Float_t Pt2V0();  
  virtual Float_t MomPosAlongV0();
  virtual Float_t MomNegAlongV0();

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
       return sqrt(pow(decayVertexV0X() - mEvent->primaryVertexX(),2) +
                   pow(decayVertexV0Y() - mEvent->primaryVertexY(),2) +
                   pow(decayVertexV0Z() - mEvent->primaryVertexZ(),2));
     return 0.;
}

inline Float_t StV0I::Ptot2Pos() {
     return (pow(momPosX(),2) + pow(momPosY(),2) + pow(momPosZ(),2));
}

inline Float_t StV0I::Ptot2Neg() {
     return (pow(momNegX(),2) + pow(momNegY(),2) + pow(momNegZ(),2));
}

inline Float_t StV0I::Pt2V0() {
     return (pow(momV0X(),2) + pow(momV0Y(),2));
}

inline Float_t StV0I::Ptot2V0() {
     return (Pt2V0() + pow(momV0Z(),2));
}

inline Float_t StV0I::MomPosAlongV0() {
     Float_t mPtot2V0 = Ptot2V0();
     if (mPtot2V0)
       return (momPosX()*momV0X() + 
               momPosY()*momV0Y() +
               momPosZ()*momV0Z()) / sqrt(mPtot2V0);
     return 0.;
}

inline Float_t StV0I::MomNegAlongV0() {
     Float_t mPtot2V0 = Ptot2V0();
     if (mPtot2V0)
       return (momNegX()*momV0X() + 
               momNegY()*momV0Y() +
               momNegZ()*momV0Z()) / sqrt(mPtot2V0);
     return 0.;
}

inline Float_t StV0I::alphaV0() {
  Float_t mMomPosAlongV0 = MomPosAlongV0();
  Float_t mMomNegAlongV0 = MomNegAlongV0();
  return (mMomPosAlongV0-mMomNegAlongV0)/
         (mMomPosAlongV0+mMomNegAlongV0);
}

inline Float_t StV0I::ptArmV0() {
  return sqrt(Ptot2Pos() - pow(MomPosAlongV0(),2));
}

inline Float_t StV0I::eLambda() {
  return sqrt(Ptot2V0()+pow(M_LAMBDA,2));
}

inline Float_t StV0I::eK0Short() {
  return sqrt(Ptot2V0()+pow(M_KAON_0_SHORT,2));
}

inline Float_t StV0I::ePosProton() {
  return sqrt(Ptot2Pos()+pow(M_PROTON,2));
}

inline Float_t StV0I::eNegProton() {
  return sqrt(Ptot2Neg()+pow(M_ANTIPROTON,2));
}

inline Float_t StV0I::ePosPion() {
  return sqrt(Ptot2Pos()+pow(M_PION_PLUS,2));
}

inline Float_t StV0I::eNegPion() {
  return sqrt(Ptot2Neg()+pow(M_PION_MINUS,2));
}

inline Float_t StV0I::massLambda() {
  return sqrt(pow(ePosProton()+eNegPion(),2)-Ptot2V0());
}

inline Float_t StV0I::massAntiLambda() {
  return sqrt(pow(eNegProton()+ePosPion(),2)-Ptot2V0());
}

inline Float_t StV0I::massK0Short() {
  return sqrt(pow(ePosPion()+eNegPion(),2)-Ptot2V0());
}

inline Float_t StV0I::rapLambda() {
  Float_t ela = eLambda();
  Float_t mMomV0Z = momV0Z();
  return 0.5*log((ela+mMomV0Z)/(ela-mMomV0Z));
}

inline Float_t StV0I::rapK0Short() {
  Float_t ek0 = eK0Short();
  Float_t mMomV0Z = momV0Z();
  return 0.5*log((ek0+mMomV0Z)/(ek0-mMomV0Z));
}

inline Float_t StV0I::cTauLambda() {
  return massLambda()*decayLengthV0()/sqrt(Ptot2V0());
}

inline Float_t StV0I::cTauK0Short() {
  return massK0Short()*decayLengthV0()/sqrt(Ptot2V0());
}

inline Float_t StV0I::ptPos() {
  return sqrt(Ptot2Pos()-pow(momPosZ(),2));
}

inline Float_t StV0I::ptotPos() {
  return sqrt(Ptot2Pos());
}

inline Float_t StV0I::ptNeg() {
  return sqrt(Ptot2Neg()-pow(momNegZ(),2));
}

inline Float_t StV0I::ptotNeg() {
  return sqrt(Ptot2Neg());
}

inline Float_t StV0I::ptV0() {
  return sqrt(Pt2V0());
}

inline Float_t StV0I::ptotV0() {
  return sqrt(Ptot2V0());
}

inline void StV0I::Clear() {
  mEvent = 0;
}

#endif
