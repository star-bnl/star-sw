/***********************************************************************
 *
 * $Id: StXiMuDst.cc,v 1.2 2000/03/29 20:52:14 genevb Exp $
 *
 * Authors: Gene Van Buren, UCLA, 24-Mar-2000
 *          Peter G. Jones, University of Birmingham, 30-Mar-1999
 *
 ***********************************************************************
 *
 * Description: Xi (cascade) micro dst class
 *
 ***********************************************************************
 *
 * $Log: StXiMuDst.cc,v $
 * Revision 1.2  2000/03/29 20:52:14  genevb
 * Added StKinkMuDst, replaced arrays
 *
 * Revision 1.1  2000/03/29 03:10:08  genevb
 * Introduction of Strangeness Micro DST package
 *
 *
 ***********************************************************************/
#include "phys_constants.h"
#include "StXiMuDst.hh"
#include "StXiVertex.h"
#include "StTrack.h"
#include "StTrackFitTraits.h"
#include "StStrangeEvMuDst.hh"

ClassImp(StXiMuDst)

StXiMuDst::StXiMuDst() { 
}

void StXiMuDst::Fill(StXiVertex* xiVertex, 
		       StV0Vertex* v0Vertex,
		       StStrangeEvMuDst* event) {
  StV0MuDst::Fill(v0Vertex, event);
  FillXi(xiVertex);
}

void StXiMuDst::FillXi(StXiVertex* xiVertex) {

  mCharge = (Int_t) (xiVertex->chargeOfBachelor());
  mDecayVertexXiX = xiVertex->position().x();
  mDecayVertexXiY = xiVertex->position().y();
  mDecayVertexXiZ = xiVertex->position().z();
  mDcaXiDaughters = xiVertex->dcaDaughters();
  mDcaBachelorToPrimVertex = xiVertex->dcaBachelorToPrimaryVertex();
  mDcaXiToPrimVertex = xiVertex->dcaParentToPrimaryVertex();
  mMomBachelorX = xiVertex->momentumOfBachelor().x();
  mMomBachelorY = xiVertex->momentumOfBachelor().y();
  mMomBachelorZ = xiVertex->momentumOfBachelor().z();

  mTpcHitsBachelor = 
    xiVertex->bachelor()->fitTraits().numberOfFitPoints(kTpcId);
}

void StXiMuDst::Clear() {
  StV0MuDst::Clear();
}

StXiMuDst::~StXiMuDst() {
}

Float_t StXiMuDst::decayVertexXi(Int_t n) {
  switch (n) {
    case (2): return mDecayVertexXiZ;
    case (1): return mDecayVertexXiY;
    default : return mDecayVertexXiX;
  }
}

Float_t StXiMuDst::momBachelor(Int_t n) {
  switch (n) {
    case (2): return mMomBachelorZ;
    case (1): return mMomBachelorY;
    default : return mMomBachelorX;
  }
}

Float_t StXiMuDst::decayLengthXi() {
  return sqrt(pow(mDecayVertexXiX - mEvent->primaryVertex(0),2) +
              pow(mDecayVertexXiY - mEvent->primaryVertex(1),2) +
              pow(mDecayVertexXiZ - mEvent->primaryVertex(2),2));
}

Float_t StXiMuDst::Ptot2Bachelor () {
  return (mMomBachelorX*mMomBachelorX +
          mMomBachelorY*mMomBachelorY + 
          mMomBachelorZ*mMomBachelorZ);
}

Float_t StXiMuDst::momXi(Int_t n) {
     return (momBachelor(n) + momV0(n));
}

Float_t StXiMuDst::Pt2Xi() {
     Float_t mMomXi_0 = momXi(0);
     Float_t mMomXi_1 = momXi(1);
     return (mMomXi_0*mMomXi_0 + mMomXi_1*mMomXi_1);
}

Float_t StXiMuDst::Ptot2Xi() {
     Float_t mMomXi_2 = momXi(2);
     return (Pt2Xi() + mMomXi_2*mMomXi_2);
}

Float_t StXiMuDst::MomBachelorAlongXi() {
  Float_t mPtot2Xi = Ptot2Xi();
  if (mPtot2Xi)
    return (mMomBachelorX*momXi(0) + 
            mMomBachelorY*momXi(1) +
            mMomBachelorZ*momXi(2)) / sqrt(mPtot2Xi);
  return 0.;
}

Float_t StXiMuDst::MomV0AlongXi() {
  Float_t mPtot2Xi = Ptot2Xi();
  if (mPtot2Xi)
    return (momV0(0)*momXi(0) + 
            momV0(1)*momXi(1) + 
            momV0(2)*momXi(2)) / sqrt(mPtot2Xi);
  return 0.;
}

Float_t StXiMuDst::alphaXi() {
  Float_t mMomBachelorAlongXi = MomBachelorAlongXi();
  Float_t mMomV0AlongXi = MomV0AlongXi();
  switch (mCharge) {
  case 1:
    return (mMomBachelorAlongXi-mMomV0AlongXi)/
           (mMomBachelorAlongXi+mMomV0AlongXi);
  case -1:
    return (mMomV0AlongXi-mMomBachelorAlongXi)/
           (mMomV0AlongXi+mMomBachelorAlongXi);
  default:
    return 0.;
  }
}

Float_t StXiMuDst::ptArmXi() {
  Float_t mMomV0AlongXi = MomV0AlongXi();
  return sqrt(Ptot2V0() - mMomV0AlongXi*mMomV0AlongXi);
}

Float_t StXiMuDst::eXi() {
  return sqrt(Ptot2Xi()+M_XI_MINUS*M_XI_MINUS);
}

Float_t StXiMuDst::eOmega() {
  return sqrt(Ptot2Xi()+M_OMEGA_MINUS*M_OMEGA_MINUS);
}

Float_t StXiMuDst::eBachelorPion() {
  return sqrt(Ptot2Bachelor()+M_PION_MINUS*M_PION_MINUS);
}

Float_t StXiMuDst::eBachelorKaon() {
  return sqrt(Ptot2Bachelor()+M_KAON_MINUS*M_KAON_MINUS);
}

Float_t StXiMuDst::massOmega() {
  return sqrt(pow(eLambda()+eBachelorKaon(),2)-Ptot2Xi());
}

Float_t StXiMuDst::massXi() {
  return sqrt(pow(eLambda()+eBachelorPion(),2)-Ptot2Xi());
}

Float_t StXiMuDst::rapXi() {
  Float_t mMomXi_2 = momXi(2);
  Float_t exi = eXi();
  return 0.5*log((exi+mMomXi_2)/(exi-mMomXi_2));
}

Float_t StXiMuDst::rapOmega() {
  Float_t mMomXi_2 = momXi(2);
  Float_t eom = eOmega();
  return 0.5*log((eom+mMomXi_2)/(eom-mMomXi_2));
}

Float_t StXiMuDst::cTauOmega() {
  return massOmega()*decayLengthXi()/sqrt(Ptot2Xi());
}

Float_t StXiMuDst::cTauXi() {
  return massXi()*decayLengthXi()/sqrt(Ptot2Xi());
}

Float_t StXiMuDst::ptBachelor() {
  return sqrt(Ptot2Bachelor()-mMomBachelorZ*mMomBachelorZ);
}

Float_t StXiMuDst::ptotBachelor() {
  return sqrt(Ptot2Bachelor());
}

Float_t StXiMuDst::ptXi() {
  return sqrt(Pt2Xi());
}

Float_t StXiMuDst::ptotXi() {
  return sqrt(Ptot2Xi());
}

