/***********************************************************************
 *
 * $Id: StXiMuDst.cc,v 2.0 2000/06/02 22:11:55 genevb Exp $
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
 * Revision 2.0  2000/06/02 22:11:55  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.3  2000/03/31 03:20:24  jones
 * Added topology map to V0/Xi; access funcs for each data member
 *
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
  mChi2Xi = xiVertex->chiSquared();
  mClXi = xiVertex->probChiSquared();

  StTrack* trk = xiVertex->bachelor();
  mKeyBachelor  = trk->key();
  mTopologyMapBachelor = trk->topologyMap();
  mChi2Bachelor = trk->fitTraits().chi2(0);
  mClBachelor = trk->fitTraits().chi2(1);
}

void StXiMuDst::Clear() {
  StV0MuDst::Clear();
}

StXiMuDst::~StXiMuDst() {
}

Float_t StXiMuDst::decayLengthV0() const {
  return sqrt(pow(mDecayVertexV0X - mDecayVertexXiX,2) +
              pow(mDecayVertexV0Y - mDecayVertexXiY,2) +
              pow(mDecayVertexV0Z - mDecayVertexXiZ,2));
}

Float_t StXiMuDst::decayLengthXi() const {
  return sqrt(pow(mDecayVertexXiX - mEvent->primaryVertexX(),2) +
              pow(mDecayVertexXiY - mEvent->primaryVertexY(),2) +
              pow(mDecayVertexXiZ - mEvent->primaryVertexZ(),2));
}

Float_t StXiMuDst::Ptot2Bachelor () {
  return (mMomBachelorX*mMomBachelorX +
          mMomBachelorY*mMomBachelorY + 
          mMomBachelorZ*mMomBachelorZ);
}

Float_t StXiMuDst::Pt2Xi() {
     Float_t mMomXiX = momXiX();
     Float_t mMomXiY = momXiY();
     return (mMomXiX*mMomXiX + mMomXiY*mMomXiY);
}

Float_t StXiMuDst::Ptot2Xi() {
     Float_t mMomXiZ = momXiZ();
     return (Pt2Xi() + mMomXiZ*mMomXiZ);
}

Float_t StXiMuDst::MomBachelorAlongXi() {
  Float_t mPtot2Xi = Ptot2Xi();
  if (mPtot2Xi)
    return (mMomBachelorX*momXiX() + 
            mMomBachelorY*momXiY() +
            mMomBachelorZ*momXiZ()) / sqrt(mPtot2Xi);
  return 0.;
}

Float_t StXiMuDst::MomV0AlongXi() {
  Float_t mPtot2Xi = Ptot2Xi();
  if (mPtot2Xi)
    return (momV0X()*momXiX() + 
            momV0Y()*momXiY() + 
            momV0Z()*momXiZ()) / sqrt(mPtot2Xi);
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
  Float_t mMomXiZ = momXiZ();
  Float_t exi = eXi();
  return 0.5*log((exi+mMomXiZ)/(exi-mMomXiZ));
}

Float_t StXiMuDst::rapOmega() {
  Float_t mMomXiZ = momXiZ();
  Float_t eom = eOmega();
  return 0.5*log((eom+mMomXiZ)/(eom-mMomXiZ));
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

Long_t StXiMuDst::detectorIdXi() {
  return ((100*detectorIdV0())+
               detectorIdTrack(mTopologyMapBachelor));
}

Long_t StXiMuDst::detectorIdPars() {
  return 1;  // Currently, only one set of parameters actually used
}
