/***********************************************************************
 *
 * $Id: StV0MuDst.cc,v 1.3 2000/03/31 03:20:24 jones Exp $
 *
 * Authors: Gene Van Buren, UCLA, 24-Mar-2000
 *          Peter G. Jones, University of Birmingham, 04-Jun-1999
 *
 ***********************************************************************
 *
 * Description: V0 micro dst class
 *
 ***********************************************************************
 *
 * $Log: StV0MuDst.cc,v $
 * Revision 1.3  2000/03/31 03:20:24  jones
 * Added topology map to V0/Xi; access funcs for each data member
 *
 * Revision 1.2  2000/03/29 20:52:13  genevb
 * Added StKinkMuDst, replaced arrays
 *
 * Revision 1.1  2000/03/29 03:10:08  genevb
 * Introduction of Strangeness Micro DST package
 *
 *
 ***********************************************************************/
#include "phys_constants.h"
#include "StV0MuDst.hh"
#include "StTrack.h"
#include "StTrackFitTraits.h"
#include "StV0Vertex.h"
#include "StStrangeEvMuDst.hh"
ClassImp(StV0MuDst)

StV0MuDst::StV0MuDst() { 
}

void StV0MuDst::Fill(StV0Vertex* v0Vertex,
                       StStrangeEvMuDst* event) {
  mEvent = event;
  
  mDecayVertexV0X = v0Vertex->position().x();
  mDecayVertexV0Y = v0Vertex->position().y();
  mDecayVertexV0Z = v0Vertex->position().z();
  mDcaV0Daughters = v0Vertex->dcaDaughters();
  mDcaV0ToPrimVertex = v0Vertex->dcaParentToPrimaryVertex();
  mDcaPosToPrimVertex = v0Vertex->dcaDaughterToPrimaryVertex(positive);
  mDcaNegToPrimVertex = v0Vertex->dcaDaughterToPrimaryVertex(negative);
  mMomNegX = v0Vertex->momentumOfDaughter(negative).x();
  mMomNegY = v0Vertex->momentumOfDaughter(negative).y();
  mMomNegZ = v0Vertex->momentumOfDaughter(negative).z();
  mMomPosX = v0Vertex->momentumOfDaughter(positive).x();
  mMomPosY = v0Vertex->momentumOfDaughter(positive).y();
  mMomPosZ = v0Vertex->momentumOfDaughter(positive).z();

  mKeyPos = v0Vertex->daughter(positive)->key();
  mKeyNeg = v0Vertex->daughter(negative)->key();

  mTopologyMapPos = v0Vertex->daughter(positive)->topologyMap();
  mTopologyMapNeg = v0Vertex->daughter(negative)->topologyMap();
}

void StV0MuDst::Clear() {
  mEvent = 0;
}

StV0MuDst::~StV0MuDst() {
}

Float_t StV0MuDst::decayLengthV0() const {
     if (mEvent)
       return sqrt(pow(mDecayVertexV0X - mEvent->primaryVertexX(),2) +
                   pow(mDecayVertexV0Y - mEvent->primaryVertexY(),2) +
                   pow(mDecayVertexV0Z - mEvent->primaryVertexZ(),2));
     return 0.;
}

Float_t StV0MuDst::Ptot2Pos() {
     return (mMomPosX*mMomPosX +
	     mMomPosY*mMomPosY +
	     mMomPosZ*mMomPosZ);
}

Float_t StV0MuDst::Ptot2Neg() {
     return (mMomNegX*mMomNegX +
             mMomNegY*mMomNegY +
             mMomNegZ*mMomNegZ);
}

Float_t StV0MuDst::Pt2V0() {
     Float_t mMomV0X = momV0X();
     Float_t mMomV0Y = momV0Y();
     return (mMomV0X*mMomV0X + mMomV0Y*mMomV0Y);
}

Float_t StV0MuDst::Ptot2V0() {
     Float_t mMomV0Z = momV0Z();
     return (Pt2V0() + mMomV0Z*mMomV0Z);
}

Float_t StV0MuDst::MomPosAlongV0() {
     Float_t mPtot2V0 = Ptot2V0();
     if (mPtot2V0)
       return (mMomPosX*momV0X() + 
               mMomPosY*momV0Y() +
               mMomPosZ*momV0Z()) / sqrt(mPtot2V0);
     return 0.;
}

Float_t StV0MuDst::MomNegAlongV0() {
     Float_t mPtot2V0 = Ptot2V0();
     if (mPtot2V0)
       return (mMomNegX*momV0X() + 
               mMomNegY*momV0Y() +
               mMomNegZ*momV0Z()) / sqrt(mPtot2V0);
     return 0.;
}

Float_t StV0MuDst::alphaV0() {
  Float_t mMomPosAlongV0 = MomPosAlongV0();
  Float_t mMomNegAlongV0 = MomNegAlongV0();
  return (mMomPosAlongV0-mMomNegAlongV0)/
         (mMomPosAlongV0+mMomNegAlongV0);
}

Float_t StV0MuDst::ptArmV0() {
  Float_t mMomPosAlongV0 = MomPosAlongV0();
  return sqrt(Ptot2Pos() - mMomPosAlongV0*mMomPosAlongV0);
}

Float_t StV0MuDst::eLambda() {
  return sqrt(Ptot2V0()+M_LAMBDA*M_LAMBDA);
}

Float_t StV0MuDst::eK0Short() {
  return sqrt(Ptot2V0()+M_KAON_0_SHORT*M_KAON_0_SHORT);
}

Float_t StV0MuDst::ePosProton() {
  return sqrt(Ptot2Pos()+M_PROTON*M_PROTON);
}

Float_t StV0MuDst::eNegProton() {
  return sqrt(Ptot2Neg()+M_ANTIPROTON*M_ANTIPROTON);
}

Float_t StV0MuDst::ePosPion() {
  return sqrt(Ptot2Pos()+M_PION_PLUS*M_PION_PLUS);
}

Float_t StV0MuDst::eNegPion() {
  return sqrt(Ptot2Neg()+M_PION_MINUS*M_PION_MINUS);
}

Float_t StV0MuDst::massLambda() {
  return sqrt(pow(ePosProton()+eNegPion(),2)-Ptot2V0());
}

Float_t StV0MuDst::massAntiLambda() {
  return sqrt(pow(eNegProton()+ePosPion(),2)-Ptot2V0());
}

Float_t StV0MuDst::massK0Short() {
  return sqrt(pow(ePosPion()+eNegPion(),2)-Ptot2V0());
}

Float_t StV0MuDst::rapLambda() {
  Float_t ela = eLambda();
  Float_t mMomV0Z = momV0Z();
  return 0.5*log((ela+mMomV0Z)/(ela-mMomV0Z));
}

Float_t StV0MuDst::rapK0Short() {
  Float_t ek0 = eK0Short();
  Float_t mMomV0Z = momV0Z();
  return 0.5*log((ek0+mMomV0Z)/(ek0-mMomV0Z));
}

Float_t StV0MuDst::cTauLambda() {
  return massLambda()*decayLengthV0()/sqrt(Ptot2V0());
}

Float_t StV0MuDst::cTauK0Short() {
  return massK0Short()*decayLengthV0()/sqrt(Ptot2V0());
}

Float_t StV0MuDst::ptPos() {
  return sqrt(Ptot2Pos()-mMomPosZ*mMomPosZ);
}

Float_t StV0MuDst::ptotPos() {
  return sqrt(Ptot2Pos());
}

Float_t StV0MuDst::ptNeg() {
  return sqrt(Ptot2Neg()-mMomNegZ*mMomNegZ);
}

Float_t StV0MuDst::ptotNeg() {
  return sqrt(Ptot2Neg());
}

Float_t StV0MuDst::ptV0() {
  return sqrt(Pt2V0());
}

Float_t StV0MuDst::ptotV0() {
  return sqrt(Ptot2V0());
}
