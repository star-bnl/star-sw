/***********************************************************************
 *
 * $Id: StXiMiniDst.cc,v 1.3 1999/09/24 01:23:32 fisyak Exp $
 *
 * Author: Peter G. Jones, University of Birmingham, 30-Mar-1999
 *
 ***********************************************************************
 *
 * Description: Xi (cascade) mini dst class
 *
 ***********************************************************************
 *
 * $Log: StXiMiniDst.cc,v $
 * Revision 1.3  1999/09/24 01:23:32  fisyak
 * Reduced Include Path
 *
 * Revision 1.2  1999/09/02 09:04:57  jones
 * Added StEvMiniDst class, New file handling, Partially implemented TTrees
 *
 * Revision 1.1  1999/08/13 12:38:17  jones
 * Major revision to merge StV0MiniDstMaker and StXiMiniDstMaker
 *
 *
 ***********************************************************************/
#include "phys_constants.h"
#include "StXiMiniDst.hh"
#include "StXiVertex.h"
#include "SystemOfUnits.h"
ClassImp(StXiMiniDst)

StXiMiniDst::StXiMiniDst() { 
}

StXiMiniDst::StXiMiniDst(StXiVertex* xiVertex, 
			 StV0Vertex* v0Vertex,
			 StEvMiniDst* event) : 
             StV0MiniDst(v0Vertex, event) {

  double B=0.5*tesla; // Hardwired - fix later

  mCharge = xiVertex->chargeOfBachelor(B);
  mDecayVertexXi[0] = xiVertex->position().x();
  mDecayVertexXi[1] = xiVertex->position().y();
  mDecayVertexXi[2] = xiVertex->position().z();
  mDcaXiDaughters = xiVertex->dcaDaughters();
  mDcaBachelorToPrimVertex = xiVertex->dcaBachelorToPrimaryVertex();
  mDcaXiToPrimVertex = xiVertex->dcaParentToPrimaryVertex();
  mMomBachelor[0] = xiVertex->momentumOfBachelor().x();
  mMomBachelor[1] = xiVertex->momentumOfBachelor().y();
  mMomBachelor[2] = xiVertex->momentumOfBachelor().z();

  mTpcHitsBachelor = 
    ((StGlobalTrack *) xiVertex->bachelor())->numberOfTpcHits();

  this->UpdateXi();
}

StXiMiniDst::~StXiMiniDst() {
}

void StXiMiniDst::UpdateXi() { 
  // Calculate derived data members

     mDecayLengthXi = sqrt(pow(mDecayVertexXi[0] - 
			       mEvent->primaryVertex()[0],2) +
			   pow(mDecayVertexXi[1] - 
			       mEvent->primaryVertex()[1],2) +
			   pow(mDecayVertexXi[2] - 
			       mEvent->primaryVertex()[2],2));
     
     mPtot2Bachelor = mMomBachelor[0]*mMomBachelor[0] +
                      mMomBachelor[1]*mMomBachelor[1] + 
                      mMomBachelor[2]*mMomBachelor[2];
  
          mMomXi[0] = mMomV0[0] + mMomBachelor[0];
          mMomXi[1] = mMomV0[1] + mMomBachelor[1];
          mMomXi[2] = mMomV0[2] + mMomBachelor[2];
             mPt2Xi = mMomXi[0]*mMomXi[0] + mMomXi[1]*mMomXi[1];
           mPtot2Xi = mPt2Xi + mMomXi[2]*mMomXi[2];
  
mMomBachelorAlongXi = ( mMomBachelor[0]*mMomXi[0] + 
			mMomBachelor[1]*mMomXi[1] +
			mMomBachelor[2]*mMomXi[2] ) / sqrt(mPtot2Xi); 
      mMomV0AlongXi = ( mMomV0[0]*mMomXi[0] + 
			mMomV0[1]*mMomXi[1] + 
			mMomV0[2]*mMomXi[2] ) / sqrt(mPtot2Xi);
}

float StXiMiniDst::alphaXi() {
  switch (mCharge) {
  case 1:
    return (mMomBachelorAlongXi-mMomV0AlongXi)/
           (mMomBachelorAlongXi+mMomV0AlongXi);
  case -1:
    return (mMomV0AlongXi-mMomBachelorAlongXi)/
           (mMomV0AlongXi+mMomBachelorAlongXi);
  default:
    return (0);
  }
}

float StXiMiniDst::ptArmXi() {
  return sqrt(mPtot2V0 - mMomV0AlongXi*mMomV0AlongXi);
}

float StXiMiniDst::eXi() {
  return sqrt(mPtot2Xi+M_XI_MINUS*M_XI_MINUS);
}

float StXiMiniDst::eOmega() {
  return sqrt(mPtot2Xi+M_OMEGA_MINUS*M_OMEGA_MINUS);
}

float StXiMiniDst::eBachelorPion() {
  return sqrt(mPtot2Bachelor+M_PION_MINUS*M_PION_MINUS);
}

float StXiMiniDst::eBachelorKaon() {
  return sqrt(mPtot2Bachelor+M_KAON_MINUS*M_KAON_MINUS);
}

float StXiMiniDst::massOmega() {
  return sqrt(pow(eLambda()+eBachelorKaon(),2)-mPtot2Xi);
}

float StXiMiniDst::massXi() {
  return sqrt(pow(eLambda()+eBachelorPion(),2)-mPtot2Xi);
}

float StXiMiniDst::rapXi() {
  float exi = eXi();
  return 0.5*log((exi+mMomXi[2])/(exi-mMomXi[2]));
}

float StXiMiniDst::rapOmega() {
  float eom = eOmega();
  return 0.5*log((eom+mMomXi[2])/(eom-mMomXi[2]));
}

float StXiMiniDst::cTauOmega() {
  return massOmega()*mDecayLengthXi/sqrt(mPtot2Xi);
}

float StXiMiniDst::cTauXi() {
  return massXi()*mDecayLengthXi/sqrt(mPtot2Xi);
}

float StXiMiniDst::ptBachelor() {
  return sqrt(mPtot2Bachelor-mMomBachelor[2]*mMomBachelor[2]);
}

float StXiMiniDst::ptotBachelor() {
  return sqrt(mPtot2Bachelor);
}

float StXiMiniDst::ptXi() {
  return sqrt(mPt2Xi);
}

float StXiMiniDst::ptotXi() {
  return sqrt(mPtot2Xi);
}

