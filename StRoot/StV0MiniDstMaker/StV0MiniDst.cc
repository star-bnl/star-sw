/***********************************************************************
 *
 * $Id: StV0MiniDst.cc,v 1.8 1999/11/19 19:44:47 genevb Exp $
 *
 * Author: Peter G. Jones, University of Birmingham, 04-Jun-1999
 *
 ***********************************************************************
 *
 * Description: V0 mini dst class
 *
 ***********************************************************************
 *
 * $Log: StV0MiniDst.cc,v $
 * Revision 1.8  1999/11/19 19:44:47  genevb
 * Modified for StEvent 2.0
 *
 * Revision 1.7  1999/09/24 01:23:32  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/09/02 09:04:56  jones
 * Added StEvMiniDst class, New file handling, Partially implemented TTrees
 *
 * Revision 1.5  1999/08/13 12:38:16  jones
 * Major revision to merge StV0MiniDstMaker and StXiMiniDstMaker
 *
 * Revision 1.4  1999/08/03 02:31:44  genevb
 * Better implementation of StHFillObject
 *
 * Revision 1.3  1999/07/30 15:01:13  genevb
 * Switched from TObject to StHFillObject inheritance
 *
 * Revision 1.2  1999/07/26 19:17:24  jones
 * Added primary vertex position and v0 daughter DCA to the primary vertex
 *
 * Revision 1.1  1999/07/13 12:42:24  jones
 * *** empty log message ***
 *
 *
 ***********************************************************************/
#include "phys_constants.h"
#include "StV0MiniDst.hh"
#include "StTrack.h"
#include "StTrackFitTraits.h"
#include "StV0Vertex.h"
ClassImp(StV0MiniDst)

StV0MiniDst::StV0MiniDst() { 
}

StV0MiniDst::StV0MiniDst(StV0Vertex* v0Vertex,
			 StEvMiniDst* event) {

  mEvent = event;

  mDecayVertexV0[0] = v0Vertex->position().x();
  mDecayVertexV0[1] = v0Vertex->position().y();
  mDecayVertexV0[2] = v0Vertex->position().z();
  mDcaV0Daughters = v0Vertex->dcaDaughters();
  mDcaV0ToPrimVertex = v0Vertex->dcaParentToPrimaryVertex();
  mDcaPosToPrimVertex = v0Vertex->dcaDaughterToPrimaryVertex(positive);
  mDcaNegToPrimVertex = v0Vertex->dcaDaughterToPrimaryVertex(negative);
  mMomNeg[0] = v0Vertex->momentumOfDaughter(negative).x();
  mMomNeg[1] = v0Vertex->momentumOfDaughter(negative).y();
  mMomNeg[2] = v0Vertex->momentumOfDaughter(negative).z();
  mMomPos[0] = v0Vertex->momentumOfDaughter(positive).x();
  mMomPos[1] = v0Vertex->momentumOfDaughter(positive).y();
  mMomPos[2] = v0Vertex->momentumOfDaughter(positive).z();

  mTpcHitsPos =
    v0Vertex->daughter(positive)->fitTraits().numberOfFitPoints(kTpcId);
  mTpcHitsNeg =
    v0Vertex->daughter(negative)->fitTraits().numberOfFitPoints(kTpcId);
  
  this->UpdateV0();
}

StV0MiniDst::~StV0MiniDst() {
}

void StV0MiniDst::UpdateV0() { 
  // Calculate derived data members

     mDecayLengthV0 = sqrt(pow(mDecayVertexV0[0] -
			       mEvent->primaryVertex()[0],2) +
			   pow(mDecayVertexV0[1] -
			       mEvent->primaryVertex()[1],2) +
			   pow(mDecayVertexV0[2] - 
			       mEvent->primaryVertex()[2],2));
  
          mPtot2Pos = mMomPos[0]*mMomPos[0] +
	              mMomPos[1]*mMomPos[1] +
	              mMomPos[2]*mMomPos[2];

          mPtot2Neg = mMomNeg[0]*mMomNeg[0] +
                      mMomNeg[1]*mMomNeg[1] +
                      mMomNeg[2]*mMomNeg[2];

          mMomV0[0] = mMomPos[0] + mMomNeg[0];
          mMomV0[1] = mMomPos[1] + mMomNeg[1];
          mMomV0[2] = mMomPos[2] + mMomNeg[2];
             mPt2V0 = mMomV0[0]*mMomV0[0] + mMomV0[1]*mMomV0[1];
           mPtot2V0 = mPt2V0 + mMomV0[2]*mMomV0[2];

     mMomPosAlongV0 = ( mMomPos[0]*mMomV0[0] + 
			mMomPos[1]*mMomV0[1] +
			mMomPos[2]*mMomV0[2] ) / sqrt(mPtot2V0); 
     mMomNegAlongV0 = ( mMomNeg[0]*mMomV0[0] + 
			mMomNeg[1]*mMomV0[1] + 
			mMomNeg[2]*mMomV0[2] ) / sqrt(mPtot2V0);
}

float StV0MiniDst::alphaV0() {
  return (mMomPosAlongV0-mMomNegAlongV0)/
         (mMomPosAlongV0+mMomNegAlongV0);
}

float StV0MiniDst::ptArmV0() {
  return sqrt(mPtot2Pos - mMomPosAlongV0*mMomPosAlongV0);
}

float StV0MiniDst::eLambda() {
  return sqrt(mPtot2V0+M_LAMBDA*M_LAMBDA);
}

float StV0MiniDst::eK0Short() {
  return sqrt(mPtot2V0+M_KAON_0_SHORT*M_KAON_0_SHORT);
}

float StV0MiniDst::ePosProton() {
  return sqrt(mPtot2Pos+M_PROTON*M_PROTON);
}

float StV0MiniDst::eNegProton() {
  return sqrt(mPtot2Neg+M_ANTIPROTON*M_ANTIPROTON);
}

float StV0MiniDst::ePosPion() {
  return sqrt(mPtot2Pos+M_PION_PLUS*M_PION_PLUS);
}

float StV0MiniDst::eNegPion() {
  return sqrt(mPtot2Neg+M_PION_MINUS*M_PION_MINUS);
}

float StV0MiniDst::massLambda() {
  return sqrt(pow(ePosProton()+eNegPion(),2)-mPtot2V0);
}

float StV0MiniDst::massAntiLambda() {
  return sqrt(pow(eNegProton()+ePosPion(),2)-mPtot2V0);
}

float StV0MiniDst::massK0Short() {
  return sqrt(pow(ePosPion()+eNegPion(),2)-mPtot2V0);
}

float StV0MiniDst::rapLambda() {
  float ela = eLambda();
  return 0.5*log((ela+mMomV0[2])/(ela-mMomV0[2]));
}

float StV0MiniDst::rapK0Short() {
  float ek0 = eK0Short();
  return 0.5*log((ek0+mMomV0[2])/(ek0-mMomV0[2]));
}

float StV0MiniDst::cTauLambda() {
  return massLambda()*mDecayLengthV0/sqrt(mPtot2V0);
}

float StV0MiniDst::cTauK0Short() {
  return massK0Short()*mDecayLengthV0/sqrt(mPtot2V0);
}

float StV0MiniDst::ptPos() {
  return sqrt(mPtot2Pos-mMomPos[2]*mMomPos[2]);
}

float StV0MiniDst::ptotPos() {
  return sqrt(mPtot2Pos);
}

float StV0MiniDst::ptNeg() {
  return sqrt(mPtot2Neg-mMomNeg[2]*mMomNeg[2]);
}

float StV0MiniDst::ptotNeg() {
  return sqrt(mPtot2Neg);
}

float StV0MiniDst::ptV0() {
  return sqrt(mPt2V0);
}

float StV0MiniDst::ptotV0() {
  return sqrt(mPtot2V0);
}
