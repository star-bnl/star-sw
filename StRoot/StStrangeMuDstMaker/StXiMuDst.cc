/***********************************************************************
 *
 * $Id: StXiMuDst.cc,v 3.5 2008/07/10 16:16:55 genevb Exp $
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
 * Revision 3.5  2008/07/10 16:16:55  genevb
 * Allow for marking of bad tracks -> bad secondary vertices
 *
 * Revision 3.4  2003/08/26 22:36:28  genevb
 * Calculate Xi momenta at/near primary vertex
 *
 * Revision 3.3  2001/11/05 23:41:07  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.2  2001/05/04 20:15:15  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.1  2000/08/10 01:16:25  genevb
 * Added number of dedx points
 *
 * Revision 3.0  2000/07/14 12:56:50  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
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
#include "StXiMuDst.hh"
#include "StXiVertex.h"
#include "StTrack.h"
#include "StTrackFitTraits.h"
#include "StStrangeEvMuDst.hh"
#include "StDedxPidTraits.h"
#include "phys_constants.h"
#include "SystemOfUnits.h"
#include "StPhysicalHelixD.hh"

StPhysicalHelixD XiHelix;
StThreeVectorD temp3VD;

ClassImp(StXiMuDst)

StXiMuDst::StXiMuDst() : StXiI() { 
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
  if (trk->bad()) setBachelorBad();
  mDedxBachelor = 0.;
  mNumDedxBachelor = 0;
  // For now, get the truncated mean dE/dX from the TPC
  StPtrVecTrackPidTraits pidBachelor = trk->pidTraits(kTpcId);
  for (UInt_t i=0; i<pidBachelor.size(); i++) {
    StDedxPidTraits* pid = (StDedxPidTraits*) pidBachelor[i];
    if (pid->method() == kTruncatedMeanId) {
      mDedxBachelor = pid->mean();
      mErrDedxBachelor = pid->errorOnMean();
      mNumDedxBachelor = pid->numberOfPoints() + (100*((int) (pid->length())));
      break;
    }
  }
}

StXiMuDst::~StXiMuDst() {
}

Long_t StXiMuDst::detectorIdXi() {
  return ((100*detectorIdV0())+
               detectorIdTrack(mTopologyMapBachelor));
}

Long_t StXiMuDst::detectorIdPars() {
  return 1;  // Currently, only one set of parameters actually used
}

void StXiMuDst::setXiHelix() {
  double pt        = ptXi();
  double bcharge   = mCharge*(mEvent->magneticField());
  double curvature = TMath::Abs(bcharge)*C_D_CURVATURE/pt;
  double dip       = TMath::ATan(momXiZ()/pt);
  int    h         = ((bcharge > 0) ? -1 : 1);
  double phase     = TMath::ATan2(momXiY(),momXiX()) - (h*TMath::PiOver2());
  temp3VD.setX(mDecayVertexXiX);
  temp3VD.setY(mDecayVertexXiY);
  temp3VD.setZ(mDecayVertexXiZ);
  XiHelix.setParameters(curvature,dip,phase,temp3VD,h);
}

StPhysicalHelixD& StXiMuDst::helixXi() {
  setXiHelix();
  return XiHelix;
}

TVector3 StXiMuDst::momXiAtPrimVertex() {
  setXiHelix();
  temp3VD.setX(mEvent->primaryVertexX());
  temp3VD.setY(mEvent->primaryVertexY());
  temp3VD.setZ(mEvent->primaryVertexZ());

  // actually the momentum at the DCA to the primary vertex
  temp3VD = XiHelix.momentumAt(XiHelix.pathLength(temp3VD),
    mEvent->magneticField()*kilogauss);
  return TVector3(temp3VD.x(),temp3VD.y(),temp3VD.z());
}
