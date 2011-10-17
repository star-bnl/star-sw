/*
 * Implementation file for StMuPrimaryVertex
 * A simple class to store porimary vertex information
 * All functions are inline
 *
 * $Id: StMuPrimaryVertex.cxx,v 1.7 2011/10/17 00:19:14 fisyak Exp $ 
 */

#include "StMuPrimaryVertex.h"
#include "StEvent/StPrimaryVertex.h"
#include "StEventUtilities/StuRefMult.hh"
#include "StEventUtilities/StuFtpcRefMult.hh"
#include "TString.h"
ClassImp(StMuPrimaryVertex)
  
StMuPrimaryVertex::StMuPrimaryVertex(const StPrimaryVertex*& vertex) {
  mPosition       = vertex->position();
  mPosError       = vertex->positionError();
  mVertexFinderId = vertex->vertexFinderId();
  mRanking        = vertex->ranking();
  mNTracksUsed    = vertex->numTracksUsedInFinder();
  mNBTOFMatch     = vertex->numMatchesWithBTOF();
  mNCTBMatch      = vertex->numMatchesWithCTB();
  mNBEMCMatch     = vertex->numMatchesWithBEMC();
  mNEEMCMatch     = vertex->numMatchesWithEEMC();
  mNCrossCentralMembrane  = vertex->numTracksCrossingCentralMembrane();
  mSumTrackPt     = vertex->sumOfTrackPt();
  mMeanDip        = vertex->meanDip();
  mChiSquared     = vertex->chiSquared();

  mRefMultPos = uncorrectedNumberOfPositivePrimaries(vertex);
  mRefMultNeg = uncorrectedNumberOfNegativePrimaries(vertex); 
  mRefMultFtpcEast = uncorrectedNumberOfFtpcEastPrimaries(vertex);
  mRefMultFtpcWest = uncorrectedNumberOfFtpcWestPrimaries(vertex); 

  mIdTruth  = vertex->idTruth();
  mQuality  = vertex->qaTruth();
  mIdParent = vertex->idParent();
}
#if 0
void StMuPrimaryVertex::Print(Option_t *option) const {
  cout << "Vertex position " << mPosition << endl;
  cout << "       errors   " << mPosError << endl;
  cout << "Rank        " << mRanking << endl;
  cout << "Chisquared  " << mChiSquared << endl;
  cout << "RefMult     " << refMult() << " ( pos " << mRefMultPos << ", neg " 
       << mRefMultNeg << " )" << endl;
  cout << "Tracks used " << mNTracksUsed << endl;
  cout << "Mean dip    " << mMeanDip << endl;
}
#endif
//________________________________________________________________________________
ostream&              operator<<(ostream& os,  const StMuPrimaryVertex& v) {
  os << Form("Vx:%7.3f+/-%6.3f %7.3f+/-%6.3f %7.3f+/-%6.3f",
	     v.position().x(),v.posError().x(),
	     v.position().y(),v.posError().y(),
	     v.position().z(),v.posError().z());
  os << Form(" Rank:%6.0f RefMult:%4i(%4i%4i)",v.ranking(),v.refMult(),v.refMultPos(),-v.refMultNeg());
  os << Form(" Tracks U:%4i ",v.nTracksUsed());
  return os;
}
//________________________________________________________________________________
void StMuPrimaryVertex::Print(Option_t *option) const {cout << *this << endl;}
//________________________________________________________________________________
// $Log: StMuPrimaryVertex.cxx,v $
// Revision 1.7  2011/10/17 00:19:14  fisyak
// Active handing of IdTruth
//
