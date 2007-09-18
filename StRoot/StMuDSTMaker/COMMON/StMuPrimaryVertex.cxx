/*
 * Implementation file for StMuPrimaryVertex
 * A simple class to store porimary vertex information
 * All functions are inline
 *
 * $Id: StMuPrimaryVertex.cxx,v 1.4 2007/09/18 02:29:58 mvl Exp $ 
 */

#include "StMuPrimaryVertex.h"
#include "StEvent/StPrimaryVertex.h"
#include "StEventUtilities/StuRefMult.hh"
#include "StEventUtilities/StuFtpcRefMult.hh"

ClassImp(StMuPrimaryVertex)
  
StMuPrimaryVertex::StMuPrimaryVertex(const StPrimaryVertex*& vertex) {
  mPosition       = vertex->position();
  mPosError       = vertex->positionError();
  mVertexFinderId = vertex->vertexFinderId();
  mRanking        = vertex->ranking();
  mNTracksUsed    = vertex->numTracksUsedInFinder();
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
}

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
