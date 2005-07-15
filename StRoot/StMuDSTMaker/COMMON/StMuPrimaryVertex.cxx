/*
 * Implementation file for StMuPrimaryVertex
 * A simple class to store porimary vertex information
 * All functions are inline
 *
 * $Id: StMuPrimaryVertex.cxx,v 1.1 2005/07/15 21:45:09 mvl Exp $ 
 */

#include "StMuPrimaryVertex.h"
#include "StEvent/StPrimaryVertex.h"

ClassImp(StMuPrimaryVertex)
  
StMuPrimaryVertex::StMuPrimaryVertex(StPrimaryVertex *vertex) {
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
}
