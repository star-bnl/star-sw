#ifndef __StMuPrimaryVertex_hh__
#define __StMuPrimaryVertex_hh__
/*
 * Simple class to store primary vertices. Data members are a mainly a copy of 
 * StPrimaryVertex
 *
 * $Id: StMuPrimaryVertex.h,v 1.1 2005/07/15 21:45:09 mvl Exp $ 
 */

#include "TObject.h"
#include "StThreeVectorF.hh"
#include "StEnumerations.h"

class StPrimaryVertex;

class StMuPrimaryVertex : public TObject {

 public:
  StMuPrimaryVertex(): mPosition(-999,-999,-999), mPosError(-999,-999,-999), mVertexFinderId(undefinedVertexFinder), mRanking(999),mNTracksUsed(0), mNCTBMatch(0), mNBEMCMatch(0), mNEEMCMatch(0), mNCrossCentralMembrane(0), mSumTrackPt(-999) {;}
  StMuPrimaryVertex(StPrimaryVertex *vertex);
  ~StMuPrimaryVertex() {;}

   StThreeVectorF   position() const        { return mPosition; }
   StThreeVectorF   posError() const        { return mPosError; }
   StVertexFinderId vertexFinderId()  const { return mVertexFinderId; } 
   Float_t          ranking()  const        { return mRanking; }
   UShort_t         nTracksUsed() const     { return mNTracksUsed; }
   UShort_t         nCTBMatch() const       { return mNCTBMatch; }
   UShort_t         nBEMCMatch() const      { return mNBEMCMatch; }
   UShort_t         nEEMCMatch() const      { return mNEEMCMatch; }
   UShort_t         nCrossCentralMembrane() const  { return mNCrossCentralMembrane; }
   Float_t          sumTrackPt() const      { return mSumTrackPt; }
   
   void setPosition(const StThreeVectorF &pos)     { mPosition = pos; }
   void setPosError(const StThreeVectorF &pos_err) { mPosError = pos_err; }


  ClassDef(StMuPrimaryVertex,1)
    
    private:
  StThreeVectorF   mPosition;
  StThreeVectorF   mPosError;
  StVertexFinderId mVertexFinderId;
  Float_t          mRanking;
  UShort_t         mNTracksUsed;
  UShort_t         mNCTBMatch;
  UShort_t         mNBEMCMatch;
  UShort_t         mNEEMCMatch;
  UShort_t         mNCrossCentralMembrane;
  Float_t          mSumTrackPt;

};
#endif
/*
 * $Log: StMuPrimaryVertex.h,v $
 * Revision 1.1  2005/07/15 21:45:09  mvl
 * Added support for multiple primary vertices (StMuPrimaryVertex). Track Dcas are now calculated with repect to the first vertex in the list (highest rank), but another vertex number can be specified. Tarcks also store the index of the vertex they belong to (StMuTrack::vertexIndex())
 *
 */
