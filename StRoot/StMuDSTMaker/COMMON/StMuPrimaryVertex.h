#ifndef __StMuPrimaryVertex_hh__
#define __StMuPrimaryVertex_hh__
/*
 * Simple class to store primary vertices. Data members are a mainly a copy of 
 * StPrimaryVertex
 *
 * $Id: StMuPrimaryVertex.h,v 1.11 2017/05/17 16:03:04 smirnovd Exp $ 
 */

#include "TObject.h"
#include "StThreeVectorF.hh"
#include "StEnumerations.h"

class StPrimaryVertex;

class StMuPrimaryVertex : public TObject {

 public:

  StMuPrimaryVertex(): mFlag(0), mPosition(-999,-999,-999), mPosError(-999,-999,-999), mVertexFinderId(undefinedVertexFinder), 
    mRanking(999),mNTracksUsed(0), mNCTBMatch(0), mNBEMCMatch(0), mNEEMCMatch(0), mNCrossCentralMembrane(0),
    mSumTrackPt(-999),mMeanDip(-999), mChiSquared(9999), mNTracks(0), mNTpcWestOnly(0), mNTpcEastOnly(0), mRefMultNeg(0), mRefMultPos(0), mRefMultFtpcWest(0), 
    mRefMultFtpcEast(0), mIdTruth(0), mQuality(0), mIdParent(0) {}

  StMuPrimaryVertex(const StPrimaryVertex* vertex);

  ~StMuPrimaryVertex() {;}
   Int_t            flag() const {return mFlag; }
   StThreeVectorF   position() const        { return mPosition; }
   StThreeVectorF   posError() const        { return mPosError; }
   StVertexFinderId vertexFinderId()  const { return mVertexFinderId; } 
   Float_t          ranking()  const        { return mRanking; }
   UShort_t         nTracksUsed() const     { return mNTracksUsed; }
   UShort_t         nBTOFMatch()  const     { return mNBTOFMatch; }
   UShort_t         nCTBMatch()   const     { return mNCTBMatch; }
   UShort_t         nBEMCMatch()  const     { return mNBEMCMatch; }
   UShort_t         nEEMCMatch()  const     { return mNEEMCMatch; }
   UShort_t         nPostXtracks() const { return mNPostXTracks;}
   UShort_t         nPromptTracks() const { return mNTracksWithPromptHit;}
   UShort_t         nBTOFNotMatch() const      { return mNBTOFNotMatch; }
   UShort_t         nCTBNotMatch()  const      { return mNCTBNotMatch; }
   UShort_t         nBEMCNotMatch() const      { return mNBEMCNotMatch; }
   UShort_t         nEEMCNotMatch() const      { return mNEEMCNotMatch; }
   UShort_t         nCrossCentralMembrane() const  { return mNCrossCentralMembrane; }
   UShort_t         nTpcWestOnly() const  { return mNTpcWestOnly; }
   UShort_t         nTpcEastOnly() const  { return mNTpcEastOnly; }
   
   Float_t          sumTrackPt() const      { return mSumTrackPt; }
   Float_t          meanDip() const         { return mMeanDip; }
   Float_t          chiSquared() const      { return mChiSquared; }
   UShort_t         noTracks() const {return mNTracks;}
   UShort_t         refMultPos() const      { return mRefMultPos; }
   UShort_t         refMultNeg() const      { return mRefMultNeg; }
   UShort_t         refMult()    const      { return refMultPos() + refMultNeg(); }
   UShort_t         refMultFtpcEast() const { return mRefMultFtpcEast; }
   UShort_t         refMultFtpcWest() const { return mRefMultFtpcWest; }
   UShort_t         refMultFtpc()     const { return refMultFtpcEast() + refMultFtpcWest(); }
   void setPosition(const StThreeVectorF &pos)     { mPosition = pos; }
   void setPosError(const StThreeVectorF &pos_err) { mPosError = pos_err; }
   Int_t            idTruth() const { return mIdTruth;}
   Int_t            qaTruth() const { return mQuality; }
   Int_t            idParent() const { return mIdParent;}
   void          setFlag(Int_t val) { mFlag = val; }
   void          setIdTruth(Int_t idtru,Int_t qatru=0) {mIdTruth = (UShort_t) idtru; mQuality = (UShort_t) qatru;}
   void          setIdParent(Int_t id) {mIdParent = id;}
   Bool_t        isBeamConstrained() const {return TESTBIT(mFlag,kBEAMConstrVtxId);}
   virtual void     Print(Option_t *option="") const; ///< Print essential vertex info

  ClassDef(StMuPrimaryVertex,8)
    
    private:
  Int_t            mFlag;
  StThreeVectorF   mPosition;
  StThreeVectorF   mPosError;
  StVertexFinderId mVertexFinderId;
  Float_t          mRanking;
  UShort_t         mNTracksUsed;
  UShort_t         mNBTOFMatch;
  UShort_t         mNCTBMatch;
  UShort_t         mNBEMCMatch;
  UShort_t         mNEEMCMatch;
  UShort_t         mNBTOFNotMatch;
  UShort_t         mNCTBNotMatch;
  UShort_t         mNBEMCNotMatch;
  UShort_t         mNEEMCNotMatch;
  UShort_t         mNCrossCentralMembrane;
  UShort_t         mNTracksWithPromptHit;
  UShort_t         mNPostXTracks;
  Float_t          mSumTrackPt;
  Float_t          mMeanDip;
  Float_t          mChiSquared;
  UShort_t         mNTracks;
  UShort_t         mNTpcWestOnly;
  UShort_t         mNTpcEastOnly;
  // RefMult fields
  UShort_t         mRefMultNeg;
  UShort_t         mRefMultPos;
  UShort_t         mRefMultFtpcWest;
  UShort_t         mRefMultFtpcEast;
  // IdTruth
  UShort_t         mIdTruth; // MC vertex id if any 
  UShort_t         mQuality; // quality of this information (percentage of tracks coming the above MC Vertex)
  Int_t            mIdParent;
};
ostream&              operator<<(ostream& os,  const StMuPrimaryVertex& v);
#endif
/*
 * $Log: StMuPrimaryVertex.h,v $
 * Revision 1.11  2017/05/17 16:03:04  smirnovd
 * StMuPrimaryVertex: Passing pointer by reference is pointless
 *
 * Revision 1.10  2017/04/17 19:19:43  smirnovd
 * [Cosmetic] Whitespace adjustments
 *
 * Revision 1.9  2012/09/16 21:58:16  fisyak
 * Make use of Tpc West and East Only no. of tracks
 *
 * Revision 1.8  2012/05/07 14:47:06  fisyak
 * Add handles for track to fast detector matching
 *
 * Revision 1.7  2011/10/17 00:19:14  fisyak
 * Active handing of IdTruth
 *
 * Revision 1.6  2011/04/08 01:25:51  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.5  2009/12/24 21:19:31  tone421
 * Added StMuPrimaryVertex::nBTOFMatch()
 *
 * Revision 1.4  2007/09/18 02:29:58  mvl
 * Added basic printing functionality. For convenience and to assist data consistency checks
 *
 * Revision 1.3  2006/04/25 08:27:43  mvl
 * Added members and getters for mean-dip and chisquared value
 *
 * Revision 1.2  2005/08/19 19:46:05  mvl
 * Further updates for multiple vertices. The main changes are:
 * 1) StMudst::primaryTracks() now returns a list (TObjArray*) of tracks
 *    belonging to the 'current' primary vertex. The index number of the
 *    'current' vertex can be set using StMuDst::setCurrentVertex().
 *    This also affects StMuDst::primaryTracks(int i) and
 *    StMuDst::numberOfprimaryTracks().
 * 2) refMult is now stored for all vertices, in StMuPrimaryVertex. The
 *    obvious way to access these numbers is from the StMuprimaryVertex structures,
 *    but for ebakcward compatibility a function is provided in StMuEvent as well
 *    (this is the only function that works for existing MuDst)
 *
 * As an aside, I've also changes the internals of StMuDst::createStEvent and
 * StMuDst::fixTrackIndices() to be able to deal with a larger range of index numbers for tracks as generated by Ittf.
 *
 * BIG FAT WARNING: StMudst2StEventMaker and StMuDstFilterMaker
 * do not fully support the multiple vertex functionality yet.
 *
 * Revision 1.1  2005/07/15 21:45:09  mvl
 * Added support for multiple primary vertices (StMuPrimaryVertex). Track Dcas are now calculated with repect to the first vertex in the list (highest rank), but another vertex number can be specified. Tarcks also store the index of the vertex they belong to (StMuTrack::vertexIndex())
 *
 */
