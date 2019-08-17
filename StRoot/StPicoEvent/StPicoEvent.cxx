//
// StPicoEvent: holds the information about the event
//

// C++ headers
#include <algorithm>
#include <limits>

#include "StPicoDst.h"
#ifdef __TFG__VERSION__
#include "StEvent/StBTofHeader.h"
#include "StEvent/StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StEventUtilities/StGoodTrigger.h"
#include "TEnv.h"
#endif /* __TFG__VERSION__ */
// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoEvent.h"

ClassImp(StPicoEvent)

//_________________
StPicoEvent::StPicoEvent(): TObject(),
#ifdef __TFG__VERSION__
  mProductionVersion(gEnv->GetValue("STAR_GIT_VERSION","Unknown")),
#endif /* __TFG__VERSION__ */
  mRunId(0), mEventId(0), mFillId(0), mBField(0), mTime(0),
  mPrimaryVertexX(0), mPrimaryVertexY(0), mPrimaryVertexZ(0),
  mPrimaryVertexErrorX(0), mPrimaryVertexErrorY(0), mPrimaryVertexErrorZ(0),
#ifdef __TFG__VERSION__
  mPrimaryVertexCorr{0,0,0},	
#endif /* __TFG__VERSION__ */
  mRanking(-999), mNBEMCMatch(0), mNBTOFMatch(0),
  mTriggerIds(),
  mRefMultFtpcEast(0), mRefMultFtpcWest(0),
  mRefMultNeg(0), mRefMultPos(0),
  mRefMult2NegEast(0), mRefMult2PosEast(0), mRefMult2NegWest(0), mRefMult2PosWest(0),
  mRefMult3NegEast(0), mRefMult3PosEast(0), mRefMult3NegWest(0), mRefMult3PosWest(0),
  mRefMult4NegEast(0), mRefMult4PosEast(0), mRefMult4NegWest(0), mRefMult4PosWest(0),
  mRefMultHalfNegEast(0), mRefMultHalfPosEast(0), mRefMultHalfNegWest(0), mRefMultHalfPosWest(0),
  mGRefMult(0), mNumberOfGlobalTracks(0), mbTofTrayMultiplicity(0), mNHitsHFT{},
  mNVpdHitsEast(0), mNVpdHitsWest(0), mNTofT0(0), mVzVpd(-999.),
  mZDCx(0), mBBCx(0), mBackgroundRate(0), mBbcBlueBackgroundRate(0), mBbcYellowBackgroundRate(0),
  mBbcEastRate(0), mBbcWestRate(0), mZdcEastRate(0), mZdcWestRate(0),
  mZdcSumAdcEast(0), mZdcSumAdcWest(0),
  mZdcSmdEastHorizontal{}, mZdcSmdEastVertical{}, mZdcSmdWestHorizontal{}, mZdcSmdWestVertical{},
  mBbcAdcEast{}, mBbcAdcWest{}, mHighTowerThreshold{}, mJetPatchThreshold{},
  mETofHitMultiplicity(0), mETofDigiMultiplicity(0) {

  // Default constructor
  if( !mTriggerIds.empty() ) {
    mTriggerIds.clear();
  }
}

//_________________
StPicoEvent::StPicoEvent(const StPicoEvent &event) : TObject() {

  // Copy constructor

  // Global info
  mRunId = event.mRunId;
  mEventId = event.mEventId;
  mFillId = event.mFillId;
  mBField = event.mBField;
  mTime = event.mTime;

  // Primary vertex info
  mPrimaryVertexX = event.mPrimaryVertexX;
  mPrimaryVertexY = event.mPrimaryVertexY;
  mPrimaryVertexZ = event.mPrimaryVertexZ;
  mPrimaryVertexErrorX = event.mPrimaryVertexErrorX;
  mPrimaryVertexErrorY = event.mPrimaryVertexErrorY;
  mPrimaryVertexErrorZ = event.mPrimaryVertexErrorZ;
  mRanking = event.mRanking;

  // Number of matched tracks to fast detectors
  mNBEMCMatch = event.mNBEMCMatch;
  mNBTOFMatch = event.mNBTOFMatch;

  // Trigger ID collection
  mTriggerIds = event.mTriggerIds;

  // Reference multiplicities
  mRefMultFtpcEast = event.mRefMultFtpcEast;
  mRefMultFtpcWest = event.mRefMultFtpcWest;
  mRefMultNeg = event.mRefMultNeg;
  mRefMultPos = event.mRefMultPos;
  mRefMult2NegEast = event.mRefMult2NegEast;
  mRefMult2PosEast = event.mRefMult2PosEast;
  mRefMult2NegWest = event.mRefMult2NegWest;
  mRefMult2PosWest = event.mRefMult2PosWest;
  mRefMult3NegEast = event.mRefMult3NegEast;
  mRefMult3PosEast = event.mRefMult3PosEast;
  mRefMult3NegWest = event.mRefMult3NegWest;
  mRefMult3PosWest = event.mRefMult3PosWest;
  mRefMult4NegEast = event.mRefMult4NegEast;
  mRefMult4PosEast = event.mRefMult4PosEast;
  mRefMult4NegWest = event.mRefMult4NegWest;
  mRefMult4PosWest = event.mRefMult4PosWest;
  mRefMultHalfNegEast = event.mRefMultHalfNegEast;
  mRefMultHalfPosEast = event.mRefMultHalfPosEast;
  mRefMultHalfNegWest = event.mRefMultHalfNegWest;
  mRefMultHalfPosWest = event.mRefMultHalfPosWest;

  mGRefMult = event.mGRefMult;
  mNumberOfGlobalTracks = event.mNumberOfGlobalTracks;

  // Hit mulitplicities
  mbTofTrayMultiplicity = event.mbTofTrayMultiplicity;
  mETofHitMultiplicity  = event.mETofHitMultiplicity;
  mETofDigiMultiplicity = event.mETofDigiMultiplicity;
  for(int iIter=0; iIter<4; iIter++) {
    mNHitsHFT[iIter] = event.mNHitsHFT[iIter];
  }

  // VPD info
  mNVpdHitsEast = event.mNVpdHitsEast;
  mNVpdHitsWest = event.mNVpdHitsWest;
  mNTofT0 = event.mNTofT0;
  mVzVpd = event.mVzVpd;

  // Forward detector info
  mZDCx = event.mZDCx;
  mBBCx = event.mBBCx;
  mBackgroundRate = event.mBackgroundRate;
  mBbcBlueBackgroundRate = event.mBbcBlueBackgroundRate;
  mBbcYellowBackgroundRate = event.mBbcYellowBackgroundRate;
  mBbcEastRate = event.mBbcEastRate;
  mBbcWestRate = event.mBbcWestRate;
  mZdcEastRate = event.mZdcEastRate;
  mZdcWestRate = event.mZdcWestRate;

  mZdcSumAdcEast = event.mZdcSumAdcEast;
  mZdcSumAdcWest = event.mZdcSumAdcWest;
  for(int iIter=0; iIter<8; iIter++) {
    mZdcSmdEastHorizontal[iIter] = event.mZdcSmdEastHorizontal[iIter];
    mZdcSmdEastVertical[iIter] = event.mZdcSmdEastVertical[iIter];
    mZdcSmdWestHorizontal[iIter] = event.mZdcSmdWestHorizontal[iIter];
    mZdcSmdWestVertical[iIter] = event.mZdcSmdWestVertical[iIter];
  }

  for(int iIter=0; iIter<24; iIter++) {
    mBbcAdcEast[iIter] = event.mBbcAdcEast[iIter];
    mBbcAdcWest[iIter] = event.mBbcAdcWest[iIter];
  }

  // Tower and patch info
  for(int iIter=0; iIter<4; iIter++) {
    mHighTowerThreshold[iIter] = event.mHighTowerThreshold[iIter];
    mJetPatchThreshold[iIter] = event.mJetPatchThreshold[iIter];
  }
}

//_________________
StPicoEvent::~StPicoEvent() {
  // Destructor
  /* empty */
}

//_________________
void StPicoEvent::Print(const Char_t *option __attribute__((unused)) ) const {
  LOG_INFO << " year = " << year()
	   << " day = " << day() << "\n"
	   << " fill/run/event Id = " << fillId() << "/" << runId() << "/" << eventId() << "\n"
	   
	   << " vertex x = " << primaryVertex().X()
	   << " vertex y = " << primaryVertex().Y()
	   << " vertex z = " << primaryVertex().Z() << "\n"
	   << " refMult = " << refMult()
	   << " grefMult = " << grefMult() << "\n"
	   << " nTofT0 = " << nTofT0()
	   << " vpdVz = " << vzVpd()
	   << endm;
}

//_________________
Int_t StPicoEvent::year() const {
  return mRunId / 1000000 - 1 + 2000;
}

//_________________
Int_t StPicoEvent::day() const {
  return (mRunId % 1000000) / 1000;
}

//_________________
Bool_t StPicoEvent::isTrigger(unsigned int id) const {
  return std::find(mTriggerIds.begin(), mTriggerIds.end(), id) != mTriggerIds.end();
}
#ifdef __TFG__VERSION__
//_________________
Bool_t StPicoEvent::IsGoodTrigger() {
  if (! StGoodTrigger::instance()) return kTRUE;
  return StGoodTrigger::instance()->IsGood(mTriggerIds);
}
#endif /* __TFG__VERSION__ */

//_________________
void StPicoEvent::setTriggerId(UInt_t id) {

  // If trigger list is not empty then loop over it
  // and check if the new trigger already in.
  if( !mTriggerIds.empty() ) {

    // Assume that the new trigger is not in the list
    Bool_t isUsed = false;

    // Loop over the trigger list
    for(UInt_t iIter=0; iIter<mTriggerIds.size(); iIter++) {

      // Compare triggers
      if( mTriggerIds.at(iIter) == id ) {
	isUsed = true;
      }
    } //(unsigned int iIter=0; iIter<mTriggerIds.size(); iIter++)

    // If the trigger not in the list then add it
    if( !isUsed ) {
      mTriggerIds.push_back(id);
    }
  } //if( !mTriggerIds.empty() )
  else {
    mTriggerIds.push_back(id);
  }
}

//_________________
void StPicoEvent::setTriggerIds(std::vector<unsigned int> newIds) {

  // Protection: work only if input vector has entries
  if (!newIds.empty()) {

    // If trigger list is not empty then loop over it
    // and check if the new trigger already in.
    if (!mTriggerIds.empty()) {

      // For each entry in the input vector
      for (UInt_t iIter1= 0; iIter1<newIds.size(); iIter1++) {
	
        // Assume that the new trigger is not in the list
        Bool_t isUsed = false;

        // Loop over existing trigger list
        for (UInt_t iIter2=0; iIter2<mTriggerIds.size(); iIter2++) {

          // Compare triggers
          if (mTriggerIds.at(iIter2) == newIds.at(iIter1)) {
            isUsed = true;
          }
        } //for (unsigned int iIter2=0; iIter2<mTriggerIds.size(); iIter2++)

        // The entry is unique then add it to the list
        if (!isUsed) {
          mTriggerIds.push_back(newIds.at(iIter1));
        }

      } //for(unsigned int iIter1= 0; iIter1<newIds.size(); iIter1++)
    }   //if( !mTriggerIds.empty() )
    else {
      mTriggerIds = newIds;
    }
  } //if( !newIds.empty() )
}

//________________
void StPicoEvent::setNHitsHFT(Int_t layer, UShort_t word) {
  if(layer>=0 && layer<=3) {
    mNHitsHFT[layer] = (UShort_t)word;
  }
  else {
    // Probably some information about this incident may be printed
  }
}

//________________
void StPicoEvent::setZdcSmdEastHorizontal(Int_t strip, Float_t zdcSmdEastHorizontal) {
  if(strip>=0 && strip<=7) {
    mZdcSmdEastHorizontal[strip] = (UShort_t)zdcSmdEastHorizontal;
  }
  else {
    // Probably some information about this incident may be printed
  }
}

//________________
void StPicoEvent::setZdcSmdEastVertical(Int_t strip, Float_t zdcSmdEastVertical) {
  if(strip>=0 && strip<=7) {
    mZdcSmdEastVertical[strip] = (UShort_t)zdcSmdEastVertical;
  }
  else {
    // Probably some information about this incident may be printed
  }
}

//________________
void StPicoEvent::setZdcSmdWestHorizontal(Int_t strip, Float_t zdcSmdWestHorizontal) {
  if(strip>=0 && strip<=7) {
    mZdcSmdWestHorizontal[strip] = (UShort_t)zdcSmdWestHorizontal;
  }
  else {
    // Probably some information about this incident may be printed
  }
}

//________________
void StPicoEvent::setZdcSmdWestVertical(Int_t strip, Float_t zdcSmdWestVertical) {
  if(strip>=0 && strip<=7) {
    mZdcSmdWestVertical[strip] = (UShort_t)zdcSmdWestVertical;
  }
  else {
    // Probably some information about this incident may be printed
  }
}

//________________
void StPicoEvent::setBbcAdcEast(Int_t iPMT, Float_t bbcAdcEast) {
  if(iPMT>=0 && iPMT<=23) {
    mBbcAdcEast[iPMT] = (UShort_t)bbcAdcEast;
  }
  else {
    // Probably some information about this incident may be printed
  }
}

//________________
void StPicoEvent::setBbcAdcWest(Int_t iPMT, Float_t bbcAdcWest) {
  if(iPMT>=0 && iPMT<=23) {
    mBbcAdcWest[iPMT] = (UShort_t)bbcAdcWest;
  }
  else {
    // Probably some information about this incident may be printed
  }
}

//_________________
void StPicoEvent::setBunchId(Int_t id) {
  if( id<0 ) {
    LOG_INFO << "StPicoEvent::setBunchID() - negative bunch ID = " << id << endm;
  }
  else {
    mBunchCrossId = ( ( id > std::numeric_limits<unsigned short>::max() ) ?
		      std::numeric_limits<unsigned short>::max() :
		      (UChar_t)id );
  }
}
