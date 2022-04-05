//
// StPicoMcVertex holds information about Monte Carlo vertex (generator + GEANT)
//

// ROOT headers
#include "TString.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoMcVertex.h"

ClassImp(StPicoMcVertex)

//_________________
StPicoMcVertex::StPicoMcVertex() : TObject(),
  mId(0), mNoDaughters(0), mIdParTrk(0), mIsInterm(0),
  mTime(0), mVx(0), mVy(0), mVz(0) {
  // Default constructor
  /* emtpy */
}

//_________________
StPicoMcVertex::StPicoMcVertex(const StPicoMcVertex& v) : TObject() {
  // Copy constructor
  mId = v.mId;
  mNoDaughters = v.mNoDaughters;
  mIdParTrk = v.mIdParTrk;
  mIsInterm = v.mIsInterm;
  mTime = v.mTime;
  mVx = v.mVx;
  mVy = v.mVy;
  mVz = v.mVz;
}

//_________________
StPicoMcVertex::~StPicoMcVertex() {
  // Destructor
  /* empty */
}

//_________________
void StPicoMcVertex::Print(const Char_t* option __attribute__((unused))) const {
  LOG_INFO << "id: " << id() << " nDaughters: " << numberOfDaughters()
	   << " parTrkId: " << idOfParentTrack() << " isInterim: " << isIntermediate()
	   << " time: " << time()
	   << Form(" x/y/z: %5.2f/%5.2f/%5.2f\n",
		   position().X(), position().Y(), position().Z()) << endm;
}
