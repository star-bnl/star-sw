#include "StPicoDstMaker/StPicoEvent.h"

#include "StPicoHFEvent.h"
#include "StHFPair.h"
#include "StHFTriplet.h"
#include "StHFQuadruplet.h"

ClassImp(StPicoHFEvent)

TClonesArray *StPicoHFEvent::fgHFSecondaryVerticesArray = 0;
TClonesArray *StPicoHFEvent::fgHFTertiaryVerticesArray  = 0;

// _________________________________________________________
StPicoHFEvent::StPicoHFEvent() : mRunId(-1), mEventId(-1), mNHFSecondaryVertices(0), mNHFTertiaryVertices(0),
						  mHFSecondaryVerticesArray(NULL), mHFTertiaryVerticesArray(NULL) {
  // -- Default constructor
  if (!fgHFSecondaryVerticesArray) fgHFSecondaryVerticesArray = new TClonesArray("StHFPair");
  mHFSecondaryVerticesArray = fgHFSecondaryVerticesArray;
}

// _________________________________________________________
StPicoHFEvent::StPicoHFEvent(unsigned int mode) : mRunId(-1), mEventId(-1), mNHFSecondaryVertices(0), mNHFTertiaryVertices(0),
						  mHFSecondaryVerticesArray(NULL), mHFTertiaryVerticesArray(NULL) {
  // -- Constructor with mode selection
  if (mode == StPicoHFEvent::kTwoAndTwoParticleDecay) {
    if (!fgHFSecondaryVerticesArray) fgHFSecondaryVerticesArray = new TClonesArray("StHFPair");
    mHFSecondaryVerticesArray = fgHFSecondaryVerticesArray;

    if (!fgHFTertiaryVerticesArray) fgHFTertiaryVerticesArray = new TClonesArray("StHFPair");
    mHFTertiaryVerticesArray = fgHFTertiaryVerticesArray;
  }
  else if (mode == StPicoHFEvent::kThreeParticleDecay) {
    if (!fgHFSecondaryVerticesArray) fgHFSecondaryVerticesArray = new TClonesArray("StHFTriplet");
    mHFSecondaryVerticesArray = fgHFSecondaryVerticesArray;
  }
  else if (mode == StPicoHFEvent::kTwoParticleDecay) {
    if (!fgHFSecondaryVerticesArray) fgHFSecondaryVerticesArray = new TClonesArray("StHFPair");
    mHFSecondaryVerticesArray = fgHFSecondaryVerticesArray;
  }
  else if (mode == StPicoHFEvent::kFourParticleDecay) {
    if (!fgHFSecondaryVerticesArray) fgHFSecondaryVerticesArray = new TClonesArray("StHFPair");
    mHFSecondaryVerticesArray = fgHFSecondaryVerticesArray;
  }
  else {
    if (!fgHFSecondaryVerticesArray) fgHFSecondaryVerticesArray = new TClonesArray("StHFPair");
    mHFSecondaryVerticesArray = fgHFSecondaryVerticesArray;
  }
}

// _________________________________________________________
void StPicoHFEvent::addPicoEvent(StPicoEvent const & picoEvent) {
   // -- add StPicoEvent variables
   mRunId   = picoEvent.runId();
   mEventId = picoEvent.eventId();
}

// _________________________________________________________
void StPicoHFEvent::clear(char const *option) {
  mHFSecondaryVerticesArray->Clear(option);
  if (mHFTertiaryVerticesArray)
    mHFTertiaryVerticesArray->Clear(option);
  
  mRunId                = -1;
  mEventId              = -1;
  mNHFSecondaryVertices = 0;
  mNHFTertiaryVertices  = 0;
}

// _________________________________________________________
void StPicoHFEvent::addHFSecondaryVertexPair(StHFPair const* t) {
  TClonesArray &vertexArray = *mHFSecondaryVerticesArray;
  new(vertexArray[mNHFSecondaryVertices++]) StHFPair(t);
}

// _________________________________________________________
void StPicoHFEvent::addHFSecondaryVertexTriplet(StHFTriplet const* t) {
  TClonesArray &vertexArray = *mHFSecondaryVerticesArray;
  new(vertexArray[mNHFSecondaryVertices++]) StHFTriplet(t);
}

// _________________________________________________________
void StPicoHFEvent::addHFTertiaryVertexPair(StHFPair const* t) {
  TClonesArray &vertexArray = *mHFTertiaryVerticesArray;
  new(vertexArray[mNHFTertiaryVertices++]) StHFPair(t);
}
// _________________________________________________________
void StPicoHFEvent::addHFSecondaryVertexQuadruplet(StHFQuadruplet const* t) {
  TClonesArray &vertexArray = *mHFTertiaryVerticesArray;
  new(vertexArray[mNHFTertiaryVertices++]) StHFQuadruplet(t);
}
