//
// StPicoMtdHit holds information about MTD hit
//

// C++ headers
#include <limits>

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoMtdHit.h"

ClassImp(StPicoMtdHit)

//_________________
StPicoMtdHit::StPicoMtdHit(): TObject(), mgChannel(-1), mTriggerFlag(0),
  mLeadingEdgeTime{ -999., -999.}, mTrailingEdgeTime{ -999., -999} {
  /* empty */
}

//_________________
StPicoMtdHit::StPicoMtdHit(const StPicoMtdHit &hit) : TObject() {
  mgChannel = hit.mgChannel;
  mTriggerFlag = hit.mTriggerFlag;
  mLeadingEdgeTime = hit.mLeadingEdgeTime;
  mTrailingEdgeTime = hit.mTrailingEdgeTime;
}

//_________________
StPicoMtdHit::~StPicoMtdHit() {
  /* emtpy */
}

//_________________
void StPicoMtdHit::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " Backleg = " << backleg()
           << " Module  = " << module()
           << " Cell    = " << cell() << endm;
}

//_________________
void StPicoMtdHit::setHitChannel(Int_t backleg, Int_t module, Int_t cell) {
  
  Int_t gchan = (backleg - 1) * 60 + (module - 1) * 12 + cell;

  // Check the range of the hit value
  if( gchan < std::numeric_limits<short>::max() ) {
    mgChannel = (Short_t)gchan;
  }
  else {
    mgChannel = -1;
    LOG_INFO << "Weird cell: backleg = " << backleg
             << ", module = " << module
             << ", cell = " << cell
             << endm;
  }
}

//_________________
void StPicoMtdHit::setLeadingEdgeTime(std::pair<Float_t, Float_t> leadingEdgeTime) {
  mLeadingEdgeTime = leadingEdgeTime;
}

//_________________
void StPicoMtdHit::setTrailingEdgeTime(std::pair<Float_t, Float_t> trailingEdgeTime) {
  mTrailingEdgeTime = trailingEdgeTime;
}
