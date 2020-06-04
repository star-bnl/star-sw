#include <limits>

#include "StMuDSTMaker/COMMON/StMuMtdHit.h"
#include "St_base/StMessMgr.h"

#include "StPicoEvent/StPicoMtdHit.h"


//----------------------------------------------------------------------------------
StPicoMtdHit::StPicoMtdHit(): mgChannel(-1), mTriggerFlag(0), mLeadingEdgeTime{ -999., -999.}, mTrailingEdgeTime{ -999., -999}
{
}

//----------------------------------------------------------------------------------
StPicoMtdHit::StPicoMtdHit(StMuMtdHit const* hit): StPicoMtdHit()
{
  Int_t gchan = (hit->backleg() - 1) * 60 + (hit->module() - 1) * 12 + hit->cell();

  if (gchan < std::numeric_limits<short>::max())
  {
    mgChannel = (Short_t)gchan;
  }
  else
  {
    mgChannel = -1;
    LOG_INFO << "Weird cell: backleg = " << hit->backleg()
             << ", module = " << hit->module()
             << ", cell = " << hit->cell()
             << endm;
  }

  mLeadingEdgeTime  = (pair<Float_t, Float_t>)hit->leadingEdgeTime();
  mTrailingEdgeTime = (pair<Float_t, Float_t>)hit->trailingEdgeTime();
}

//----------------------------------------------------------------------------------
StPicoMtdHit::~StPicoMtdHit()
{
  // dummy destructor
}

//----------------------------------------------------------------------------------
void StPicoMtdHit::Print(const Char_t* option) const
{
  LOG_INFO << " Backleg = " << backleg()
           << " Module  = " << module()
           << " Cell    = " << cell()
           << endm;
}
