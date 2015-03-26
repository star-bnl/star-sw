#include "StMuDSTMaker/COMMON/StMuMtdHit.h"
#include "StMessMgr.h"

#include "StPicoMtdHit.h"
#include "StPicoConstants.h"


ClassImp(StPicoMtdHit)

//----------------------------------------------------------------------------------
StPicoMtdHit::StPicoMtdHit()
{
  //constructor
  mgChannel = -1;
  mTriggerFlag = 0;
  mLeadingEdgeTime.first   = -999.;
  mLeadingEdgeTime.second  = -999.;
  mTrailingEdgeTime.first  = -999.;
  mTrailingEdgeTime.second = -999.;
}

//----------------------------------------------------------------------------------
StPicoMtdHit::StPicoMtdHit(const StMuMtdHit* hit)
{
  mTriggerFlag = 0;
  Int_t gchan = (hit->backleg()-1)*60 + (hit->module()-1)*12 + hit->cell();
  if(gchan<Pico::SHORTMAX)
    {
      mgChannel = (Short_t)gchan;
    }
  else
    {
      mgChannel = -1;
      LOG_INFO << "Weird cell: backlet = " << hit->backleg()
	       << ", module = " << hit->module()
	       << ", cell = " << hit->cell()
	       << endm;
    }
  mLeadingEdgeTime  = (pair<Float_t,Float_t>)hit->leadingEdgeTime();
  mTrailingEdgeTime = (pair<Float_t,Float_t>)hit->trailingEdgeTime();
}

//----------------------------------------------------------------------------------
StPicoMtdHit::~StPicoMtdHit()
{ 
  // dummy destructor
}

//----------------------------------------------------------------------------------
void StPicoMtdHit::Print(const Char_t *option) const
{
  LOG_INFO << " Backleg = " << backleg()
	   << " Module  = " << module()
	   << " Cell    = " << cell()
	   <<endm;
}
