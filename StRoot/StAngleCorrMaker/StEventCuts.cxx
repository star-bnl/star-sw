#include "StEventCuts.h"
#include "StEvent.h"

StEventCuts::StEventCuts() {}

StEventCuts::~StEventCuts() {}

Int_t 
StEventCuts::EventSatisfiesCuts(StEvent& ev)
{
  return 1; 
}

void 
StEventCuts::SetMultiplicityCuts(Double_t lowerCut, Double_t upperCut)
{
  upperMult = upperCut;
  lowerMult = lowerCut;

}



