#include "StEventCuts.h"

StEventCuts::StEventCuts() {}

StEventCuts::~StEventCuts() {}

int 
StEventCuts::EventSatisfiesCuts(StEvent& ev)
{
  return 1; 
}

void 
StEventCuts::SetMultiplicityCuts(double lowerCut, double upperCut)
{
  upperMult = upperCut;
  lowerMult = lowerCut;

}



