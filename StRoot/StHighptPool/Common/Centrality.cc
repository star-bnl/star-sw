
#include "Centrality.h"

//NOTE:  This is now the same as what is filled in StHiMicroEvent as the Flow Centrality
// definition -- JLK 15.apr.2002

NchCentrality centralityNch(int nCh)
{
  if(nCh>=500) return kFive;
  else if(nCh>=428) return kTen;
  else if(nCh>=311) return kTwenty;
  else if(nCh>=221) return kThirty;
  else if(nCh>=150) return kForty;
  else if(nCh>=98) return kFifty;
  else if(nCh>=59) return kSixty;
  else if(nCh>=33) return kSeventy;
  else if(nCh>=14) return kEighty;
  else return kTotal;

}

