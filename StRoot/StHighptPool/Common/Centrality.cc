
#include "Centrality.h"

//NOTE:  This is now the same as what is filled in StHiMicroEvent as the Flow Centrality
// definition -- JLK 15.apr.2002


NchCentrality centralityNch(int nCh)
{

  // 4-June-2002 JLK
  //Updated with Zhangbu's new numbers for P02gc

  if(nCh>=510) return kFive;
  else if(nCh>=431) return kTen;
  else if(nCh>=312) return kTwenty;
  else if(nCh>=217) return kThirty;
  else if(nCh>=146) return kForty;
  else if(nCh>=94) return kFifty;
  else if(nCh>=56) return kSixty;
  else if(nCh>=30) return kSeventy;
  else if(nCh>=14) return kEighty;
  else return kTotal;

}

