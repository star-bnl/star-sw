
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

int flowCentrality(int N)
{

  // 4-June-2002 JLK
  //Updated with Zhangbu's new numbers for P02gc

  int cent[] = {14,30,56,94,146,217,312,431,510};
      if (N < cent[0])       { return 0; }
      else if (N < cent[1])  { return 1; }
      else if (N < cent[2])  { return 2; }
      else if (N < cent[3])  { return 3; }
      else if (N < cent[4])  { return 4; }
      else if (N < cent[5])  { return 5; }
      else if (N < cent[6])  { return 6; }
      else if (N < cent[7])  { return 7; }
      else if (N < cent[8])  { return 8; }
      else                        { return 9; }  
      
}

