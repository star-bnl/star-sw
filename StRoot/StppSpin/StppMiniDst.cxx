//JB
#include "StppMiniDst.h"
#include "StEventTypes.h"
#include "StChain.h"

ClassImp(StppMiniDst)
StppMiniDst :: StppMiniDst ()
{
  CtbAdcSumChan=0;
  gLP.good=-1;
  gvert.z=-999;
  gLP.ng2tHit=-1;
  rLP.nTclHit=-2;
  polDir=voidPol;
  data1=-1;
  data2=-2;
}

//_____________________________________________________________________________
StppMiniDst * StppMiniDst::GetppMiniDst( StMaker *chain){
  printf("read StppMiniDst    . . .\n");
  assert(chain);
  StppMiniDst *my=NULL;
  char *myname="StppMiniDst";
  int i;
  StEvent *stEvent= (StEvent *)  chain->GetInputDS("StEvent");  assert(stEvent);
  StSPtrVecObject &vec   = stEvent->content();
  for (i=0; i<(int)vec.size(); i++){
    //printf("%d dst branch=%s=\n",i, vec[i]->GetName());
    if(strncmp(myname, vec[i]->GetName(), strcspn(myname,""))) continue;
    my=(StppMiniDst *) vec[i];
    //printf(" Found %s.CtbAdcSumChan=%d\n",myname,my->CtbAdcSumChan);
    break;
  }
  //printf("i=%d, add=%d,add2=%d polDir=%d\n",i,(int)my, (int)my0,my->polDir); 
  return my;
}

