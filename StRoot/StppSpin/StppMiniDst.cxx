//*-- Author : Jan Balewski
//  
// $Id: StppMiniDst.cxx,v 1.2 2001/02/28 19:06:12 balewski Exp $
// $Log: StppMiniDst.cxx,v $
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  Store the DST info relevant for the ppSpin analysis                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include "StppMiniDst.h"
#include "StEventTypes.h"
#include "StChain.h"

ClassImp(StppMiniDst)
StppMiniDst :: StppMiniDst ()
{
  rLP.pt=-777;// flag for non valid data
  rLP.nTclHit=-2;
  CtbAdcSumChan=0;
  Tslat=0; //not a valid value
  gLP.good=-1;
  gvert.z=-999;
  gLP.ng2tHit=-1;
  polDir=voidPol;
  data1=-1;
  data2=-2;
}

//_____________________________________________________________________________
StppMiniDst * StppMiniDst::GetppMiniDst( StMaker *chain){
  printf("read StppMiniDst    . . .");
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
  if(my)  printf("OK\n");

  //printf("i=%d, add=%d,add2=%d polDir=%d\n",i,(int)my, (int)my0,my->polDir); 
  return my;
}

