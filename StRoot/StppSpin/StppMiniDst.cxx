//*-- Author : Jan Balewski
//  
// $Id: StppMiniDst.cxx,v 1.1.1.2 2001/04/21 00:43:14 fisyak Exp $
// $Log: StppMiniDst.cxx,v $
// Revision 1.1.1.2  2001/04/21 00:43:14  fisyak
// *** empty log message ***
//
// Revision 1.3  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//
//  This class is self inserting to the StEvent
//  It is used to store the DST info relevant for the ppSpin analysis 
//                                                                    
//////////////////////////////////////////////////////////////////////////


#include "StppMiniDst.h"
#include "StEventTypes.h"
#include "StChain.h"

ClassImp(StppMiniDst)

StppMiniDst :: StppMiniDst ()
{
  rLP.pt=-777;// flag for non valid data
  rLP.nTclHit=-2;
  gLP.good=-1;
  gLP.vert.z=-999;
  gLP.ng2tHit=-1;
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

  if(my==NULL){
    printf("Instantiate ppMiniDST\n");
    my=new StppMiniDst;
    stEvent->content().push_back(my);   
  }

  return my;
}

#if 0
  
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
void  StppTrigMaker::addSpinTagDst()
{
  printf("add StppSpinTag ..\n");
  // Create a data set and add the table to it.
  St_ppSpinTag *tagtab= new St_ppSpinTag("ppSpinTag",1); 
  m_DataSet->Add(tagtab);
  
  ppSpinTag_st tagrow;
  //fill default values for the ppSPin Tags
  tagrow.chargeLeadingParticlePt=-55.;
  tagtab->AddAt(&tagrow,0);
  // end of tag initialization

}

#endif

