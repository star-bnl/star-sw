//*-- Author : Jan Balewski
//  
// $Id: StppLPprojectMaker.cxx,v 1.2 2001/02/28 19:06:12 balewski Exp $
// $Log: StppLPprojectMaker.cxx,v $
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Projects events in tho phi/pT bins depending on spin bits            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StppLPprojectMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "StppMiniDst.h" 

ClassImp(StppLPprojectMaker)

//_____________________________________________________________________________
StppLPprojectMaker::StppLPprojectMaker(const char *name):StMaker(name){
 cout <<" Cccccccccccccccccccccccccccccccccccc construct::"<<GetName() <<endl;
 }
//_____________________________________________________________________________
StppLPprojectMaker::~StppLPprojectMaker(){

}
//_____________________________________________________________________________
Int_t StppLPprojectMaker::Init(){
  cout <<" Iiiiiiiiiiiiiiiiiiiiiiiiiiiii init ::"<<GetName() <<endl;
  init_histo();
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StppLPprojectMaker::Make(){
  cout <<" Mmmmmmmmmmmmmmmmmmmmmm   start maker ::"<<GetName() <<" mode="<<m_Mode<<endl;
  
  StEvent *stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
  StppMiniDst *my=StppMiniDst::GetppMiniDst(this); assert(my); 
  printf("ppMiniDst back: pT=%f polDir=%d\n",my->rLP.pt,my->polDir);
  if(my->rLP.pt<0) return kStOK; //not valid event
  
  //................................................
  hst[0]->Fill(my->gLP.pt); // all input events vs. gPT

  // project PHI-distributios
  hpol[my->polDir]->Fill(my->rLP.psi);
  hpol[4+my->polDir]->Fill(my->rLP.psi);
  hpol[8+my->polDir]->Fill(my->rLP.psi);
   

  if(m_Mode==1) {// applay cuts
    if(my->rLP.pt>10. ) return kStOK;// disqualify events with too high rLP PT
    if(my->rLP.Rxy>0.3) return kStOK;
    if(my->rLP.DRxy>0.015) return kStOK;
    if(fabs(my->rLP.Dz)>0.3)  return kStOK;
    printf(" ppCUT1  passed\n");
  }

  hst[1]->Fill(my->gLP.pt); // only accepted events vs. gPT

  //......................... upadate matching quality
   
  float bin=my->gLP.good*10 -1; //(9 or 19) all what was accepted    
  if(fabs(my->rLP.pt-1.5)<0.5) hst[3]->Fill(bin);
  if(fabs(my->rLP.pt-3.5)<0.5) hst[4]->Fill(bin);
  if(fabs(my->rLP.pt-5.5)<0.5) hst[5]->Fill(bin);
  
  float bin1=(my->gLP.good-1)*10+my->gLP.match;  //(0-4 or 10 -14) the highest match  
  if(fabs(my->rLP.pt-1.5)<0.5) hst[3]->Fill(bin1);
  if(fabs(my->rLP.pt-3.5)<0.5) hst[4]->Fill(bin1);
  if(fabs(my->rLP.pt-5.5)<0.5) hst[5]->Fill(bin1);
   

  if(fabs(my->rLP.pt-1.5)<0.5)  { // tune for this pT-bin
    hm[0+my->gLP.good]->Fill(my->rLP.nTclHit); // No. of points on track
    hm[2+my->gLP.good]->Fill(my->rLP.chi2f); // chi2/free
    hm[4+my->gLP.good]->Fill(my->rvert.nPrim); // vertex multiplicity
    hm[6+my->gLP.good]->Fill(my->rLP.Dz);
    hm[8+my->gLP.good]->Fill(my->rLP.DRxy);
    hm[10+my->gLP.good]->Fill(my->rLP.Rxy);
    hm[12+my->gLP.good]->Fill(my->rLP.pt-my->gLP.pt);
    hm[14+my->gLP.good]->Fill(my->gLP.Dpsi);
    printf("rPT12 =%f  gPT=%f  r-g=%f, good=%d eveID=%d\n",my->rLP.pt,my->gLP.pt,my->rLP.pt-my->gLP.pt,my->gLP.good,(int)stEvent->id());
  }


  printf("rPT=%f, match=%d, good=%d\n",my->rLP.pt,my->gLP.match,my->gLP.good);

 return kStOK;
}

//_____________________________________________________________________________
Int_t StppLPprojectMaker::Finish()
{
  cout <<" Finish fffffffffffffffff ::"<<GetName() <<endl;
  return  kStOK;
}








