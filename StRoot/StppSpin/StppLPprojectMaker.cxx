//*-- Author : Jan Balewski
//  
// $Id: StppLPprojectMaker.cxx,v 1.9 2001/11/28 23:03:42 balewski Exp $
// $Log: StppLPprojectMaker.cxx,v $
// Revision 1.9  2001/11/28 23:03:42  balewski
// ppLMV uses only tracks matched to CTB slats, runs with DAQ & MC data
//
// Revision 1.8  2001/05/08 03:24:51  balewski
// *** empty log message ***
//
// Revision 1.7  2001/05/04 20:29:36  balewski
// *** empty log message ***
//
// Revision 1.6  2001/04/27 20:50:45  balewski
// *** empty log message ***
//
// Revision 1.5  2001/04/19 21:30:36  balewski
// add I/O to ppDst
//
// Revision 1.4  2001/04/19 15:33:19  balewski
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
//                                                                      //
// Projects events in tho phi/pT bins depending on spin bits            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StppLPprojectMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "StppDst.h" 

#include "TH2.h"

#include "StTreeMaker/StTreeMaker.h"

ClassImp(StppLPprojectMaker)


//_____________________________________________________________________________
StppLPprojectMaker::StppLPprojectMaker(const char *name):StMaker(name){
  JspinID=NULL;
 cout <<" Cccccccccccccccccccccccccccccccccccc construct::"<<GetName() <<endl;
 }
//_____________________________________________________________________________
StppLPprojectMaker::~StppLPprojectMaker(){
  printf("%s-Destructor ddddddddddddddddd\n",GetName());
  float *p[MxSpinID];
  float tot[MxSpinID];

  int is,ib;
  for(is=0;is<MxSpinID;is++) {
    //printf("is=%d, add=%d\n",is, hpol[is]);
    p[is]=((TH1F*) hpol[is])->GetArray();
    assert(p[is]);
    tot[is]=0;
  }
  int nb=((TH1F*) hpol[0])->GetNbinsX();
  
  printf("content of spin sorted %d-bin phi distributions\n",nb);
  printf("bin spinID");
  for(is=0;is<MxSpinID;is++) printf("=[%2d]     ",is);
  printf("\n");
  
  for(ib=1;ib<=nb;ib++) {
    printf("%3d ",ib);
    for(is=0;is<MxSpinID;is++) {
      tot[is]+=p[is][ib];
      printf("  %8.0f",p[is][ib]);
    }
    printf("\n");
  }
  
  printf("total=");
  for(is=0;is<MxSpinID;is++) printf("%8.0f  ",tot[is]);
  printf("\n %s-maker finisfed\n",GetName());


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

  int spinID=1; // default value

  if(JspinID) {
    spinID=*JspinID;
  }
  else {
    printf("%s-maker WARN, default value of spinID=%d used\n",GetName(),spinID);
  }


  //StEvent *stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
     
  //printf("spinID=%d, eveID=%d\n",spinID,(int)stEvent->id());
  printf("spinID=%d\n",spinID);
  
  St_DataSet* ds1=GetDataSet("dst/rec_lp"); // assert(ds1);
  if(ds1==NULL) {
    printf("%s-maker, no dst/rec_lp, ignore eve\n",GetName());
    return kStOk;
  }

  ppDst *myR=(ppDst *)ds1->Find("rec_lp"); //assert(myR);
  if(myR==NULL) {
    printf("%s-maker, no rec_lp, ignore eve\n",GetName());
    return kStOk;
  }

  assert(myR->GetNRows()==1);
  ppDst_t *my=(ppDst_t *)myR->GetArray();

  printf("ppDst found: rec pT=%f \n",my->pt);
  if(my->pt<0) return kStOK; //not valid event


 //................................................
 hst[0]->Fill(my->pt); // all input events 
 if(fabs(my->pt-0.5)<0.5 )hst[5]->Fill(my->psi);
 if(fabs(my->pt-1.5)<0.5 )hst[6]->Fill(my->psi);
 if(fabs(my->pt-2.5)<0.5 )hst[7]->Fill(my->psi);
 if(fabs(my->pt-3.5)<0.5 )hst[8]->Fill(my->psi);

 if(m_Mode==1) {// applay cuts  !!!!!!!!!!!!!!!!!!!!!!!!!!!
   if(my->pt>10. ) return kStOK;// disqualify events with too high rLP PT
   if(my->Rxy>0.3) return kStOK;
   if(my->DRxy>0.015) return kStOK;
   if(fabs(my->Dz)>0.3)  return kStOK;
   printf(" pp LP CUT 1  passed\n");
 }
 
 hst[1]->Fill(my->pt); // all input events 
 hst[2]->Fill(spinID);
 hst[4]->Fill(my->psi);

 
 // check validity of spinID
 assert(spinID>=0);
 assert(spinID<MxSpinID);
 
 // spin-sorted  PHI-distributios
 hpol[spinID]->Fill(my->psi);

 return kStOK;
}

//_____________________________________________________________________________
Int_t StppLPprojectMaker::Finish()
{

  //printf("%s-Finish ffffffffffffffffff\n",GetName());

  return  kStOK;
}








