//*-- Author : Jan Balewski
//  
// $Id: StppLPprojectMaker.cxx,v 1.1.1.2 2001/04/21 00:43:13 fisyak Exp $
// $Log: StppLPprojectMaker.cxx,v $
// Revision 1.1.1.2  2001/04/21 00:43:13  fisyak
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

#include "tables/St_tcl_tphit_Table.h" //tmp for CL vs. nPrim
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
  //assert(JspinID);
  if(JspinID) {
    spinID=*JspinID;
  }
  else {
    printf("%s-maker WARN, default value of spinID=%d used\n",GetName(),spinID);
  }
	   
  StEvent *stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
  printf("spinID=%d, eveID=%d\n",spinID,(int)stEvent->id());
     
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
  
 //   G E T   D A T A
 St_DataSet *ds=GetDataSet("tpc_hits"); assert(ds);
 St_tcl_tphit  *tpcl=(St_tcl_tphit  *) ds->Find( "tphit");
 if(tpcl==0) printf("NULL pointer to St_tcl_tphit table\n");
 int nCL=tpcl->GetNRows();
 

 //................................................
 hst[0]->Fill(my->pt); // all input events 

 if(m_Mode==1) {// applay cuts  !!!!!!!!!!!!!!!!!!!!!!!!!!!
   if(my->pt>10. ) return kStOK;// disqualify events with too high rLP PT
   if(my->Rxy>0.3) return kStOK;
   if(my->DRxy>0.015) return kStOK;
   if(fabs(my->Dz)>0.3)  return kStOK;
   printf(" pp LP CUT 1  passed\n");
 }
 
 hst[1]->Fill(my->pt); // all input events 
 hst[2]->Fill(spinID);
 ((TH2F *)hst[3])->Fill(my->nPrim,nCL/1000.);
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
  cout <<" Finish fffffffffffffffff ::"<<GetName() <<endl;
  return  kStOK;
}








