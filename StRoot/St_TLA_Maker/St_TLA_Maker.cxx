// $Id: St_TLA_Maker.cxx,v 1.5 1998/10/02 13:46:08 fine Exp $
// $Log: St_TLA_Maker.cxx,v $
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TLA_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_TLA_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
ClassImp(St_TLA_Maker)

//_____________________________________________________________________________
St_TLA_Maker::St_TLA_Maker(){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_TLA_Maker::St_TLA_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_TLA_Maker::~St_TLA_Maker(){
  SafeDelete(m_DataSet); 
}
//_____________________________________________________________________________
void St_TLA_Maker::Clear(Option_t *option){
  SafeDelete(m_DataSet); 
}

//_____________________________________________________________________________
void St_TLA_Maker::Finish(){ 
 Clear();
}
//_____________________________________________________________________________
void St_TLA_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->GetParams());
// Create Histograms    
   StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_TLA_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
}
 return kSTAFCV_OK;
}
//_____________________________________________________________________________
void St_TLA_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_TLA_Maker.cxx,v 1.5 1998/10/02 13:46:08 fine Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

