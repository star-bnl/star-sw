// $Id: St_TLA_Maker.cxx,v 1.2 1998/07/20 15:08:15 fisyak Exp $
// $Log: St_TLA_Maker.cxx,v $
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
#include "St_DataSet.h"
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
 if (m_DataSet) delete m_DataSet;
 m_DataSet = 0;
}
//_____________________________________________________________________________
void St_TLA_Maker::Clear(Option_t *option){
  if (m_DataSet) {delete m_DataSet; m_DataSet = 0;}
}

//_____________________________________________________________________________
void St_TLA_Maker::Finish(){ 
 Clear();
}
//_____________________________________________________________________________
void St_TLA_Maker::Init(){
// Create tables
// Create Histograms    
   StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_TLA_Maker::Make(){
  //  PrintInfo();
 return kSTAFCV_OK;
}
//_____________________________________________________________________________
void St_TLA_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_TLA_Maker.cxx,v 1.2 1998/07/20 15:08:15 fisyak Exp $    *\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

