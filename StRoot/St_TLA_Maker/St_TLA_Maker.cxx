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
  if (gStChain->Debug()) StMaker::PrintInfo();
}

