// $Id: St_geant_Maker.cxx,v 1.1 1998/10/12 14:34:18 fisyak Exp $
// $Log: St_geant_Maker.cxx,v $
// Revision 1.1  1998/10/12 14:34:18  fisyak
// new geant Maker
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
// St_geant_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_geant_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
ClassImp(St_geant_Maker)

//_____________________________________________________________________________
St_geant_Maker::St_geant_Maker(){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_geant_Maker::St_geant_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_geant_Maker::~St_geant_Maker(){
  SafeDelete(m_DataSet); 
}
//_____________________________________________________________________________
void St_geant_Maker::Clear(Option_t *option){
  SafeDelete(m_DataSet); 
}

//_____________________________________________________________________________
Int_t St_geant_Maker::Finish(){ 
 Clear();
 return kStOK;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->GetParams());
// Create Histograms    
   StMaker::Init();
   return kStOK;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
}
 return kSTAFCV_OK;
}
//_____________________________________________________________________________
void St_geant_Maker::PrintInfo(){
  printf("***************************************************************\n");
  printf("* $Id: St_geant_Maker.cxx,v 1.1 1998/10/12 14:34:18 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("***************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

