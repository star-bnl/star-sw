// $Id: St_geom_Maker.cxx,v 1.1 1998/10/31 00:28:31 fisyak Exp $
// $Log: St_geom_Maker.cxx,v $
// Revision 1.1  1998/10/31 00:28:31  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
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
// St_geom_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_geom_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
ClassImp(St_geom_Maker)

//_____________________________________________________________________________
St_geom_Maker::St_geom_Maker(const char *name, const char *title):
StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_geom_Maker::~St_geom_Maker(){
}
//_____________________________________________________________________________
Int_t St_geom_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_geom_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
}
 return kStOK;
}
//_____________________________________________________________________________
void St_geom_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_geom_Maker.cxx,v 1.1 1998/10/31 00:28:31 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}


