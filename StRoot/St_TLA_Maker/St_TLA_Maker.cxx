// $Id: St_TLA_Maker.cxx,v 1.9 1999/03/11 03:33:16 perev Exp $
// $Log: St_TLA_Maker.cxx,v $
// Revision 1.9  1999/03/11 03:33:16  perev
// new schema
//
// Revision 1.8  1999/03/10 15:02:07  fine
// HTML link to STAR problem report form has been introduced
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
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
// St_TLA_Maker class for Makers                                        //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include "St_TLA_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
ClassImp(St_TLA_Maker)

//_____________________________________________________________________________
St_TLA_Maker::St_TLA_Maker(const char *name, const char *title):StMaker(name,title){
}
//_____________________________________________________________________________
St_TLA_Maker::~St_TLA_Maker(){
}
//_____________________________________________________________________________
Int_t St_TLA_Maker::Init(){
// Create tables
   St_DataSetIter       local(GetDataBase("params"));
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_TLA_Maker::Make(){
//  PrintInfo();

 return kStOK;
}
//_____________________________________________________________________________
void St_TLA_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_TLA_Maker.cxx,v 1.9 1999/03/11 03:33:16 perev Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

