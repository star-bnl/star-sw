// $Id: St_trg_Maker.cxx,v 1.1 1999/02/06 01:51:22 yepes Exp $
// $Log: St_trg_Maker.cxx,v $
// Revision 1.1  1999/02/06 01:51:22  yepes
// Add trg maker
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
// St_trg_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_trg_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "trg/St_trg_fillDst_Module.h"
ClassImp(St_trg_Maker)

//_____________________________________________________________________________
St_trg_Maker::St_trg_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_trg_Maker::~St_trg_Maker(){
}
//_____________________________________________________________________________
Int_t St_trg_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_trg_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    St_DataSet *ctf = gStChain->DataSet("ctf");
    St_DataSet *mwc = gStChain->DataSet("mwc");
    if (ctf && mwc) {
      St_DataSetIter ctff(ctf);
      St_ctu_cor   *ctu_cor   = (St_ctu_cor   *) ctff("ctb_cor");
      St_DataSetIter mwcf(mwc);
      St_mwc_raw    *mwc_raw  = (St_mwc_raw   *) mwcf("raw");
      if (ctu_cor && mwc_raw) {
	St_dst_TriggerDetectors *dst = new St_dst_TriggerDetectors("dst_TriggerDetectors",1);
	m_DataSet->Add(dst);
	Int_t Res = trg_fillDst (ctu_cor,mwc_raw,dst);
      }
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
void St_trg_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_trg_Maker.cxx,v 1.1 1999/02/06 01:51:22 yepes Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

