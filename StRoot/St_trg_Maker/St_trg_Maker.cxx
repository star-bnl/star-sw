// $Id: St_trg_Maker.cxx,v 1.4 1999/07/11 19:32:42 druss Exp $
// $Log: St_trg_Maker.cxx,v $
// Revision 1.4  1999/07/11 19:32:42  druss
// Change DST table from dst_TriggerDetectors to dst_TrgDet.
// The classname can only be 19 letters long.
//
// also put the m_Dataset->add line before the PAM is called.
// SPIROS is checking in the change to the actual idl file (part of global)
//
// Revision 1.3  1999/06/24 18:02:46  druss
// add a line to make sure that the table exists...
//
// Revision 1.2  1999/03/14 00:25:40  perev
// New makers
//
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
St_trg_Maker::St_trg_Maker(const char *name):StMaker(name){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_trg_Maker::~St_trg_Maker(){
}
//_____________________________________________________________________________
Int_t St_trg_Maker::Init(){
// Create tables
// Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_trg_Maker::Make(){

// Get CTB and MWC Tables
  St_DataSet *ctf = GetInputDS("ctf");
  St_DataSet *mwc = GetInputDS("mwc");
  if (!ctf || !mwc) return kStWarn;
  St_ctu_cor   *ctu_cor  = (St_ctu_cor   *) ctf->Find("ctb_cor");
  St_mwc_raw   *mwc_raw  = (St_mwc_raw   *) mwc->Find("raw"    );
  if (!ctu_cor ||  !mwc_raw) return kStWarn;

// Create 1 row table dst add it to the dataset and run the PAM
  St_dst_TrgDet *dst = new St_dst_TrgDet("TrgDet",1);
  if (!dst) return kStWarn;
  m_DataSet->Add(dst);
  Int_t Res = trg_fillDst (ctu_cor,mwc_raw,dst);
  if (Res != kSTAFCV_OK) return kStWarn;

  return kStOK;
}
//_____________________________________________________________________________
void St_trg_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_trg_Maker.cxx,v 1.4 1999/07/11 19:32:42 druss Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
