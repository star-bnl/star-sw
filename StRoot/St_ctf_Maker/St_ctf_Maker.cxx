// $Id: St_ctf_Maker.cxx,v 1.1 1999/01/01 02:39:38 fisyak Exp $
// $Log: St_ctf_Maker.cxx,v $
// Revision 1.1  1999/01/01 02:39:38  fisyak
// Add ctf Maker
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
// St_ctf_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_ctf_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "ctf/St_ctg_Module.h"
#include "ctf/St_ctg_Module.h"
#include "ctf/St_cts_Module.h"
#include "ctf/St_ctu_Module.h"

ClassImp(St_ctf_Maker)

//_____________________________________________________________________________
St_ctf_Maker::St_ctf_Maker(const char *name, const char *title):StMaker(name,title),
m_ctb(0),
m_ctb_slat_phi(0),
m_ctb_slat_eta(0),
m_ctb_slat(0),
m_cts(0)
{
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_ctf_Maker::~St_ctf_Maker(){
}
//_____________________________________________________________________________
Int_t St_ctf_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));
   m_ctb          = (St_ctg_geo      *) local("ctf/ctg/ctb");
   m_ctb_slat_phi = (St_ctg_slat_phi *) local("ctf/ctg/ctb_slat_phi");
   m_ctb_slat_eta = (St_ctg_slat_eta *) local("ctf/ctg/ctb_slat_eta");
   m_ctb_slat     = (St_ctg_slat     *) local("ctf/ctg/ctb_slat");
   Int_t Res_ctg  =  ctg (m_ctb,m_ctb_slat_phi,m_ctb_slat_eta,m_ctb_slat);
   m_cts          = (St_cts_mpara    *) local("ctf/cts/cts");
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_ctf_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
  St_cts_mslat *ctb_mslat = new  St_cts_mslat("ctb_mslat",240); m_DataSet->Add(ctb_mslat);
  St_cts_event *ctb_event = new  St_cts_event("ctb_event",500); m_DataSet->Add(ctb_event);
  St_ctu_raw   *ctb_raw   = new  St_ctu_raw("ctb_raw",240);     m_DataSet->Add(ctb_raw);
  St_ctu_cor   *ctb_cor   = new  St_ctu_cor("ctb_cor",240);     m_DataSet->Add(ctb_cor);
  St_DataSetIter geant(gStChain->DataSet("geant"));
  St_g2t_ctf_hit *g2t_ctf_hit = (St_g2t_ctf_hit *) geant("g2t_ctf_hit");
  St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
  Int_t Res_cts = cts(g2t_ctf_hit, g2t_track,
		      m_ctb,  m_ctb_slat, m_ctb_slat_phi, m_ctb_slat_eta, m_cts,
		      ctb_event, ctb_mslat, ctb_raw);
 
  Int_t Res_ctu =  ctu(m_ctb,  m_ctb_slat,
		       ctb_raw, ctb_cor);

  }
 return kStOK;
}
//_____________________________________________________________________________
void St_ctf_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_ctf_Maker.cxx,v 1.1 1999/01/01 02:39:38 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

