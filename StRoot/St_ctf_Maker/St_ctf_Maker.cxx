// $Id: St_ctf_Maker.cxx,v 1.6 1999/02/23 01:09:47 fisyak Exp $
// $Log: St_ctf_Maker.cxx,v $
// Revision 1.6  1999/02/23 01:09:47  fisyak
// Add Bill Llope dst tables
//
// Revision 1.5  1999/02/06 00:15:46  fisyak
// Add adc/tdc histograms
//
// Revision 1.4  1999/01/25 23:39:12  fisyak
// Add tof
//
// Revision 1.3  1999/01/21 00:52:31  fisyak
// Cleanup
//
// Revision 1.2  1999/01/02 19:08:14  fisyak
// Add ctf
//
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
#include "ctf/St_fill_dst_tof_Module.h"
#include "TH1.h"

ClassImp(St_ctf_Maker)

//_____________________________________________________________________________
St_ctf_Maker::St_ctf_Maker(const char *name, const char *title):StMaker(name,title),
m_ctb(0),
m_ctb_slat_phi(0),
m_ctb_slat_eta(0),
m_ctb_slat(0),
m_cts_ctb(0),
m_tof(0),
m_tof_slat_phi(0),
m_tof_slat_eta(0),
m_tof_slat(0),
m_cts_tof(0)
{
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_ctf_Maker::~St_ctf_Maker(){
}
//_____________________________________________________________________________
Int_t St_ctf_Maker::Init(){
  // Create tables
  St_DataSetIter       params(gStChain->DataSet("params"));
  m_ctb          = (St_ctg_geo      *) params("ctf/ctg/ctb");
  m_ctb_slat_phi = (St_ctg_slat_phi *) params("ctf/ctg/ctb_slat_phi");
  m_ctb_slat_eta = (St_ctg_slat_eta *) params("ctf/ctg/ctb_slat_eta");
  m_ctb_slat     = (St_ctg_slat     *) params("ctf/ctg/ctb_slat");
  Int_t Res_ctg_ctb  =  ctg (m_ctb,m_ctb_slat_phi,m_ctb_slat_eta,m_ctb_slat);
  m_tof          = (St_ctg_geo      *) params("ctf/ctg/tof");
  m_tof_slat_phi = (St_ctg_slat_phi *) params("ctf/ctg/tof_slat_phi");
  m_tof_slat_eta = (St_ctg_slat_eta *) params("ctf/ctg/tof_slat_eta");
  m_tof_slat     = (St_ctg_slat     *) params("ctf/ctg/tof_slat");
  Int_t Res_ctg_tof  =  ctg (m_tof,m_tof_slat_phi,m_tof_slat_eta,m_tof_slat);
  // Special treatment for double names
  //  m_cts          = (St_cts_mpara    *) params("ctf/cts")->GetList()->FindObject("cts");
  m_cts_ctb          = (St_cts_mpara    *) params("ctf/cts/cts_ctb");
  m_cts_tof          = (St_cts_mpara    *) params("ctf/cts/cts_tof");
  // Create Histograms  
  m_adc              = new TH1F("abc","CTB ADC counters",100,0,100);
  m_tdc              = new TH1F("tdc","CTB TDC counters",100,0,100);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_ctf_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    St_DataSetIter geant(gStChain->DataSet("geant"));
    St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
    St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) geant("g2t_ctb_hit");
    if (g2t_ctb_hit) {
      St_cts_mslat *ctb_mslat = new  St_cts_mslat("ctb_mslat", 240); m_DataSet->Add(ctb_mslat);
      St_cts_event *ctb_event = new  St_cts_event("ctb_event",5000); m_DataSet->Add(ctb_event);
      St_ctu_raw   *ctb_raw   = new  St_ctu_raw("ctb_raw",     240); m_DataSet->Add(ctb_raw);
      St_ctu_cor   *ctb_cor   = new  St_ctu_cor("ctb_cor",     240); m_DataSet->Add(ctb_cor);
      Int_t Res_cts_ctb = cts(g2t_ctb_hit, g2t_track,
			      m_ctb,  m_ctb_slat, m_ctb_slat_phi, m_ctb_slat_eta, m_cts_ctb,
			      ctb_event, ctb_mslat, ctb_raw);
      
      Int_t Res_ctu_ctb =  ctu(m_ctb,  m_ctb_slat,
			       ctb_raw, ctb_cor);
      ctu_raw_st *raw = ctb_raw->GetTable();
      for (Int_t i=0; i<ctb_raw->GetNRows();i++,raw++){
	m_adc->Fill((Float_t) raw->adc);
	m_tdc->Fill((Float_t) raw->tdc);
      }
    }
    St_g2t_ctf_hit *g2t_tof_hit = (St_g2t_ctf_hit *) geant("g2t_tof_hit");
    if (g2t_tof_hit) {
      St_cts_mslat *tof_mslat = new  St_cts_mslat("tof_mslat",5400); m_DataSet->Add(tof_mslat);
      St_cts_event *tof_event = new  St_cts_event("tof_event",5000); m_DataSet->Add(tof_event);
      St_ctu_raw   *tof_raw   = new  St_ctu_raw("tof_raw",    5400); m_DataSet->Add(tof_raw);
      St_ctu_cor   *tof_cor   = new  St_ctu_cor("tof_cor",    5400); m_DataSet->Add(tof_cor);
      Int_t Res_cts_tof = cts(g2t_tof_hit, g2t_track,
			      m_tof,  m_tof_slat, m_tof_slat_phi, m_tof_slat_eta, m_cts_tof,
			      tof_event, tof_mslat, tof_raw);
      St_DataSet *tpc_tracks = gStChain->DataSet("tpc_tracks");
      St_tpt_track  *tptrack = 0;
      St_tte_mctrk  *mctrk   = 0;
      if (tpc_tracks) {
	St_DataSetIter tpcI(tpc_tracks);
	tptrack = (St_tpt_track *) tpcI["tptrack"];
	mctrk   = (St_tte_mctrk *) tpcI["mctrk"];
      }
      St_DataSet *global = gStChain->DataSet("global");
      St_dst_vertex     *vertex      = 0;
      if (global) {
	St_DataSetIter globalI(global);
	vertex  = (St_dst_vertex *) globalI["dst/vertex"];
      }
      if (g2t_track && tptrack && mctrk && vertex ) {
	St_dst_tof_trk *dst_tof_trk = new St_dst_tof_trk("dst_tof_trk",1000);
	m_DataSet->Add(dst_tof_trk);
	St_dst_tof_evt *dst_tof_evt = new St_dst_tof_evt("dst_tof_evt",1);
	m_DataSet->Add(dst_tof_evt);
	Int_t Res_fill_dst_tof = fill_dst_tof(g2t_tof_hit,g2t_track,
					      tptrack,mctrk,vertex,
					      m_tof,m_tof_slat,
					      m_tof_slat_phi,m_tof_slat_eta,
					      m_cts_tof,tof_mslat,
					      dst_tof_trk,dst_tof_evt);
      }	 
      Int_t Res_ctu_tof =  ctu(m_tof,  m_tof_slat,
			       tof_raw, tof_cor);
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
void St_ctf_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_ctf_Maker.cxx,v 1.6 1999/02/23 01:09:47 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

