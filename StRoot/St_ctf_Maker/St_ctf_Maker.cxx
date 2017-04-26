// $Id: St_ctf_Maker.cxx,v 1.18 2017/04/26 20:19:18 perev Exp $
// $Log: St_ctf_Maker.cxx,v $
// Revision 1.18  2017/04/26 20:19:18  perev
// Hide m_DataSet
//
// Revision 1.17  2009/11/10 21:09:59  fisyak
// pams clean up
//
// Revision 1.16  2007/04/28 17:55:47  perev
// Redundant StChain.h removed
//
// Revision 1.15  2003/04/30 20:39:14  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.14  2000/06/26 22:13:06  fisyak
// remove params
//
// Revision 1.13  1999/07/15 13:57:48  perev
// cleanup
//
// Revision 1.12  1999/03/11 03:55:06  perev
// new schema
//
// Revision 1.11  1999/03/02 00:11:49  llope
// reduced table maxlen's for mdc2
//
// Revision 1.10  1999/02/26 14:49:54  kathy
// fixed histogram limits as per Bill's request
//
// Revision 1.9  1999/02/25 21:11:46  kathy
// fix histograms
//
// Revision 1.8  1999/02/25 19:38:50  kathy
// fix up histograms
//
// Revision 1.7  1999/02/23 21:25:42  llope
// fixed histograms, added 1/beta vs p
//
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
#include "St_DataSetIter.h"
#include "ctf/St_ctg_Module.h"
#include "ctf/St_ctg_Module.h"
#include "ctf/St_cts_Module.h"
#include "ctf/St_ctu_Module.h"
#ifdef __TPT__
#include "ctf/St_fill_dst_tof_Module.h"
#endif
#include "TH1.h"
#include "TH2.h"

ClassImp(St_ctf_Maker)

//_____________________________________________________________________________
St_ctf_Maker::St_ctf_Maker(const char *name):StMaker(name),
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
  int iInit=0;

// Create tables
  St_DataSet *ctfg = GetInputDB("ctf/ctg");
  assert (ctfg);
  St_DataSetIter       gime(ctfg);
  m_ctb          = (St_ctg_geo      *) gime("ctb");
  m_ctb_slat_phi = (St_ctg_slat_phi *) gime("ctb_slat_phi");
  m_ctb_slat_eta = (St_ctg_slat_eta *) gime("ctb_slat_eta");
  m_ctb_slat     = (St_ctg_slat     *) gime("ctb_slat");

  Int_t Res_ctg_ctb  =  ctg (m_ctb,m_ctb_slat_phi,m_ctb_slat_eta,m_ctb_slat);
  if (Res_ctg_ctb!=kSTAFCV_OK) iInit = kStWarn;
  m_tof          = (St_ctg_geo      *) gime("tof");
  m_tof_slat_phi = (St_ctg_slat_phi *) gime("tof_slat_phi");
  m_tof_slat_eta = (St_ctg_slat_eta *) gime("tof_slat_eta");
  m_tof_slat     = (St_ctg_slat     *) gime("tof_slat");

  Int_t Res_ctg_tof  =  ctg (m_tof,m_tof_slat_phi,m_tof_slat_eta,m_tof_slat); if(Res_ctg_tof){};
  // Special treatment for double names
  //  m_cts          = (St_cts_mpara    *) params("ctf/cts")->GetList()->FindObject("cts");
  St_DataSet *ctfs = GetInputDB("ctf/cts");
  assert (ctfs);
  gime.Reset(ctfs);
  m_cts_ctb          = (St_cts_mpara    *) gime("cts_ctb");
  m_cts_tof          = (St_cts_mpara    *) gime("cts_tof");
  // Create Histograms  
  m_adcc  = new TH1F("CtfCtbrawAdc","CTB ADCs",128,0.,1024.);
  m_adct  = new TH1F("CtfTofrawAdc","TOF ADCs",128,0.,2048.);
  m_tsvsp = new TH2F("CtfDsttoftrkTsvsp","TOF 1/beta vs. Ptrk",80,0.1,4.1,100,0.,10.);
  m_tsvsp->SetXTitle("TPC track momentum (GeV/c)");
  m_tsvsp->SetYTitle("1/beta from TOFp");
  m_tsvsp1= new TH2F("CtfDsttoftrkTsvsp1","TOF 1/beta vs. Ptrk",80,0.1,4.1,100,0.,10.);
  m_tsvsp1->SetXTitle("TPC track momentum (GeV/c)");
  m_tsvsp1->SetYTitle("1/beta from TOFp, slat Nhits=1");
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_ctf_Maker::Make(){
//  PrintInfo();

  int iMake=kStOK;
  
  St_DataSet *gea = GetInputDS("geant");
  if (gea) {//		Geant input exists
    St_DataSetIter geant(gea);
    St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
    St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) geant("g2t_ctb_hit");
    if (g2t_ctb_hit) {
      St_cts_mslat *ctb_mslat = new  St_cts_mslat("ctb_mslat", 240); AddData(ctb_mslat);
      St_cts_event *ctb_event = new  St_cts_event("ctb_event",   1); AddData(ctb_event);
      St_ctu_raw   *ctb_raw   = new  St_ctu_raw("ctb_raw",     240); AddData(ctb_raw);
      St_ctu_cor   *ctb_cor   = new  St_ctu_cor("ctb_cor",     240); AddData(ctb_cor);
      Int_t Res_cts_ctb = cts(g2t_ctb_hit, g2t_track,
			      m_ctb,  m_ctb_slat, m_ctb_slat_phi, m_ctb_slat_eta, m_cts_ctb,
			      ctb_event, ctb_mslat, ctb_raw); if(Res_cts_ctb){};
      Int_t Res_ctu_ctb = ctu(m_ctb,  m_ctb_slat, ctb_raw, ctb_cor);if(Res_ctu_ctb){};
      ctu_raw_st *raw   = ctb_raw->GetTable();
      for (Int_t i=0; i<ctb_raw->GetNRows();i++,raw++){
        m_adcc->Fill((Float_t) raw->adc);
      }
    }

    St_g2t_ctf_hit *g2t_tof_hit = (St_g2t_ctf_hit *) geant("g2t_tof_hit");
    if (g2t_tof_hit) {
      St_cts_mslat *tof_mslat = new  St_cts_mslat("tof_mslat",500); AddData(tof_mslat); //safe factor of 5 for mdc2
      St_cts_event *tof_event = new  St_cts_event("tof_event",  1); AddData(tof_event);
      St_ctu_raw   *tof_raw   = new  St_ctu_raw("tof_raw",     50); AddData(tof_raw);
      St_ctu_cor   *tof_cor   = new  St_ctu_cor("tof_cor",     50); AddData(tof_cor); 
      Int_t Res_cts_tof = cts(g2t_tof_hit, g2t_track,
			      m_tof,  m_tof_slat, m_tof_slat_phi, m_tof_slat_eta, m_cts_tof,
			      tof_event, tof_mslat, tof_raw);if(Res_cts_tof){}
#ifdef __TPT__
      St_DataSet *tpc_tracks = GetDataSet("tpc_tracks");
      St_tpt_track  *tptrack = 0;
      St_tte_mctrk  *mctrk   = 0;
      if (tpc_tracks) {
         St_DataSetIter tpcI(tpc_tracks);
         tptrack = (St_tpt_track *) tpcI["tptrack"];
         mctrk   = (St_tte_mctrk *) tpcI["mctrk"];
      }
      St_DataSet *global = GetDataSet("global");
      St_dst_vertex     *vertex      = 0;
      if (global) {
         St_DataSetIter globalI(global);
         vertex  = (St_dst_vertex *) globalI["dst/vertex"];
      }
      if (g2t_track && tptrack && mctrk && vertex ) {
         St_dst_tof_trk *dst_tof_trk = new St_dst_tof_trk("dst_tof_trk",250); //safe factor of 5 for mdc2
         AddData(dst_tof_trk);
         St_dst_tof_evt *dst_tof_evt = new St_dst_tof_evt("dst_tof_evt",1);
         AddData(dst_tof_evt);
         Int_t Res_fill_dst_tof = fill_dst_tof(g2t_tof_hit,g2t_track,
					      tptrack,mctrk,vertex,
					      m_tof,m_tof_slat,
					      m_tof_slat_phi,m_tof_slat_eta,
					      m_cts_tof,tof_mslat,
					      dst_tof_trk,dst_tof_evt); if(Res_fill_dst_tof){};
         dst_tof_trk_st *dst = dst_tof_trk->GetTable();
         for (Int_t i=0; i<dst_tof_trk->GetNRows();i++,dst++){
           m_tsvsp->Fill(dst->ptot,dst->ts_mtime);
           if (dst->n_hits == 1) {
            m_tsvsp1->Fill(dst->ptot,dst->ts_mtime);
           }
         }
      }	 
      Int_t Res_ctu_tof = ctu(m_tof,  m_tof_slat, tof_raw, tof_cor);if(Res_ctu_tof){}
      ctu_raw_st *raw   = tof_raw->GetTable();
      for (Int_t i=0; i<tof_raw->GetNRows();i++,raw++){
        m_adct->Fill((Float_t) raw->adc);
      }
#endif
    }
  }
  return iMake;
  
}
//_____________________________________________________________________________

