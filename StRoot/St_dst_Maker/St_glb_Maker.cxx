// $Id: St_glb_Maker.cxx,v 1.29 1999/02/17 23:58:11 caines Exp $
// $Log: St_glb_Maker.cxx,v $
// Revision 1.29  1999/02/17 23:58:11  caines
// changed ev0 cuts
//
// Revision 1.28  1999/02/17 20:50:57  fisyak
// reduce no. of reconstructed tracks/verteces from 100K to 20K
//
// Revision 1.27  1999/02/16 21:34:01  caines
//  Added exi back in
//
// Revision 1.26  1999/02/16 03:03:46  fisyak
// Split Make and Histograms
//
// Revision 1.25  1999/02/14 18:38:22  caines
//  Fixed ev0 bugs in hist
//
// Revision 1.24  1999/02/13 20:22:31  caines
// Added exi and temp dir for when svt not there
//
// Revision 1.23  1999/02/12 22:27:35  ogilvie
// added in spectra/pid QA histograms
//
// Revision 1.22  1999/02/12 19:23:46  didenko
// updated v0 finding code from Helen
//
// Revision 1.21  1999/02/11 02:53:37  fisyak
// Janet update to FTPC dst table
//
// Revision 1.20  1999/02/05 17:58:18  fisyak
// Spiros correction to evr
//
// Revision 1.19  1999/01/28 17:09:59  fisyak
// Add ftpc to software monitor
//
// Revision 1.18  1999/01/20 23:58:03  fisyak
// Tree 2 GetTree
//
// Revision 1.17  1999/01/02 19:08:17  fisyak
// Add ctf
//
// Revision 1.16  1998/12/21 19:41:50  fisyak
// Move dst 2 glb
//
// Revision 1.15  1998/12/21 19:26:08  fisyak
// Make ROOT include non system
//
// Revision 1.14  1998/12/17 14:37:19  fisyak
// Fix tp_param
//
// Revision 1.13  1998/12/16 22:22:40  fisyak
// New global from Spiros
//
// Revision 1.12  1998/12/12 02:37:53  fisyak
// fix evr
//
// Revision 1.11  1998/12/01 02:02:38  fisyak
// fix run_summary_param
//
// Revision 1.10  1998/11/25 21:58:24  fisyak
// Cleanup
//
// Revision 1.9  1998/11/12 23:38:36  fisyak
// Account new g2t
//
// Revision 1.8  1998/11/01 16:42:27  fisyak
// dst analysis
//
// Revision 1.7  1998/10/31 00:26:12  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:34  perev
// cleanup
//
// Revision 1.5  1998/09/23 20:22:54  fisyak
// Prerelease SL98h
//
// Revision 1.4  1998/09/15 20:55:20  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.3  1998/09/08 22:43:10  fisyak
// Modify St_glb_Maker to account new calling sequence
//
// Revision 1.2  1998/08/26 12:15:08  fisyak
// Remove asu & dsl libraries
//
// Revision 1.1  1998/08/18 14:06:06  fisyak
// Add to bfc dst
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_glb_Maker class for Makers (evr + egr + ev0 + ev0_eval)           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "PhysicalConstants.h"
#include "TMath.h"
#include "St_glb_Maker.h"
#include "St_particle_Table.h"
#include "St_hepe_gent_Table.h"

#include "StChain.h"
#include "St_DataSetIter.h"

#include "global/St_svm_am_Module.h"
#include "global/St_svm_eval2_Module.h"
#include "global/St_svm_svt_eval_Module.h"
#include "global/St_svm_efficiency_Module.h"

#include "global/St_evr_am_Module.h"
#include "global/St_egr_fitter_Module.h"
#include "global/St_track_propagator_Module.h"
#include "global/St_ev0_am2_Module.h"
#include "global/St_ev0_eval2_Module.h"
#include "global/St_exiam_Module.h"
#include "global/St_dst_dedx_filler_Module.h"
#include "global/St_fill_ftpc_dst_Module.h"
#include "global/St_dst_monitor_soft_filler_Module.h"

#include "global/St_particle_dst_filler_Module.h"
#include "global/St_dst_point_filler_Module.h"
#include "global/St_fill_dst_event_summary_Module.h"

#include "St_dst_tof_Table.h"

const Int_t St_glb_Maker::nxpT = 50;
const Int_t St_glb_Maker::nyeta = 50;
const Float_t St_glb_Maker::xminpT = 0.0;
const Float_t St_glb_Maker::xmaxpT = 5.0;
const Float_t St_glb_Maker::ymineta = -2.0;
const Float_t St_glb_Maker::ymaxeta =  2.0;

ClassImp(St_glb_Maker)

//_____________________________________________________________________________
St_glb_Maker::St_glb_Maker(const char *name, const char *title):StMaker(name,title),
m_svm_ctrl(0),
m_evr_evrpar(0),
m_ev0par(0),
m_magf(0),
m_egr_egrpar(0),
m_particle_dst_param(0)

{
  drawinit=kFALSE;
  m_scenario  = 8;
  m_svtchicut = 0;
  m_useglobal = 2;
  m_usesvt    = 1;
  m_usetpc    = 1;
  m_usevert   = 0;
  m_flag      = 2;
}
//_____________________________________________________________________________
St_glb_Maker::~St_glb_Maker(){
}
//_____________________________________________________________________________
Int_t St_glb_Maker::Init(){
  // Create tables
  St_DataSetIter params(gStChain->DataSet("params"));
  //svm
  m_svm_ctrl   = (St_svm_ctrl *)   params("global/svmpars/svm_ctrl");
  //egr 
  m_egr_egrpar = (St_egr_egrpar *) params("global/egrpars/egr_egrpar");
  egr_egrpar_st *egr_egrpar = m_egr_egrpar->GetTable();
  egr_egrpar->scenario =   0;
  egr_egrpar->mxtry =     10;
  egr_egrpar->minfit =     2;
  egr_egrpar->prob[0] =    2;
  egr_egrpar->prob[1] =    2;
  memset(egr_egrpar->debug, 0, 10*sizeof(Int_t));  
  egr_egrpar->debug[0] =   1; 
  egr_egrpar->svtchicut =  0;
  egr_egrpar->usetpc =     1;
  egr_egrpar->usesvt =     0;
  egr_egrpar->usevert =    0;
  egr_egrpar->useglobal =  2;
  //  egr_egrpar->scenario  = m_scenario;
  //  egr_egrpar->svtchicut = m_svtchicut;
  //  egr_egrpar->useglobal = m_useglobal;
  //  egr_egrpar->usetpc    = m_usetpc;
  //  egr_egrpar->usesvt    = m_usesvt; 
  //  egr_egrpar->usevert   = m_usevert;
  //evr
  m_evr_evrpar  = (St_evr_evrpar *) params("global/evrpars/evr_evrpar");
  // prop
  m_tp_param = new St_egr_propagate("tp_param",1); 
  params("global/evrpars")->Add(m_tp_param);
  egr_propagate_st *tp_param = m_tp_param->GetTable();
  tp_param->iflag =   m_flag;
  if (m_flag == 1 || m_flag == 2) {
    memset(tp_param->x,0,3*sizeof(Float_t));  
  }
  if (m_flag == 3) {
   tp_param->r     =  4.;
  }
  if (m_flag == 4) {
    tp_param->z =  0.; 
  }
  // egr2
  m_egr2_egrpar = (St_egr_egrpar *) params("global/egrpars/egr2_egrpar");
  if (!m_egr2_egrpar) {
    m_egr2_egrpar = new St_egr_egrpar("egr2_egrpar",1);
    params("global/egrpars")->Add(m_egr2_egrpar);
  }
  egr_egrpar_st *egr2_egrpar = m_egr2_egrpar->GetTable();
  egr2_egrpar->scenario =  5;
  egr2_egrpar->mxtry =    10;
  egr2_egrpar->minfit =    5;
  egr2_egrpar->prob[0] =   2;
  egr2_egrpar->prob[1] =   2;
  memset (egr2_egrpar->debug,0,10*sizeof(Int_t));
  egr2_egrpar->debug[0] =  1;
  egr2_egrpar->svtchicut = 0;
  egr2_egrpar->usetpc    = 1;
  egr2_egrpar->usesvt    = 0;
  egr2_egrpar->usevert   = 1;
  egr2_egrpar->useglobal = 0;

  //ev0   
  m_ev0par2 = (St_ev0_ev0par2 *)  params("global/ev0pars/ev0par2");
  if (!m_ev0par2) {
    m_ev0par2 = new St_ev0_ev0par2("ev0par2",3);
    params("global/ev0pars")->Add(m_ev0par2);
  }
  ev0_ev0par2_st *ev0par2 = m_ev0par2->GetTable();
  m_ev0par2->SetNRows(3);
  // TPC only cuts

  ev0par2->dca        =  0.8;
  ev0par2->dcav0      =  0.7;
  ev0par2->dlen       =  2.0;
  ev0par2->alpha_max  = 1.2;
  ev0par2->ptarm_max  = 0.3;
  ev0par2->dcapnmin   = 0.7;
  ev0par2++;

  //SVT only cuts

  ev0par2->dca        = 0.8;
  ev0par2->dcav0      = 0.3;
  ev0par2->dlen       = 10000.;
  ev0par2->alpha_max  = 1.2;
  ev0par2->ptarm_max  = 0.3;
  ev0par2->dcapnmin   = 100;
  ev0par2++;

  // SVT+TPC cuts
  ev0par2->dca        = 0.8;
  ev0par2->dcav0      = 0.3;
  ev0par2->dlen       = 0.6;
  ev0par2->alpha_max  = 1.2;
  ev0par2->ptarm_max  = 0.3;
  ev0par2->dcapnmin   = 0.7;
  //exi
  if (!m_exiaux) m_exiaux = new St_exi_aux("exi_aux",10000);
  m_exipar = (St_exi_exipar *)  params("global/exipars/exipar");
  if (!m_exipar) {
    m_exipar = new St_exi_exipar("exipar",3);
    params("global/exipars")->Add(m_exipar);
  }
  exi_exipar_st *exipar = m_exipar->GetTable();
  // TPC only cuts

  exipar->use_pid = 0;
  exipar->dca_max = 1.;
  exipar->bxi_max = 1.;
  exipar->rv_xi   = 2.;
  exipar->rv_v0   = 5.;
  exipar->dmass   = 0.01;
  exipar->bpn_v0  = 2.;
  exipar->pchisq  = 0.;
  exipar++;

  //SVT only cuts

  exipar->use_pid = 0;
  exipar->dca_max = 0.;
  exipar->bxi_max = 0.;
  exipar->rv_xi   = 999.;
  exipar->rv_v0   = 999.;
  exipar->dmass   = 0.;
  exipar->bpn_v0  = 999.;
  exipar->pchisq  = 0.;
  exipar++;

  // SVT+TPC cuts

  exipar->use_pid = 0;
  exipar->dca_max = 1.;
  exipar->bxi_max = 1.;
  exipar->rv_xi   = 2.;
  exipar->rv_v0   = 5.;
  exipar->dmass   = 0.01;
  exipar->bpn_v0  = 2.;
  exipar->pchisq  = 0.;
  exipar++;
  
  // Create Histograms    
  m_pT_eta_rec = new TH2F("pT_eta_rec","pT versus eta (reconstructed)",
			  nyeta,ymineta,ymaxeta,nxpT,xminpT,xmaxpT);
  m_pT_eta_rec->SetXTitle("eta");
  m_pT_eta_rec->SetYTitle("pT (GeV)");
  m_pT_eta_gen = new TH2F("pT_eta_gen","pT versus eta (generated)",
			  nyeta,ymineta,ymaxeta,nxpT,xminpT,xmaxpT);
  m_pT_eta_gen->SetXTitle("eta");
  m_pT_eta_gen->SetYTitle("pT (GeV)");
  m_pT   = new TH1F("pT","pT distribution",nxpT,xminpT,xmaxpT);
  m_eta  = new TH1F("eta","eta distribution",nyeta,ymineta,ymaxeta);
  // Al Saulys histograms
  m_tlength = new TH1F("tlenght","dst track length",100,0.,200.);
  m_chi2xd  = new TH1F("chi2xd","x chisq/degf",100,0.,10.);
  m_chi2yd  = new TH1F("chi2yd","y chisq/degf",100,0.,10.);
  m_ev0_lama_hist  = new TH1F("ev0_lama_hist","Lambda mass",50,1.05,1.25);
  m_ev0_k0ma_hist  = new TH1F("ev0_k0ma_hist","k0 mass",50,.4,.6);
  // Spectra/pid histograms. C.Ogilvie
  Int_t np = 100;
  Int_t ndedx = 100;
  Float_t minp = 0.0;
  Float_t maxp = 2.0;
  Float_t mindedx = 0.0;
  Float_t maxdedx =  0.1e-04;
  m_p_dedx_rec = new TH2F("p_dedx_rec","p versus dedx (reconstructed)",
                           np,minp,maxp,ndedx,mindedx,maxdedx);
  m_p_dedx_rec->SetYTitle("dedx");
  m_p_dedx_rec->SetXTitle("p (GeV)");
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_glb_Maker::Make(){
  //  PrintInfo();
  St_DataSetIter global(m_DataSet);         // data/global
  St_DataSet  *dst_loc = global("dst");
  if (! dst_loc) dst_loc = global.Mkdir("dst");
  // Make a temp dir. to hold SVT stuff when the SVT isnt there
  St_DataSet  *temp = global.Mkdir("temp");
  St_DataSetIter dst(dst_loc);

  St_dst_track      *globtrk     = (St_dst_track     *) dst("globtrk");
  St_dst_track_aux  *globtrk_aux = (St_dst_track_aux *) dst("globtrk_aux");
  St_dst_track      *primtrk     = (St_dst_track     *) dst("primtrk");
  St_dst_track_aux  *primtrk_aux = (St_dst_track_aux *) dst("primtrk_aux");
  St_dst_vertex     *vertex      = (St_dst_vertex    *) dst("vertex");
  St_dst_v0_vertex  *dst_v0_vertex = (St_dst_v0_vertex    *) dst("dst_v0_vertex"); 
   St_dst_xi_vertex  *dst_xi_vertex = (St_dst_xi_vertex    *) dst("dst_xi_vertex");
  St_dst_dedx       *dst_dedx    = (St_dst_dedx      *) dst("dst_dedx");
  St_dst_point      *point       = (St_dst_point     *) dst("point");
  St_dst_event_header  *event_header  = (St_dst_event_header  *) dst("event_header");
  St_dst_event_summary *event_summary = (St_dst_event_summary *) dst("event_summary");
  St_dst_monitor_soft  *monitor_soft  = (St_dst_monitor_soft  *) dst("monitor_soft");
  if (! event_header) {
    event_header  = new St_dst_event_header("event_header",1);
    dst.Add(event_header);
  }
  dst_event_header_st  event =   {"Collision", //event_type
				  {0,0},       // n_event[2]
				  0, 0, 0, 0}; // n_run,time,trig_mask,bunch_cross
  event_header->AddAt(&event,0);
  if (! event_summary) {
    event_summary = new St_dst_event_summary("event_summary",1);
    dst.Add(event_summary);
  }
  
  St_DataSet    *tpctracks = gStChain->DataSet("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  St_tte_eval   *evaltrk   = 0;
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
    evaltrk   = (St_tte_eval   *) tpc_tracks("evaltrk");
  }
  St_DataSet         *tpchits = gStChain->DataSet("tpc_hits");
  St_tcl_tphit     *tphit     = 0;
  St_tcl_tpcluster *tpcluster = 0;
  if (tpchits) {
    St_DataSetIter    tpc_hits(gStChain->DataSet("tpc_hits")); 
    tphit     = (St_tcl_tphit     *) tpc_hits("tphit");
    tpcluster = (St_tcl_tpcluster *) tpc_hits("tpcluster");
  }
  if (! tpcluster)    {tpcluster = new St_tcl_tpcluster("tpcluster",1); temp->Add(tpcluster);}
  St_DataSet     *svtracks = gStChain->DataSet("svt_tracks"); 
  St_stk_track  *stk_track = 0;
  St_sgr_groups *groups    = 0;
  
  if (svtracks) {
    St_DataSetIter svt_tracks(svtracks);
    stk_track = (St_stk_track  *) svt_tracks("stk_track");
    groups    = (St_sgr_groups *) svt_tracks("groups");
  }
  if (!stk_track) {stk_track = new St_stk_track("stk_track",1); temp->Add(stk_track);}
  if (!groups) {groups = new St_sgr_groups("groups",1); temp->Add(groups);}

  St_DataSet         *svthits = gStChain->DataSet("svt_hits");
  St_scs_spt     *scs_spt     = 0;
  St_scs_cluster *scs_cluster = 0;
  if (svthits) {
    St_DataSetIter   svt_hits(gStChain->DataSet("svt_hits"));
    scs_spt      = (St_scs_spt     *) svt_hits("scs_spt");
    scs_cluster = (St_scs_cluster *) svt_hits("scs_cluster");
  }
  if (!scs_spt) {scs_spt = new St_scs_spt("scs_spt",1); temp->Add(scs_spt);}
  // What is [data]/svt/hits/scs_cluster ?
  if (! scs_cluster) {scs_cluster = new St_scs_cluster("scs_cluster",1); temp->Add(scs_cluster);}
  St_DataSet *ctf = gStChain->DataSet("ctf");
  St_ctu_cor *ctb_cor = 0;
  if (!ctf) {cout << "St_ctf_Maker has not been called " << endl;}
  else {
    St_DataSetIter ctf_hits(ctf);
    ctb_cor = (St_ctu_cor *) ctf_hits("ctb_cor"); 
    if (! ctb_cor) {ctb_cor = new St_ctu_cor("ctb_cor",1); temp->Add(ctb_cor);}
  }
  if (!primtrk){ //create dst
    St_svm_evt_match *evt_match = 0;
    if (tptrack && stk_track) {
       //svm
      evt_match = (St_svm_evt_match *) global("tracks/evt_match");
      if (!evt_match) {evt_match  = new St_svm_evt_match("evt_match",3000); temp->Add(evt_match);}
      Int_t res_svm =  svm_am (stk_track, tptrack,
			       m_svm_ctrl, evt_match);
      if (! globtrk)    {globtrk = new St_dst_track("globtrk",20000);             dst.Add(globtrk);}
      if (! globtrk_aux){globtrk_aux = new St_dst_track_aux("globtrk_aux",20000); dst.Add(globtrk_aux);}
      if (! vertex) {vertex = new St_dst_vertex("vertex",20000); dst.Add(vertex);}
      // egr
      Int_t Res_egr =  egr_fitter (tphit,vertex,tptrack,evaltrk,
				   scs_spt,m_egr_egrpar,stk_track,groups,
				   evt_match,globtrk,globtrk_aux);
    
      if (Res_egr != kSTAFCV_OK) {cout << "Problem on return from EGR_FITTER" << endl;}
      cout << " finished calling egr_fitter" << endl;
    }
#if 1
    // evr
    cout << "run_evr: calling evr_am" << endl;
    Int_t Res_evr = evr_am(m_evr_evrpar,m_egr_egrpar,globtrk,vertex);
    // track_propagator
    St_dst_track *globtrk2     = new St_dst_track("globtrk2",20000);
    dst.Add(globtrk2);
    *globtrk2  = *globtrk;
    cout << " Calling track_propagator " << endl;
    if (m_tp_param && vertex) {
      egr_propagate_st *tp_param = m_tp_param->GetTable();
      tp_param->iflag =   m_flag;
      if (m_flag == 1 || m_flag == 2) {
	dst_vertex_st *vrtx = vertex->GetTable();
	memcpy(&tp_param->x,&vrtx->x,3*sizeof(Float_t));  
      }
    }
    Int_t Res_tp = track_propagator(globtrk,m_tp_param,globtrk2);

    if (Res_tp !=  kSTAFCV_OK) {
      cout << "Problem on return from Track_Propagator" << endl;
    }
    cout << " finished calling track-propagator" << endl;
    // egr2
    if (tphit && stk_track) {
      if (!primtrk) {
	primtrk = new St_dst_track("primtrk",30000);
	dst.Add(primtrk);
      }
      if (!primtrk_aux) {
	primtrk_aux = new St_dst_track_aux("primtrk_aux",30000);
	dst.Add(primtrk_aux);
      }
      cout << "Calling EGR_fitter - Second time" << endl;
      Int_t Res_egr2 = egr_fitter (tphit,vertex,tptrack,evaltrk,
				   scs_spt,m_egr2_egrpar,stk_track,groups,
				   evt_match,primtrk,primtrk_aux);
      if (Res_egr2 != kSTAFCV_OK){
	cout << "Problem on return from EGR_FITTER" << endl;
      }
      cout <<" finished calling egr_fitter - second time" << endl;
    }
#endif
    // ev0
    cout << "Calling ev0..." << endl;
    if (! dst_v0_vertex) {dst_v0_vertex = new St_dst_v0_vertex("dst_v0_vertex",20000); dst.Add(dst_v0_vertex);}
    St_ev0_track2 *ev0track2 = new St_ev0_track2("ev0_track2",20000);
    dst.Add(ev0track2);
    if (vertex->GetNRows() != 1) {vertex->SetNRows(1);} 
    Int_t Res_ev0 = ev0_am2(m_ev0par2,globtrk,vertex,dst_v0_vertex,ev0track2);
    if (Res_ev0 != kSTAFCV_OK) {cout << " Problem on return from EV0 " << endl;}
#if 0
    //  ev0_eval2
    if (stk_track && tptrack && evaltrk) {
      St_ev0_eval *ev0_eval = new St_ev0_eval("ev0_eval",20000);
      dst.Add(ev0_eval); 
      St_DataSetIter geant(gStChain->DataSet("geant"));
      St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("Event/g2t_track");
      St_g2t_vertex  *g2t_vertex   = (St_g2t_vertex *) geant("Event/g2t_vertex");
      if (g2t_track && g2t_vertex){
	cout << " Calling ev0_eval2.." << endl;
    
	Int_t Res_ev0_eval = ev0_eval2(stk_track,tptrack,evaltrk,
				       vertex,ev0out,ev0_eval,
				       g2t_track,g2t_vertex);
    
	if (Res_ev0_eval != kSTAFCV_OK) {cout << "Problem on return from ev0eval2" << endl;}
      }
    }
#endif
    // exi
    cout << "Calling exi..."<< endl;
    if (! dst_xi_vertex) {
      dst_xi_vertex = new St_dst_xi_vertex("dst_xi_vertex",10000);
      dst.Add(dst_xi_vertex);
     }
    Int_t Res_exi = exiam(m_exipar,globtrk,vertex,dst_v0_vertex,dst_xi_vertex,m_exiaux);
    if (Res_exi != kSTAFCV_OK) {cout << " Problem on return from EXI " << endl;}
    // dst 
    // dst_dedx_filler
    if (tptrack && stk_track) {
      cout << " run_dst: Calling dst_dedx_filler" << endl;
      if (!dst_dedx) {
	dst_dedx = new St_dst_dedx("dst_dedx",20000); dst.Add(dst_dedx);
      }
      Int_t Res_dedx_filler =  dst_dedx_filler(tptrack,stk_track,dst_dedx);
    
      if (Res_dedx_filler != kSTAFCV_OK) {
	cout << "Problem on return from DST_DEDX_FILLER" << endl; 
      }
      cout << " run_dst: finshed calling dst_dedx_filler" << endl;
    }
    // dst_mon_soft
    if (tphit &&  scs_spt) {
      cout << " run_dst: Calling dst_point_filler" << endl;
      // dst_point_filler
      if (! point) {point = new St_dst_point("point",200000); dst.Add(point);}
      St_dst_tof *tof = new St_dst_tof("tof",2000); dst.Add(tof);
      Int_t Res_dst_point_filler = dst_point_filler(tphit, scs_spt, point);
    
      if ( Res_dst_point_filler != kSTAFCV_OK) {
	cout << "Problem on return from DST_POINT_FILLER" << endl;
      }
    
      cout << " run_dst: finished calling dst_point_filler" << endl;
    }
    St_DataSet *ftpc_hits   = gStChain->DataSet("ftpc_hits");
    St_fcl_fppoint *fcl_fppoint = 0;
    if (ftpc_hits) {
      St_DataSetIter hits(ftpc_hits);
      fcl_fppoint = (St_fcl_fppoint *) hits["fcl_fppoint"];
    }
    St_DataSet *ftpc_tracks = gStChain->DataSet("ftpc_tracks");
    St_fpt_fptrack *fpt_fptrack = 0;
    if (ftpc_tracks) {
      St_DataSetIter tracks(ftpc_tracks);
      fpt_fptrack = (St_fpt_fptrack *) tracks["fpt_fptrack"];
    }
    if (fcl_fppoint && fpt_fptrack) {
      cout<<" run_dst: Calling fill_ftpc_dst"<<endl;
      Int_t Res_fill_ftpc_dst= fill_ftpc_dst(fpt_fptrack,fcl_fppoint,globtrk,
                              globtrk_aux,point,vertex,dst_dedx);
      if ( Res_fill_ftpc_dst != kSTAFCV_OK) {
        cout << "Problem on return from FILL_FTPC_DST" << endl;
      }
      cout << " run_dst: finished calling fill_ftpc_dst" << endl;
    }
    if (tphit && scs_spt && tptrack && stk_track && evt_match && ctf && fcl_fppoint && fpt_fptrack) {
      if (! monitor_soft) {
	monitor_soft = new St_dst_monitor_soft("monitor_soft",1);
	dst.Add(monitor_soft);
      }
      cout << " run_dst: Calling dst_monitor_soft_filler" << endl;
      
      Int_t Res_dst_monitor =  dst_monitor_soft_filler(tpcluster,
						       scs_cluster,
						       tphit,scs_spt,fcl_fppoint,
						       tptrack,stk_track,fpt_fptrack,
						       evt_match,
						       ctb_cor,vertex,event_summary,monitor_soft);
      if (Res_dst_monitor  != kSTAFCV_OK){
	cout << "Problem on return from DST_MONITOR_SOFT_FILLER" << endl;
      }
      cout << " run_dst: finished calling dst_monitor_soft_filler" << endl;
    }      
    St_DataSet *run_summary = gStChain->DataSet("run_summary");
    if (run_summary) {
      cout << " run_dst: Calling fill_dst_event_summary" << endl;
      St_DataSetIter summary(run_summary);
      St_dst_run_header *run_header = (St_dst_run_header *) summary("run_header");
      St_dst_summary_param *summary_param = (St_dst_summary_param *) summary("summary_param");
      
      Int_t Res_fill_dst_event_summary = fill_dst_event_summary(summary_param,run_header,event_header,
								globtrk,vertex,event_summary);
    
      if (Res_fill_dst_event_summary != kSTAFCV_OK) {
	cout << "Problem on return from FILL_DST_EVENT_SUMMARY" << endl;
      }
      cout << " run_dst: finished calling fill_dst_event_summary" << endl;
    }
  }
  // Fill histograms
  Histograms();
 // look for generator data

  // delete temp dir which holds fake svt when its not there
  SafeDelete(temp);
  return kStOK;
}

//_____________________________________________________________________________
void St_glb_Maker::Histograms(){
  St_DataSetIter global(m_DataSet);         // data/global
  St_dst_track      *primtrk     = (St_dst_track     *) global("dst/primtrk");
  if (primtrk) {
    table_head_st *trk_h = primtrk->GetHeader();
    dst_track_st  *trk   = primtrk->GetTable();
    for (Int_t i = 0; i < primtrk->GetNRows(); i++){
      dst_track_st *t = trk + i;
      Float_t pT = 9999.;
      if (t->invpt) pT = 1./TMath::Abs(t->invpt);
      Float_t theta = TMath::Pi() - TMath::ATan(t->tanl);
      Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
      m_pT->Fill(pT);
      m_eta->Fill(eta);
      m_pT_eta_rec->Fill(eta,pT);
      // Al histograms
      m_tlength->Fill(t->length);
      if (t->ndegf>0) {
        m_chi2xd->Fill(t->chisq[0]/((t->ndegf+5.)/2.-3.));  
        m_chi2yd->Fill(t->chisq[1]/((t->ndegf+5.)/2.-2.));  
      }
    }
  }
  St_hepe_gent *hepev = (St_hepe_gent *) global("dst/particle");
  St_particle  *particle=0;
  if (hepev) {
    hepe_gent_st *p = hepev->GetTable();
    for (Int_t l=0; l < hepev->GetNRows(); l++, p++){
      if (p->isthep == 1) {
	Double_t px = p->phep[0];
	Double_t py = p->phep[1];
	Double_t pz = p->phep[2];
	Double_t pT    =  TMath::Sqrt(px*px+py*py);
	Double_t theta =  TMath::ATan2 ( pT, pz );
	Float_t  eta  = -TMath::Log(TMath::Tan(theta/2.));
	m_pT_eta_gen->Fill(eta, (Float_t) pT);
      }
    }
  }
  else {
    St_DataSet *evgen = gStChain->DataSet("evgen");
    if (evgen) {
      St_DataSetIter local(evgen);
      St_particle *pa = (St_particle *) local("particle");
      if (pa){
        particle_st *p = pa->GetTable();
        for (Int_t l=0; l < pa->GetNRows(); l++, p++){
          if (p->isthep == 1) {
            Double_t px = p->phep[0];
            Double_t py = p->phep[1];
            Double_t pz = p->phep[2];
            Double_t pT    =  TMath::Sqrt(px*px+py*py);
            Double_t theta =  TMath::ATan2 ( pT, pz );
	    //        Double_t theta =  atan2 ( pT, pz );
            Float_t  eta  = -TMath::Log(TMath::Tan(theta/2.));
            m_pT_eta_gen->Fill(eta, (Float_t) pT);
	  }
	}
      }
    }
  }
  // V0
  St_dst_v0_vertex  *dst_v0_vertex = (St_dst_v0_vertex *) global("dst/dst_v0_vertex");
  if (dst_v0_vertex) {

    cout << "Filling ev0 histos" << endl;
    dst_v0_vertex_st *v0 = dst_v0_vertex->GetTable();
    Float_t m_prmass2 = proton_mass_c2*proton_mass_c2;
    Float_t m_pimass2 = (0.139567*0.139567);
    for (Int_t k=0; k<dst_v0_vertex->GetNRows(); k++, v0++){
      Float_t e1a = v0->pos_px*v0->pos_px +  v0->pos_py*v0->pos_py
	+ v0->pos_pz*v0->pos_pz;
      Float_t e2 = v0->neg_px*v0->neg_px +  v0->neg_py*v0->neg_py
	+ v0->neg_pz*v0->neg_pz;
      Float_t e1 = e1a + m_prmass2;  
      e2 += m_pimass2;
      e1 = TMath::Sqrt(e1);
      e2 = TMath::Sqrt(e2);
      Float_t p = (v0->neg_px+v0->pos_px)*(v0->neg_px+v0->pos_px)
	+  (v0->neg_py+v0->pos_py)*(v0->neg_py+v0->pos_py)
	+ (v0->neg_pz+v0->pos_pz)*(v0->neg_pz+v0->pos_pz);
      Float_t inv_mass_la = TMath::Sqrt((e1+e2)*(e1+e2) - p);
      e1 = e1a + m_pimass2;
      e1 = TMath::Sqrt(e1);
      Float_t inv_mass_k0 = TMath::Sqrt((e1+e2)*(e1+e2) - p);
      m_ev0_lama_hist->Fill(inv_mass_la);
      m_ev0_k0ma_hist->Fill(inv_mass_k0);   
    }
  }
  // spectra-PID diagnostic histograms
  St_dst_dedx       *dst_dedx    = (St_dst_dedx *) global("dst/dst_dedx");
  if (dst_dedx && primtrk) {
     	dst_dedx_st  *de   = dst_dedx->GetTable();
        dst_track_st  *trk   = primtrk->GetTable();
	// loop over dedx entries
        for (Int_t l = 0; l < dst_dedx->GetNRows(); l++){
	       dst_dedx_st *d = de + l;
               Float_t dedx_m = d->dedx[0];
               Int_t igl = d->id_track;
               Int_t igl_use = igl - 1;
    // this is bad style, since it assumes the global track has not been sorted
    // it works for now
               dst_track_st  *t = trk + igl_use ;
               Float_t invpt = t->invpt;
	       Float_t pT = 9999.;
	       if (invpt) pT = 1./TMath::Abs(invpt);
               Float_t pz = pT*t->tanl;
               Float_t  p = sqrt(pT*pT+pz*pz);
               Float_t z0 = abs(t->x_first[2]);
               Float_t x0 = t->x_first[0];
               Float_t y0 = t->x_first[1];
               Float_t r0 = sqrt(x0*x0+y0*y0);

	       if (d->det_id==1 && d->ndedx >15 ) {
		 m_p_dedx_rec->Fill(p,dedx_m);
	       }
	}
	cout << " run_dst: finished filling dedx histograms" << endl;
  }
}
//_____________________________________________________________________________
void St_glb_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_glb_Maker.cxx,v 1.29 1999/02/17 23:58:11 caines Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

