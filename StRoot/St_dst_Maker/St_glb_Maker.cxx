// $Id: St_glb_Maker.cxx,v 1.8 1998/11/01 16:42:27 fisyak Exp $
// $Log: St_glb_Maker.cxx,v $
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
// Modify St_dst_Maker to account new calling sequence
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
// St_dst_Maker class for Makers (evr + egr + ev0 + ev0_eval)           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <TMath.h>
#include "St_dst_Maker.h"
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
#include "global/St_ev0_am_Module.h"
#include "global/St_ev0_eval2_Module.h"
#include "global/St_dst_dedx_filler_Module.h"
#include "global/St_dst_monitor_soft_filler_Module.h"

#include "global/St_particle_dst_filler_Module.h"
#include "global/St_dst_point_filler_Module.h"
#include "global/St_fill_dst_event_summary_Module.h"

   const Int_t St_dst_Maker::nxpT = 50;
   const Int_t St_dst_Maker::nyeta = 50;
   const Float_t St_dst_Maker::xminpT = 0.0;
   const Float_t St_dst_Maker::xmaxpT = 5.0;
   const Float_t St_dst_Maker::ymineta = -2.0;
   const Float_t St_dst_Maker::ymaxeta =  2.0;

ClassImp(St_dst_Maker)

//_____________________________________________________________________________
St_dst_Maker::St_dst_Maker(const char *name, const char *title):StMaker(name,title),
m_svm_ctrl(0),
m_evr_privert(0),
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
}
//_____________________________________________________________________________
St_dst_Maker::~St_dst_Maker(){
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Init(){
// Create tables
   St_DataSetIter params(gStChain->DataSet("params"));
//svm
   m_svm_ctrl   = (St_svm_ctrl *)   params("global/svmpars/svm_ctrl");
//evr
   m_evr_privert = (St_evr_privert *) params("global/evrpars/evr_privert");
   m_evr_evrpar  = (St_evr_evrpar *) params("global/evrpars/evr_evrpar");
//egr 
   m_egr_egrpar = (St_egr_egrpar *) params("global/egrpars/egr_egrpar");
   egr_egrpar_st *egr_egrpar = m_egr_egrpar->GetTable();
   egr_egrpar->scenario  = m_scenario;
   egr_egrpar->svtchicut = m_svtchicut;
   egr_egrpar->useglobal = m_useglobal;
   egr_egrpar->usetpc    = m_usetpc;
   egr_egrpar->usesvt    = m_usesvt; 
   egr_egrpar->usevert   = m_usevert;
//ev0   
   m_ev0par = (St_ev0_ev0par *)  params("global/ev0pars/ev0par");
   m_magf   = (St_mft_control *) params("global/magnetic_field/magf");  
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
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Make(){
  //  PrintInfo();
  if (!m_DataSet->GetList()){ //create dst
    St_DataSetIter dst(m_DataSet);         // data/global/dst
    St_DataSetIter global(gStChain->DataSet("global")); // data/global
    St_DataSetIter tpc_tracks(gStChain->DataSet("tpc_tracks")); 
    St_DataSetIter tpc_hits(gStChain->DataSet("tpc_hits")); 

    St_DataSetIter svt_tracks(gStChain->DataSet("svt_tracks"));
    St_DataSetIter svt_hits(gStChain->DataSet("svt_hits"));

    St_tpt_track  *tptrack   = (St_tpt_track *) tpc_tracks("tptrack");
    St_tcl_tphit  *tphit = (St_tcl_tphit *) tpc_hits("tphit");
    St_stk_track  *stk_track = (St_stk_track *) svt_tracks("stk_track");

    St_tte_eval     *evaltrk      = (St_tte_eval   *) tpc_tracks("evaltrk");
    St_scs_spt      *scs_spt      = (St_scs_spt    *) svt_hits("scs_spt");
    St_sgr_groups   *groups       = (St_sgr_groups *) svt_tracks("groups");
  //svm
    St_DataSet  *global_track = global.Mkdir("tracks");
    St_DataSetIter track(global_track);
    St_svm_evt_match *evt_match  = new St_svm_evt_match("evt_match",3000); track.Add(evt_match);

    if (tptrack && stk_track) {

      Int_t res_svm =  svm_am (stk_track, tptrack,
                           m_svm_ctrl, evt_match);
      cout << "Calling SVM_EVAL2.." << endl;
  
     //
      St_DataSetIter run(gStChain->DataSet("geom"));
      St_g2t_gepart *g2t_gepart  = (St_g2t_gepart *) run("g2t_gepart");
      if (!g2t_gepart){
        g2t_gepart   = new St_g2t_gepart("g2t_gepart",1);
        run.Add(g2t_gepart);
      }
      St_svm_eval_par *svm_eval_par = new St_svm_eval_par("svm_eval_par",1); track.Add(svm_eval_par);
      St_svm_eval_svt *svm_eval_svt = new St_svm_eval_svt("svm_eval_svt",3000); track.Add(svm_eval_svt);
      St_svm_eval_tpc *svm_eval_tpc = new St_svm_eval_tpc("svm_eval_tpc",3000); track.Add(svm_eval_tpc);

      Int_t Res_svm_eval = svm_eval2(g2t_gepart,scs_spt,groups,stk_track,tptrack,
		                 evaltrk,evt_match,svm_eval_par,
		                 svm_eval_svt,svm_eval_tpc);

      cout << " Calling SVM_SVT_EVAL " << endl;
  // What is [data]/glosvm_eval_strkbal/tracks/svm_eval_strk ?
      St_svm_eval_strk *svm_eval_strk = new St_svm_eval_strk("svm_eval_strk",100000);
                                 track.Add(svm_eval_strk);
      Int_t Res_svm_svt_eval  = svm_svt_eval(scs_spt,groups,stk_track,tptrack,evaltrk,
                                        evt_match, svm_eval_par,svm_eval_strk);
  }
    St_dst_track     *globtrk     = new St_dst_track("globtrk",100000);         dst.Add(globtrk);
    St_dst_track_aux *globtrk_aux = new St_dst_track_aux("globtrk_aux",100000); dst.Add(globtrk_aux);
    St_ev0_track     *ev0track    = new St_ev0_track("ev0track",100000);        dst.Add(ev0track);
    St_DataSet *vertices    = global("vertices");
    if (!vertices) vertices = global.Mkdir("vertices");
  // ev0
    St_ev0_aux *ev0out = new St_ev0_aux("ev0out",100000); vertices->Add(ev0out);
    St_dst_vertex *vertex = new St_dst_vertex("vertex",100000); dst.Add(vertex);
  // evr
    cout << "run_evr: calling evr_am" << endl;

    Int_t Res_evr = evr_am(m_evr_evrpar,tptrack,stk_track,globtrk,evt_match,vertex);

  // egr

    Int_t Res_egr =  egr_fitter (tphit,vertex,tptrack,evaltrk,
                             scs_spt,m_egr_egrpar,stk_track,groups,
                             evt_match,globtrk,globtrk_aux);

  if (Res_egr != kSTAFCV_OK) {
    cout << "Problem on return from EGR_FITTER" << endl;
  }
  cout << " finished calling egr_fitter" << endl;
  //ev0
  cout << "Calling ev0..." << endl;

  Int_t Res_ev0 = ev0_am(m_ev0par,globtrk,vertex,
                         ev0out,ev0track,m_magf);

  if (Res_ev0 != kSTAFCV_OK){
    cout << "Problem on return from ev0_am" << endl;
  }
  //  ev0_eval2
    St_ev0_eval *ev0_eval = new St_ev0_eval("ev0_eval",100000); vertices->Add(ev0_eval); 
    St_DataSetIter geant(gStChain->DataSet("geant"));
    St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("Event/g2t_track");
    St_g2t_vertex  *g2t_vertex   = (St_g2t_vertex *)geant("Event/g2t_vertex");
  
  cout << " Calling ev0_eval2.." << endl;

  Int_t Res_ev0_eval = ev0_eval2(stk_track,tptrack,evaltrk,
                                 vertex,ev0out,ev0_eval,
                                 g2t_track,g2t_vertex);

  if (Res_ev0_eval != kSTAFCV_OK) {
     cout << "Problem on return from ev0eval2" << endl;
  }
 //  dst 
    St_dst_dedx *dst_dedx = new St_dst_dedx("dst_dedx",100000); dst.Add(dst_dedx);
				 // dst_dedx_filler

  cout << " run_dst: Calling dst_dedx_filler" << endl;

  Int_t Res_dedx_filler =  dst_dedx_filler(tptrack,stk_track,dst_dedx);

  if (Res_dedx_filler != kSTAFCV_OK) {
    cout << "Problem on return from DST_DEDX_FILLER" << endl; 
  }
  cout << " run_dst: finished calling dst_dedx_filler" << endl;
					// dst_mon_soft
  cout << " run_dst: Calling dst_point_filler" << endl;
                              // dst_point_filler
    St_dst_point *point = new St_dst_point("point",200000); dst.Add(point);
#if 0
    St_dst_tof *tof = new St_dst_tof("tof",2000); dst.Add(tof);
#endif
  Int_t Res_dst_point_filler = dst_point_filler(tphit, scs_spt, point);

  if ( Res_dst_point_filler != kSTAFCV_OK) {
     cout << "Problem on return from DST_POINT_FILLER" << endl;
  }

  cout << " run_dst: finished calling dst_point_filler" << endl;
    St_dst_event_header  *event_header  = new St_dst_event_header("event_header",1);
                                                              dst.Add(event_header);
    St_dst_event_summary *event_summary = new St_dst_event_summary("event_summary",1);
                                                              dst.Add(event_summary);
    St_dst_monitor_soft *monitor_soft = new St_dst_monitor_soft("monitor_soft",1);
                                                              dst.Add(monitor_soft);
    cout << " run_dst: Calling dst_monitor_soft_filler" << endl;
    St_tcl_tpcluster  *tpcluster = (St_tcl_tpcluster *) tpc_hits("tpcluster");
    if (! tpcluster) {tpcluster = new St_tcl_tpcluster("tpcluster",1); tpc_hits.Add(tpcluster);}
  // What is [data]/svt/hits/scs_cluster ?
    St_scs_cluster *scs_cluster = new St_scs_cluster("scs_cluster",1); svt_hits.Add(scs_cluster);
    St_DataSet *ctf = gStChain->DataSet("ctf_hits");
    if (!ctf) {cout << "St_ctf_Maker has not been called " << endl;}
    St_DataSetIter ctf_hits(ctf);
    St_ctu_cor *ctb_cor = (St_ctu_cor *) ctf_hits("ctb_cor"); 
    if (! ctb_cor) {ctb_cor = new St_ctu_cor("ctb_cor",1); ctf_hits.Add(ctb_cor,"ctf");}
    Int_t Res_dst_monitor =  dst_monitor_soft_filler(tpcluster,
                            scs_cluster,
                            tphit,scs_spt,tptrack,stk_track,evt_match,
                            ctb_cor,vertex,event_summary,monitor_soft);

    if (Res_dst_monitor  != kSTAFCV_OK){
      cout << "Problem on return from DST_MONITOR_SOFT_FILLER" << endl;
    }
    cout << " run_dst: finished calling dst_monitor_soft_filler" << endl;

    cout << " run_dst: Calling fill_dst_event_summary" << endl;
    St_DataSetIter run_summary(gStChain->DataSet("run_summary"));
    St_dst_run_header *run_header = (St_dst_run_header *) run_summary("run_header");
    Int_t Res_fill_dst_event_summary = fill_dst_event_summary(run_header,event_header,
                                                            globtrk,vertex,event_summary);

    if (Res_fill_dst_event_summary != kSTAFCV_OK) {
      cout << "Problem on return from FILL_DST_EVENT_SUMMARY" << endl;
    }
    cout << " run_dst: finished calling fill_dst_event_summary" << endl;
  }
  // Fill histograms
  St_DataSetIter dst(m_DataSet);         // data/global/dst
  St_dst_track *globtrk = (St_dst_track *) dst("globtrk");
  if (!globtrk) {
    table_head_st *trk_h = globtrk->GetHeader();
    dst_track_st  *trk   = globtrk->GetTable();
    for (Int_t i = 0; i < globtrk->GetNRows(); i++){
      dst_track_st *t = trk + i;
      Float_t pT = 9999.;
      if (t->invpt) pT = 1./TMath::Abs(t->invpt);
      Float_t theta = TMath::Pi() - TMath::ATan(t->tanl);
      Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
      m_pT->Fill(pT);
      m_eta->Fill(eta);
      m_pT_eta_rec->Fill(eta,pT);
    }
  }
  St_hepe_gent *hepev = (St_hepe_gent *) dst("hepe_gent");
  St_particle  *particle=0;
  if (hepev) {
      table_head_st *t1_h = hepev->GetHeader();
      hepe_gent_st *particle = hepev->GetTable();
      for (Int_t l=0; l < hepev->GetNRows(); l++){
        hepe_gent_st *p = particle+l;
        if (p->isthep == 1) {
          Float_t px = p->phep[0];
          Float_t py = p->phep[1];
          Float_t pz = p->phep[2];
          Float_t pT    =  TMath::Sqrt(px*px+py*py);
          Double_t theta =  TMath::Atan2 ( pT, pz );
//        Double_t theta =  atan2 ( pT, pz );
          Float_t  eta  = -TMath::Log(TMath::Tan(theta/2.));
          m_pT_eta_gen->Fill(eta,pT);
	}
      }
  }
  else {
    St_DataSet *evgen = gStChain->DataSet("evgen");
    if (evgen) {
      St_DataSetIter local(evgen);
      St_particle *pa = (St_particle *) local("particle");
      if (pa){
        table_head_st *t1_h = pa->GetHeader();
        particle_st *particle = pa->GetTable();
        for (Int_t l=0; l < pa->GetNRows(); l++){
          particle_st *p = particle+l;
          if (p->isthep == 1) {
            Float_t px = p->phep[0];
            Float_t py = p->phep[1];
            Float_t pz = p->phep[2];
            Float_t pT    =  TMath::Sqrt(px*px+py*py);
            Double_t theta =  TMath::Atan2 ( pT, pz );
//        Double_t theta =  atan2 ( pT, pz );
            Float_t  eta  = -TMath::Log(TMath::Tan(theta/2.));
            m_pT_eta_gen->Fill(eta,pT);
	  }
	}
      }
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
void St_dst_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_glb_Maker.cxx,v 1.8 1998/11/01 16:42:27 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

