// $Id: St_tpt_Maker.cxx,v 1.13 1999/01/12 19:50:19 sakrejda Exp $
// $Log: St_tpt_Maker.cxx,v $
// Revision 1.13  1999/01/12 19:50:19  sakrejda
// QA histograms added to the tpt maker
//
// Revision 1.12  1999/01/08 23:19:42  sakrejda
// histogramming added
//
// Revision 1.11  1999/01/02 19:08:23  fisyak
// Add ctf
//
// Revision 1.10  1998/10/31 00:26:23  fisyak
// Makers take care about branches
//
// Revision 1.9  1998/10/06 18:00:50  perev
// cleanup
//
// Revision 1.8  1998/09/23 20:23:16  fisyak
// Prerelease SL98h
//
// Revision 1.7  1998/09/19 00:15:44  fisyak
// move iostrem into <>
//
// Revision 1.6  1998/09/15 20:55:29  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.5  1998/08/18 14:05:04  fisyak
// Add to bfc dst
//
// Revision 1.3  1998/08/07 19:34:55  fisyak
// Add St_run_Maker
//
// Revision 1.2  1998/07/21 01:04:39  fisyak
// Clean up
//
// Revision 1.1  1998/07/21 00:36:46  fisyak
// tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tpt_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "St_tpt_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tpt_Module.h"
#include "tpc/St_tpt_residuals_Module.h"
#include "tpc/St_tte_track_Module.h"
#include "tpc/St_tde_new_Module.h"
#include "tpc/St_tte_Module.h"
#include "TH1.h"
ClassImp(St_tpt_Maker)
  
  //_____________________________________________________________________________
  St_tpt_Maker::St_tpt_Maker(const char *name, const char *title):
    StMaker(name,title),
    m_tpg_pad_plane(0),
    m_type(0),
    m_tpt_pars(0),
    m_tpt_spars(0),
    m_tte_control(0),
    m_tdeparm(0),
    m_tpipar(0)
{
  drawinit=kFALSE;
  m_iftte =kFALSE;
}
//_____________________________________________________________________________
St_tpt_Maker::~St_tpt_Maker(){
}
//_____________________________________________________________________________
Int_t St_tpt_Maker::Init(){
  // Create tables
  St_DataSetIter       local(gStChain->DataSet("params"));
  St_DataSet *tpc = local("tpc");
  if (! tpc)  tpc = local.Mkdir("tpc");
  // tpg parameters
  St_DataSet *tpgpar = local("tpc/tpgpar");
  if (tpgpar){
    St_DataSetIter partable(tpgpar);
    m_tpg_pad_plane = (St_tpg_pad_plane *) partable("tpg_pad_plane");
    if (!(m_tpg_pad_plane)) 
      printf("tpc/tpgpar is not initialized. Please add run_Maker to your chain\n");
  }
  // tcl parameters
  St_DataSet *tclpars = local("tpc/tclpars");
  if (tclpars){
    St_DataSetIter partable(tclpars);
    m_type             = (St_tcl_tpc_index_type *) partable("type");
    if (!m_type) {
      cout << " St_tcl_Maker:  clustering parameters have not been initialized" << endl;
      SafeDelete(tclpars);
    }
  }
  // tpt parameters
  St_DataSet *tptpars = local("tpc/tptpars");
  if (tptpars){
    St_DataSetIter partable(tptpars);
    m_tpt_pars = (St_tpt_pars *) partable("tpt_pars");
    m_tpt_spars = (St_tpt_spars *) partable("tpt_spars");
    if (!(m_tpt_pars && m_tpt_spars)) {
      cout << " St_tpt_Maker: tpt parameters have not been initialized" << endl;
      SafeDelete(tptpars);
    }
  }
  // tte_parameters
  St_DataSet *ttepars = local("tpc/ttepars");
  if (ttepars){
    St_DataSetIter partable(ttepars);
    m_tte_control = (St_tte_control *) partable("tte_control");
  }
  // tid parameters
  St_DataSet *tidpars = local("tpc/tidpars");
  if (tidpars){
    St_DataSetIter partable(tidpars);
    m_tdeparm = (St_tdeparm *) partable("tdeparm");
    m_tpipar  = (St_tpipar *)  partable("tpipar");
    if (!(m_tdeparm && m_tpipar)) {
      cout << " St_tpt_Maker: tid parameters have not been initialized" << endl;
      SafeDelete(tidpars);
    }
  }
  // Create Histograms
m_hits_on_track = new TH1F("hits_on_track","Number of hits on reconstructed tracks",50,.5,50.5);
m_hits_in_fit   = new TH1F("hits_in_fit","Number of hits used in the momentum fit",50,.5,50.5);
m_azimuth       = new TH1F("azimuth","Azimuthal distribution of tracks",60,0.,360.0);
m_tan_dip       = new TH1F("tan_dip","Distribution of the dip angle",100,-1.5,1.5);
m_r0            = new TH1F("r0","Radius for the first point",100,50.0,200);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_tpt_Maker::Make(){
  //  PrintInfo();
  const Int_t maxNofTracks = 20000; 
  if (!m_DataSet->GetList()) {// If DataSet list empty then create it
    St_DataSet *tpc_data =  gStChain->DataSet("tpc_hits");
    if (tpc_data) {// Clusters exist -> do tracking
      St_DataSetIter next(tpc_data);
      St_tcl_tphit      *tphit = (St_tcl_tphit     *) next("tphit");
      St_tcl_tpc_index  *index = (St_tcl_tpc_index *) next("index");
      if (!index) {index = new St_tcl_tpc_index("index",10*maxNofTracks); next.Add(index);}
      
      St_tpt_track  *tptrack = new St_tpt_track("tptrack",maxNofTracks); m_DataSet->Add(tptrack);
      //tte_e
      St_tte_mctrk  *mctrk   = new St_tte_mctrk("mctrk",maxNofTracks);   m_DataSet->Add(mctrk);
      St_tte_eval *evaltrk   = new St_tte_eval("evaltrk",maxNofTracks);  m_DataSet->Add(evaltrk);
      St_tte_res      *res   = new St_tte_res("res",1);    m_DataSet->Add(res);
      St_tpt_res      *restpt= new St_tpt_res("restpt",10*maxNofTracks);    m_DataSet->Add(restpt);
      St_DataSetIter geant(gStChain->DataSet("geant"));
      St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("g2t_track");
      St_g2t_tpc_hit *g2t_tpc_hit  = (St_g2t_tpc_hit *)geant("g2t_tpc_hit");
      //tpt
      if (!m_iftte) {
	cout << " start tpt_run " << endl;
        Int_t Res_tpt = tpt(m_tpt_pars,tphit,tptrack);
        if (Res_tpt != kSTAFCV_OK) {cout << "Problem with tpt.." << endl;}
	cout << " finish tpt_run " << endl;
	
	cout << "start run_tpt_residuals" << endl;
	Int_t Res_tpt_res = tpt_residuals(tphit,tptrack,restpt);
	if (Res_tpt_res != kSTAFCV_OK) {cout << "Problem with tpt_residuals...." << endl;}
	cout << "finish run_tpt_residuals" << endl;
      }
      else {//tte_track
	if (g2t_tpc_hit && g2t_track) {
	  cout << " start run_tte_track " << endl;
	  Int_t Res_tte_track =  tte_track(tptrack,tphit,g2t_tpc_hit,g2t_track,index,m_type);
	  if (Res_tte_track != kSTAFCV_OK) {cout << " Problem running tte_track " << endl;}
	  cout << " finish run_tte_track " << endl; 
	}
      }
      //tid
      cout << " start tid_run " << endl;
      Int_t Res_tde = tde_new(m_tdeparm,tphit,tptrack,m_tpg_pad_plane);
      if (Res_tde != kSTAFCV_OK) {cout << " Problem with tde_new.. " << endl;}
      cout << " finish tid_run " << endl;
      //tte
      if (g2t_tpc_hit && g2t_track) {
	cout << " start run_tte " << endl;
	Int_t Res_tte = tte(tptrack,tphit,
			    g2t_tpc_hit,g2t_track,
			    index,m_type,evaltrk,mctrk,res,m_tte_control);
	if (Res_tte != kSTAFCV_OK) {cout << " Problem with tte.. " << endl;}
	cout << " finish run_tte " << endl;
      }
    }
  }
   MakeHistograms(); // tracking histograms
  return kStOK;
}
void St_tpt_Maker::MakeHistograms() {
  // Create an iterator
St_DataSetIter tpc_tracks(gStChain->DataSet("tpc_tracks"));
//Get the table:
St_tpt_track *tpr = 0;
tpr               = (St_tpt_track *) tpc_tracks.Find("tptrack");
 if (tpr) {
tpt_track_st *r = tpr->GetTable();
 for(Int_t i=0; i<tpr->GetNRows();i++,r++){
Int_t flag = r->flag;
Float_t rnrec = r->nrec;
Float_t rnfit = r->nfit;
 if(flag>0) {
m_hits_on_track->Fill(rnrec);
m_hits_in_fit->Fill(rnfit);
m_azimuth->Fill(r->psi);
m_tan_dip->Fill(r->tanl);      
m_r0->Fill(r->r0);
 }         
 }
 }
}
//_____________________________________________________________________________
void St_tpt_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_tpt_Maker.cxx,v 1.13 1999/01/12 19:50:19 sakrejda Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

