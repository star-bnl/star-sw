// $Id: St_tpt_Maker.cxx,v 1.23 1999/03/17 02:02:58 fisyak Exp $
// $Log: St_tpt_Maker.cxx,v $
// Revision 1.23  1999/03/17 02:02:58  fisyak
// New scheme
//
// Revision 1.22  1999/03/14 00:23:38  perev
// New makers
//
// Revision 1.21  1999/03/01 18:24:07  sakrejda
// evaluation and residuals calculation made switchable
//
// Revision 1.20  1999/02/26 20:03:59  didenko
// fixed stupid mistake
//
// Revision 1.19  1999/02/26 19:24:15  didenko
// fixed histogram maker
//
// Revision 1.18  1999/02/26 17:25:38  kathy
// fix histograms
//
// Revision 1.17  1999/02/26 00:08:18  didenko
// fixed bug
//
// Revision 1.16  1999/02/25 20:55:31  love
// ntuple named final added
//
// Revision 1.15  1999/01/25 05:57:56  sakrejda
// obsolete table removed from the tte call
//
// Revision 1.14  1999/01/13 17:41:17  sakrejda
// tabs added by Yuri so it looks better
//
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
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_Module.h"
#include "tpc/St_tpt_residuals_Module.h"
#include "tpc/St_tte_track_Module.h"
#include "tpc/St_tde_new_Module.h"
#include "tpc/St_tte_Module.h"
#include "TH1.h"
#include "TNtuple.h"
#include "St_type_index_Table.h"
ClassImp(St_tpt_Maker)
  
  //_____________________________________________________________________________
  St_tpt_Maker::St_tpt_Maker(const char *name):
    StMaker(name),
    m_tpg_pad_plane(0),
    m_type(0),
    m_tpt_pars(0),
    m_tpt_spars(0),
    m_tte_control(0),
    m_tdeparm(0),
    m_tpipar(0)
{
  m_iftteTrack =kFALSE;
  m_tteEvalOn=kTRUE;
  m_tptResOn=kFALSE;
  m_mkfinal=kFALSE;
}
//_____________________________________________________________________________
St_tpt_Maker::~St_tpt_Maker(){}
//_____________________________________________________________________________
Int_t St_tpt_Maker::Init(){
  // Create tables
  
  St_DataSet *tpcpars = GetInputDB("params/tpc");
  assert(tpcpars);
  
  St_DataSetIter       gime(tpcpars);
  
// 		TPG parameters
  m_tpg_pad_plane = (St_tpg_pad_plane *) gime("tpgpar/tpg_pad_plane");
  if (!(m_tpg_pad_plane)) Error("Init","tpc/tpgpar is not initialized. \n");
  assert(m_tpg_pad_plane);
  
// 		TCL parameters
  m_type = (St_tcl_tpc_index_type *) gime("tclpars/type");
  if (!m_type) Error("Init"," Clustering parameters have not been initialized");
  assert(m_type);
  
// 		TPT parameters
  m_tpt_pars  = (St_tpt_pars* ) gime("tptpars/tpt_pars" );
  m_tpt_spars = (St_tpt_spars*) gime("tptpars/tpt_spars");
  if (!(m_tpt_pars && m_tpt_spars)) 
    Error("Init", "tpt parameters have not been initialized" );
  assert(m_tpt_pars && m_tpt_spars);
  
// 		TTE parameters
  m_tte_control = (St_tte_control *) gime("ttepars/tte_control");
  assert(m_tte_control);
  
// 		TID parameters
  m_tdeparm = (St_tdeparm*) gime("tidpars/tdeparm");
  m_tpipar  = (St_tpipar* ) gime("tidpars/tpipar" );
  if (!(m_tdeparm && m_tpipar)) 
    Error("Init","tid parameters have not been initialized");
  assert(m_tdeparm && m_tpipar);
  
// 		Create Histograms

  m_hits_on_track = new TH1F("TptrackHitsOnTrk","Number of hits on reconstructed tracks",50,.5,50.5);
  m_hits_in_fit   = new TH1F("TptrackHitsInFit","Number of hits used in the momentum fit",50,.5,50.5);
  m_azimuth       = new TH1F("TptrackPhi"      ,"Azimuthal distribution of tracks",60,0.,360.0);
  m_tan_dip       = new TH1F("TptrackTanDip"   ,"Distribution of the dip angle",100,-1.5,1.5);
  m_r0            = new TH1F("TptrackR0"       ,"Radius for the first point",100,50.0,200);
  m_invp          = new TH1F("TptrackInvpt"    ,"1/Pt inverse momentum",100,0.0,10.0);

// 		Create ntuple
  m_final = new TNtuple("final","Tpctest tracks and hits",
     "evno:alpha:lambda:row:x:y:z:track:cluster:q:xave:sigma:zrf:prf:nfit:invp:psi:phi0:r0:tanl:z0:chisqxy:chisqz:nseq");

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_tpt_Maker::Make(){
  
  St_DataSet *tpc_data =  GetInputDS("tpc_hits");
  
  if (!tpc_data) return 0;
  
// 		Clusters exist -> do tracking
  St_DataSetIter gime(tpc_data);
  St_tcl_tphit     *tphit = (St_tcl_tphit     *) gime("tphit");
  St_tcl_tpc_index *index = (St_tcl_tpc_index *) gime("index");
  if (!index) {index = new St_tcl_tpc_index("index",10*maxNofTracks);  m_DataSet->Add(index);}
      
  St_tpt_track  *tptrack = new St_tpt_track("tptrack",maxNofTracks); m_DataSet->Add(tptrack);


//			TPT
  if (!m_iftteTrack) {
    if (Debug()) cout << " start tpt_run " << endl;
    Int_t Res_tpt = tpt(m_tpt_pars,tphit,tptrack);
//                      ==============================
    
    if (Res_tpt != kSTAFCV_OK) {cout << "Problem with tpt.." << endl;}
    if (Debug()) cout << " finish tpt_run " << endl;

  }  else {//tte_track
    St_DataSet *geant = GetInputDS("geant");
    if (geant) {
      St_DataSetIter geantI(geant);
      
      St_g2t_track   *g2t_track    = (St_g2t_track  *) geantI("g2t_track");
      St_g2t_tpc_hit *g2t_tpc_hit  = (St_g2t_tpc_hit *)geantI("g2t_tpc_hit");
      if (g2t_tpc_hit && g2t_track) {
	if (Debug()) cout << "start run_tpt_residuals" << endl;
	Int_t Res_tte_track =  tte_track(tptrack,tphit,g2t_tpc_hit,g2t_track,index,m_type);
	if (Res_tte_track != kSTAFCV_OK) {cout << " Problem running tte_track " << endl;}
	if (Debug()) cout << " finish run_tte_track " << endl; 
      }
    }
  }

//		Calculate residuals
  if(m_tptResOn){	

//		Set up table for residuals
    St_tpt_res      *restpt= new St_tpt_res("restpt",10*maxNofTracks);
    m_DataSet->Add(restpt);
    if (Debug()) cout << "start run_tpt_residuals" << endl;
    Int_t Res_tpt_res = tpt_residuals(tphit,tptrack,restpt);
//			===================================
    if (Res_tpt_res != kSTAFCV_OK) {cout << "Problem with tpt_residuals...." << endl;}
    else {if (Debug()) cout << "finish run_tpt_residuals" << endl;}
  }

//		End of residuals calculations

//		TID
  if (Debug()) cout << " start tid_run " << endl;
  Int_t Res_tde = tde_new(m_tdeparm,tphit,tptrack,m_tpg_pad_plane);
//	   	      ================================================
  if (Res_tde != kSTAFCV_OK) {cout << " Problem with tde_new.. " << endl;}
  else {if (Debug()) cout << " finish tid_run " << endl;}


//		TTE
  St_DataSet *geant = GetInputDS("geant");
  if (geant) {
    St_DataSetIter geantI(geant);
    
    St_g2t_track   *g2t_track    = (St_g2t_track  *) geantI("g2t_track");
    St_g2t_tpc_hit *g2t_tpc_hit  = (St_g2t_tpc_hit *)geantI("g2t_tpc_hit");
    if (g2t_tpc_hit && g2t_track) {
      if (Debug()) cout << " start run_tte " << endl;
      //		If tte on, create evaluation tables
      St_tte_mctrk  *mctrk   = new St_tte_mctrk("mctrk",maxNofTracks);
      m_DataSet->Add(mctrk);
      St_tte_eval *evaltrk   = new St_tte_eval("evaltrk",maxNofTracks);
      m_DataSet->Add(evaltrk);

      Int_t Res_tte = tte(tptrack,tphit,
			  g2t_tpc_hit,g2t_track,
			  index,m_type,evaltrk,mctrk,m_tte_control);
//		    ==============================================
    
      if (Res_tte != kSTAFCV_OK) {cout << " Problem with tte.. " << endl;}
      else {if (Debug()) cout << " finish run_tte " << endl;}
    }
  }

  MakeHistograms(); // tracking histograms
  return kStOK;
}
//_____________________________________________________________________________
  void St_tpt_Maker::MakeHistograms() {
   // go get event number from the event data
   Int_t evno = 0;
   if (m_mkfinal) {

     St_DataSet *raw = GetInputDS("TPC_DATA");
  if (raw) {
    St_DataSetIter nex(raw);
    St_type_index *I1 = (St_type_index *) nex("IT1");
    type_index_st *ii = I1->GetTable();
    evno = ii->data_row;
     }
  }

  // Create an iterator
  St_DataSetIter tpc_tracks(m_DataSet);
  //Get the track table:
  St_tpt_track *tpr = 0;
  tpr               = (St_tpt_track *) tpc_tracks.Find("tptrack");
  if (tpr) {
    tpt_track_st *r = tpr->GetTable();
    for(Int_t i=0; i<tpr->GetNRows();i++,r++){
      if(!r->flag>0) 	continue;
      m_hits_on_track->Fill((float)r->nrec);
      m_hits_in_fit->Fill((float)r->nfit);
      m_azimuth->Fill(r->psi);
      m_tan_dip->Fill(r->tanl);      
      m_r0->Fill(r->r0);
      m_invp->Fill(r->invp);
    }         
  }
  //  Make the "Final" hit and track ntuple  Should be controllable.
  if (m_mkfinal) {
    St_tcl_tphit  *n_hit = 0;
    St_tcl_tpcluster *n_clus  = 0;
    St_tcl_tphit_aux *n_hitau = 0;
    St_DataSet *tpc_hits = GetInputDS("tpc_hits");
    if (tpc_hits) {
      St_DataSetIter tpc_data(tpc_hits);
      n_hit      = (St_tcl_tphit *) tpc_data["hits/tphit"];
      n_clus     = (St_tcl_tpcluster *)  tpc_data["hits/tpcluster"];
      n_hitau    = (St_tcl_tphit_aux *) tpc_data["hits/tphitau"];
    }
    if(n_hit){
      St_tpt_track * n_track    = (St_tpt_track *) tpc_tracks["tracks/tptrack"];
      tcl_tphit_st *h = n_hit->GetTable(); 
      for (int i = 0;i<n_hit->GetNRows();i++,h++){
	tcl_tphit_aux_st *au =  n_hitau->GetTable();
	for(int iau=0;iau<n_hitau->GetNRows();iau++,au++){
	  if(au->id != h->id) continue;
	  // cluster variable is one more than row num in cluster table
	  tcl_tpcluster_st *clu = n_clus->GetTable();
	  clu += h->cluster -1;
	  if(h->track/1000 != 0){
	    //find the track, if any
	    tpt_track_st *t = n_track->GetTable(); 
	    for(int itk=0;itk<n_track->GetNRows();itk++,t++){
	      if(t->id != h->track/1000) continue;
	      //   TNtuple *final = new TNtuple("final","Tpctest tracks and hits",
	      //     "evno:alpha:lambda:row:x:y:z:track:cluster:q:xave:sigma:zrf:prf:nfit:invp:psi:phi0:r0:tanl:z0:chisqxy:chisqz:nseq");
	      
	      Float_t row[] = {evno,h->alpha,h->lambda,
			       h->row,h->x,h->y,h->z,h->track,h->cluster,h->q,
			       au->xave,au->sigma,h->zrf,h->prf,
			       t->nfit,t->invp,t->psi,t->phi0,t->r0,t->tanl,
			       t->z0,t->chisq[0],t->chisq[1],clu->nseq};
	      m_final->Fill(row);
	    } //end of itk for loop
	  } //end of if h->track/1000 
	  else{
	    Float_t row[] = {evno,h->alpha,h->lambda,
                             h->row,h->x,h->y,h->z,h->track,h->cluster,h->q,
			     au->xave,au->sigma,h->zrf,h->prf,0,0,0,0,0,
			     0,0,0,0,clu->nseq};
	    m_final->Fill(row);
	  } // end of no track else
	} // end of hit_aux table loop
      }  // end of hit loop
    }
  }// end of if on m_mkfinal flag.
}  // end of MakeHistograms member.
//_____________________________________________________________________________
void St_tpt_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_tpt_Maker.cxx,v 1.23 1999/03/17 02:02:58 fisyak Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

