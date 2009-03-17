// $Id: St_tpt_Maker.cxx,v 1.80 2009/03/17 22:37:48 fisyak Exp $
// $Log: St_tpt_Maker.cxx,v $
// Revision 1.80  2009/03/17 22:37:48  fisyak
// Clean up
//
// Revision 1.79  2007/05/17 14:13:42  fisyak
// replace printf and  cout by logger printouts
//
// Revision 1.78  2007/04/28 17:57:23  perev
// Redundant StChain.h removed
//
// Revision 1.77  2005/01/13 21:43:15  jeromel
// Why 3 ?? Extend mask to sizeof()=4
//
// Revision 1.76  2004/08/07 03:01:24  perev
// TF1 cleanup
//
// Revision 1.75  2004/06/05 23:40:05  fisyak
// Add (sector,row) for TpcCoordinate transformations
//
// Revision 1.74  2004/01/27 03:47:35  jeromel
// Indent and define TPT_CORRECTION
//
// Revision 1.73  2003/09/02 17:59:31  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.72  2002/02/22 19:20:28  hardtke
// change order of instantiation for Jim
//
// Revision 1.70  2002/02/05 22:22:15  hardtke
// Move Init code to InitRun
//
// Revision 1.69  2002/02/02 00:52:03  jeromel
// Modified mask (extended options). Instantiaion f MagUtilities() uses db.
//
// Revision 1.68  2001/10/25 23:02:00  hardtke
// 2 changes: 1) Use db in constructor for StMagUtilities, 2) Invert order of transformations.  First align sectors, then undo distortions, then transform to global coordinates
//
// Revision 1.67  2001/10/06 05:20:02  jeromel
// 0x-ing printf()
//
// Revision 1.66  2001/10/04 22:33:34  jeromel
// Option mask changed to 0xFE to accomodate for soon-to-come changes (more options).
//
// Revision 1.65  2001/09/06 18:27:37  jeromel
// Modifications for larger number of ExB options, forcing different configuration 9EB1 EB2 ...). Added loading of StTableUtilities when 'display' option is required.
//
// Revision 1.64  2001/08/31 20:45:54  hardtke
// Add bums hit moving option
//
// Revision 1.63  2001/08/08 20:11:43  jeromel
// Added debugging lines for ExB correction option. WAS NEVER ON ==> Corrected & -> | (i.e. mea culpa)
//
// Revision 1.62  2001/08/03 16:22:41  jeromel
// Enabled option for StMagUtilities()
//
// Revision 1.61  2001/08/01 00:55:36  jeromel
// Code modification (incomplete) for Jim's requested option at StMagUtilities() constructor level
//
// Revision 1.60  2001/05/01 21:47:28  wdeng
// St_tpt_Maker.cxx
//
// Revision 1.59  2001/04/23 17:31:52  wdeng
// Create a temporary sorted tcl_tphit table for function estimateVertexZ. Delete it after calling the function.
//
// Revision 1.58  2001/04/23 17:08:28  didenko
// restore right revision
//
// Revision 1.56  2001/02/08 20:37:15  saulys
// Update call to ExB
//
// Revision 1.55  2000/11/25 23:22:51  fisyak
// move dEdx calculations into StdEdxMaker
//
// Revision 1.54  2000/11/03 21:23:34  saulys
// Added ExB correction code
//
// Revision 1.53  2000/07/26 00:53:37  sakrejda
// Pre-vertex info added to the tpt module
//
// Revision 1.52  2000/06/23 19:40:27  fisyak
// remove access to params
//
// Revision 1.51  2000/06/22 21:07:10  wdeng
// New iflag for cluster vertex.
//
// Revision 1.50  2000/06/20 19:21:44  wdeng
// Plug in function estimateVertexZ.
//
// Revision 1.49  2000/06/15 19:06:15  aihong
// ensemble truncation for de/dx calculation added
//
// Revision 1.48  2000/05/30 19:19:53  sakrejda
// A debyg cout statement removed
//
// Revision 1.47  2000/03/07 05:14:54  sakrejda
// Jan's modifications to run on L3 clusters (I think)
//
// Revision 1.46  2000/02/23 23:04:43  hardtke
// get tpg tables from tpcDB
//
// Revision 1.45  2000/02/23 03:41:39  sakrejda
// Histograms names in EVAL changed to TptTte* by Kathy.
// Also comments added.
//
// Revision 1.44  1999/12/17 23:40:49  sakrejda
// Add message suppression:
// gMessMgr->SetLimit("TPTRSP-E1",10);
//
// Revision 1.43  1999/11/19 02:01:40  sakrejda
// if (! tphit) return kStWarn;
// added after:
// St_tcl_tphit     *tphit = (St_tcl_tphit     *) gime("tphit");
// to protect against empty events
//
// Revision 1.42  1999/11/09 20:38:56  fisyak
// Fix for ROOT 2.23
//
// Revision 1.41  1999/09/24 01:23:47  fisyak
// Reduced Include Path
//
// Revision 1.40  1999/09/04 15:42:41  fine
// St_XDFFIle include has been removed to break dependences
//
// Revision 1.39  1999/08/19 21:09:51  sakrejda
// limit on annoying messages set to 10
//
// Revision 1.38  1999/08/12 17:04:12  ogilvie
// changed to tde, added tpc_dedx table
//
// Revision 1.37  1999/08/07 19:47:07  fisyak
// cleanup check of kFALSE
//
// Revision 1.36  1999/07/15 13:58:27  perev
// cleanup
//
// Revision 1.35  1999/06/02 01:28:22  sakrejda
// comment before tte_track corrected (was about residuals instead of tte_track)
//
// Revision 1.34  1999/05/21 21:40:34  liq
// set protection for no selected tracks from mctrk
//
// Revision 1.33  1999/05/21 14:41:18  sakrejda
// tte is called now only if the evaluation flag is on
//
// Revision 1.32  1999/05/21 02:02:19  liq
// set protections on table pointers
//
// Revision 1.31  1999/05/07 15:52:06  liq
// change names of hist., and set protection for non-MC data
//
// Revision 1.30  1999/05/06 19:01:35  liq
// set m_tteEvalOn=kFALSE;
//
// Revision 1.29  1999/05/05 18:45:40  liq
// include valuation plots of reconstraction
//
// Revision 1.28  1999/04/23 19:03:30  sakrejda
// One more scope problem, type definition removed from the if scope
//
// Revision 1.26  1999/04/22 18:50:05  sakrejda
// a protection in case IT1 does not exist
//
// Revision 1.25  1999/03/30 03:08:43  sakrejda
// remanents of the auxiliary table removed
//
// Revision 1.24  1999/03/20 23:30:00  perev
// new maker schema
//
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

#include <Stiostream.h>
#include <math.h>
#include "St_tpt_Maker.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_Module.h"
#include "tpc/St_tpt_residuals_Module.h"
#include "tpc/St_tte_track_Module.h"
#include "tpc/St_tte_Module.h"
#include "tpc/St_tfs_g2t_Module.h"  
#include "TH1.h"
#include "TF1.h"
#include "TH2.h"
#include "TH3.h"
#include "TNtuple.h"
#include "TTableSorter.h"
#include "TTableIter.h"
#include "tables/St_type_index_Table.h"
#include "tables/St_dst_vertex_Table.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StCoordinates.hh"
#include "TString.h"

void estimateVertexZ(St_tcl_tphit *tphit, Float_t& vertexZ, Float_t& relativeHeight);
 
ClassImp(St_tpt_Maker)
  
  //_____________________________________________________________________________
  St_tpt_Maker::St_tpt_Maker(const char *name):
    StMaker(name),
    m_tpg_pad_plane(0),
    m_type(0),
    m_tpt_pars(0),
    m_tpt_spars(0),
    m_tte_control(0)
{
  m_iftteTrack =kFALSE;
  m_tteEvalOn=kFALSE;
  m_tptResOn=kFALSE;
  m_mkfinal=kFALSE;
  {gMessMgr->QAInfo() << Form("\n TPT CONSTRUCTOR name=\"%s\"\n",GetName()) << endm;}
  SetInputHits("tpc_hits","tphit"); // initialize default input
}
//_____________________________________________________________________________
void St_tpt_Maker:: SetInputHits(  TString DataSet,  TString Hit)
{
  m_InputDataSetName=DataSet;
  m_InputHitName=Hit;
  {gMessMgr->QAInfo() << Form("%s.SetInputHits to: DataSet=\"%s\", Hit=\"%s\"\n",GetName(),m_InputDataSetName.Data(), m_InputHitName.Data()) << endm;}
}

//_____________________________________________________________________________
St_tpt_Maker::~St_tpt_Maker(){}
//_____________________________________________________________________________
Int_t St_tpt_Maker::Init(){
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_tpt_Maker::InitRun(int runnumber){
  //Suppress annoying messages
  gMessMgr->SetLimit("TPTROOT1-E1",10);
  gMessMgr->SetLimit("TPTOUT2-E1",10);
  gMessMgr->SetLimit("TPTRSP-E1",10);

  // Create tables
  St_DataSet *tpcpars = GetInputDB("tpc");
  assert(tpcpars);
  
  St_DataSetIter       gime(tpcpars);
  
  // 		TPG parameters
  m_tpg_pad_plane = (St_tpg_pad_plane *)  GetDataSet("tpcDB/.const/tpg_pad_plane");
  if (!(m_tpg_pad_plane)) Error("Init","no tpg_pad_plane is not initialized. \n");
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
  
  // 	 	TTE parameters
  m_tte_control = (St_tte_control *) gime("ttepars/tte_control");
  assert(m_tte_control);
  
  // 		Create Histograms


  // reconstructed track histograms from values calculated in St_tpt_Maker:
  m_hits_on_track = new TH1F("TptrackHitsOnTrk","Number of hits on reconstructed tracks",50,.5,50.5);
  m_hits_in_fit   = new TH1F("TptrackHitsInFit","Number of hits used in the momentum fit",50,.5,50.5);
  m_azimuth       = new TH1F("TptrackPhi"      ,"Azimuthal distribution of tracks",60,0.,360.0);
  m_tan_dip       = new TH1F("TptrackTanDip"   ,"Distribution of the dip angle",100,-1.5,1.5);
  m_r0            = new TH1F("TptrackR0"       ,"Radius for the first point",100,50.0,200);
  m_invp          = new TH1F("TptrackInvpt"    ,"1/Pt inverse momentum",100,0.0,10.0);

  // 		Create ntuple
  m_final = new TNtuple("final","Tpctest tracks and hits",
     "evno:alpha:lambda:row:x:y:z:track:cluster:q:xave:sigma:zrf:prf:nfit:invp:psi:phi0:r0:tanl:z0:chisqxy:chisqz:nseq");
  // xave and sigma are not filled anymore because they came from aux table (RAI)

  // if m_tteEvalOn=kTrue, then initialize the histograms of the efficiency and momentum resolution
  //  gMessMgr->QAInfo() <<"kFALSE="<<kFALSE<<endm;;

  if(m_tteEvalOn){VertexEffResolutionInit();}
  return kStOK;
}

//_____________________________________________________________________________
Int_t St_tpt_Maker::Make(){

  {gMessMgr->QAInfo() << Form("\n TPT=\"%s\" Input is: DataSet=\"%s\", Hit=\"%s\"\n",GetName(),m_InputDataSetName.Data(), m_InputHitName.Data()) << endm;}
  
  St_DataSet *tpc_data =  GetInputDS(m_InputDataSetName);
  
  if (!tpc_data) return 0;
  
  // 		Clusters exist -> do tracking
  //tpc_data->ls(4);
  //gMessMgr->QAInfo() << Form("DEBUG2 :: [%s]\n",m_InputHitName.Data()) << endm;
  St_DataSetIter gime(tpc_data);
  St_tcl_tphit     *tphit = (St_tcl_tphit     *) gime(m_InputHitName);
  if (! tphit) return kStWarn;
  gMessMgr->QAInfo() << Form(" Input hit table size is %d\n\n",(int)tphit->GetNRows()) << endm;

  ///!!!!!!!!!!
  dst_vertex_st dstVertexRow;
  ::memset(&dstVertexRow, 0, sizeof(dstVertexRow));
  dstVertexRow.z = 0;
  dstVertexRow.id = 1;
  dstVertexRow.iflag = 105;
  dstVertexRow.det_id = 1;
  dstVertexRow.vtx_id = 1;

  St_dst_vertex  *clusterVertex = new St_dst_vertex("clusterVertex",1); 
  m_DataSet->Add(clusterVertex);
  clusterVertex->AddAt(&dstVertexRow, 0);
  ///!!!!!!!!!! Dummy cluster vertex since module tpt interface needs it.


  St_tcl_tpc_index *index = (St_tcl_tpc_index *) gime("index");
  if (!index) {index = new St_tcl_tpc_index("index",10*maxNofTracks);  m_DataSet->Add(index);}
      
  St_tpt_track  *tptrack = new St_tpt_track("tptrack",maxNofTracks); m_DataSet->Add(tptrack);
  //			TPT
  if (!m_iftteTrack) {

    if (Debug()) {gMessMgr->QAInfo()  << " start tpt_run " << endm;}
    Int_t Res_tpt = tpt(m_tpt_pars,tphit,tptrack,clusterVertex);
    //                      ==============================
    
    if (Res_tpt != kSTAFCV_OK) {gMessMgr->QAInfo()  << "Problem with tpt.." << endm;}
    if (Debug()) {gMessMgr->QAInfo()  << " finish tpt_run " << endm;}
    
  } 
  else 
    {//tte_track
      St_DataSet *geant = GetInputDS("geant");
      if (geant) {
	St_DataSetIter geantI(geant);
      
      St_g2t_track   *g2t_track    = (St_g2t_track  *) geantI("g2t_track");
      St_g2t_tpc_hit *g2t_tpc_hit  = (St_g2t_tpc_hit *)geantI("g2t_tpc_hit");
      if (g2t_tpc_hit && g2t_track) {
	if (Debug()) gMessMgr->QAInfo()  << "start run_tte_track" << endm;
	Int_t Res_tte_track =  tte_track(tptrack,tphit,g2t_tpc_hit,g2t_track,index,m_type);
	if (Res_tte_track != kSTAFCV_OK) {gMessMgr->QAInfo()  << " Problem running tte_track " << endm;}
	if (Debug()) {gMessMgr->QAInfo()  << " finish run_tte_track " << endm;}
      }
    }
  }

//		Calculate residuals
  if(m_tptResOn){	

//		Set up table for residuals
    St_tpt_res      *restpt= new St_tpt_res("restpt",10*maxNofTracks);
    m_DataSet->Add(restpt);
    if (Debug()) {gMessMgr->QAInfo()  << "start run_tpt_residuals" << endm;}
    Int_t Res_tpt_res = tpt_residuals(tphit,tptrack,restpt);
//			===================================
    if (Res_tpt_res != kSTAFCV_OK) {gMessMgr->QAInfo()  << "Problem with tpt_residuals...." << endm;}
    else {if (Debug()) {gMessMgr->QAInfo()  << "finish run_tpt_residuals" << endm;}}
  }

//		End of residuals calculations

//		TTE
  if(m_tteEvalOn){
  St_DataSet *geant = GetInputDS("geant");
  if (geant) {
    St_DataSetIter geantI(geant);
    
    St_g2t_track   *g2t_track    = (St_g2t_track  *) geantI("g2t_track");
    St_g2t_tpc_hit *g2t_tpc_hit  = (St_g2t_tpc_hit *)geantI("g2t_tpc_hit");
    if (g2t_tpc_hit && g2t_track) {
      if (Debug()) {gMessMgr->QAInfo()  << " start run_tte " << endm;}
      //		If tte on, create evaluation tables
      St_tte_mctrk  *mctrk   = new St_tte_mctrk("mctrk",maxNofTracks);
      m_DataSet->Add(mctrk);
      St_tte_eval *evaltrk   = new St_tte_eval("evaltrk",maxNofTracks);
      m_DataSet->Add(evaltrk);

      Int_t Res_tte = tte(tptrack,tphit,
			  g2t_tpc_hit,g2t_track,
			  index,m_type,evaltrk,mctrk,m_tte_control);
//		    ==============================================
    
      if (Res_tte != kSTAFCV_OK) {gMessMgr->QAInfo()  << " Problem with tte.. " << endm;}
      else {if (Debug()) gMessMgr->QAInfo()  << " finish run_tte " << endm;}
    }
  }
// Calculate  efficiency and the momentum resolution
  VertexEffResolutionMakeHistograms();
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
    type_index_st *ii = 0;
    if(I1) ii = I1->GetTable();
    if(ii)evno = ii->data_row;
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
    //    St_tcl_tphit_aux *n_hitau = 0; // aux table 
    St_DataSet *tpc_hits = GetInputDS("tpc_hits");
    if (tpc_hits) {
      St_DataSetIter tpc_data(tpc_hits);
      n_hit      = (St_tcl_tphit *) tpc_data["tphit"];
      n_clus     = (St_tcl_tpcluster *)  tpc_data["tpcluster"];
      //      n_hitau    = (St_tcl_tphit_aux *) tpc_data["tphitau"]; //aux table
    }
    if(n_hit){
      St_tpt_track * n_track    = (St_tpt_track *) tpc_tracks["tptrack"];
      tcl_tphit_st *h = n_hit->GetTable(); 
      for (int i = 0;i<n_hit->GetNRows();i++,h++){
	//	tcl_tphit_aux_st *au =  n_hitau->GetTable(); //aux table
	//	for(int iau=0;iau<n_hitau->GetNRows();iau++,au++){ //aux table
	//	  if(au->id != h->id) continue; //aux table
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
			       //au->xave,au->sigma,h->zrf,h->prf, //aux table
			       0,0,h->zrf,h->prf,
			       t->nfit,t->invp,t->psi,t->phi0,t->r0,t->tanl,
			       t->z0,t->chisq[0],t->chisq[1],clu->nseq};
	      m_final->Fill(row);
	    } //end of itk for loop
	  } //end of if h->track/1000 
	  else{
	    Float_t row[] = {evno,h->alpha,h->lambda,
                             h->row,h->x,h->y,h->z,h->track,h->cluster,h->q,
			     //au->xave,au->sigma,h->zrf,h->prf,0,0,0,0,0, //aux table
			     0,0,h->zrf,h->prf,0,0,0,0,0,
			     0,0,0,0,clu->nseq};
	    m_final->Fill(row);
	  } // end of no track else
	  //	} // end of hit_aux table loop
      }  // end of hit loop
    }
  }// end of if on m_mkfinal flag.
}  // end of MakeHistograms member.
//_____________________________________________________________________________
void St_tpt_Maker::VertexEffResolutionMakeHistograms() {

  gMessMgr->QAInfo() <<"begin to run St_tpt_Maker::VertexEffResolutionMakeHistogram"<<endm; 

  // Create an iterator
  St_DataSetIter ap(m_DataSet);
  St_tte_mctrk *af=0;
  tte_mctrk_st *wr=0;
  if(m_DataSet){
    af=(St_tte_mctrk  *) ap("mctrk");
    if(af){
      wr=af->GetTable();
      if(!wr){
 	gMessMgr->QAInfo() <<"no information in mctrk, quit from VertexEffResolutionMakeHistogram"<<endm;
	return;
      }
    }else {
      gMessMgr->QAInfo() <<"there is no mctrk, quit from VertexEffResolutionMakeHistogram"<<endm;
      return;
    }
  }else {
    gMessMgr->QAInfo() <<"wrong in get m_DataSet, quit from VertexEffResolutionMakeHistogram"<<endm;
    return;
  }
  
  mevent++;
  gMessMgr->QAInfo() <<"Event number is "<<mevent<<endm; 
  
  //get tpc_tracks/evaltrk table
  St_tte_eval *aeval=(St_tte_eval  *) ap("evaltrk");
  tte_eval_st *weval=0;
  if(aeval){
    weval=aeval->GetTable();
    if(!weval){
      gMessMgr->QAInfo() <<"no information in evaltrk,quit from VertexEffResolutionMakeHistogram"<<endm;
      return;
    }
  }else {
    gMessMgr->QAInfo() <<"there is no evaltrk,quit from VertexEffResolutionMakeHistogram"<<endm;
    return;
  }
  //get g2t_vertex table, and get the position of vertex, fill histograms
  Float_t vertex[3];
  Float_t eg_vertex[3];

  St_DataSet *yyp = GetInputDS("geant");
  St_g2t_vertex *ayf=0;
  g2t_vertex_st *wyr=0;

  if (yyp) {
    St_DataSetIter ayp(yyp) ;
    ayf=(St_g2t_vertex *) ayp("g2t_vertex");
    if(ayf){
      wyr=ayf->GetTable();
      if(!wyr){
	gMessMgr->QAInfo() <<"no information in g2t_vertex,quit from VertexEffResolutionMakeHistogram"<<endm;
        return;
      }
    }else {
      gMessMgr->QAInfo() <<"there is no g2t_vertex,quit from VertexEffResolutionMakeHistogram"<<endm;
      return;
    }
    //wr->eg_label=1 stand for the primary vertex
    for(Int_t ij=0;ij<3;ij++)vertex[ij]=wyr->ge_x[ij];
    for(Int_t ji=0;ji<3;ji++)eg_vertex[ji]=wyr->eg_x[ji];
  }
  else {
    gMessMgr->QAInfo() <<"there is no geant dataset,quit from VertexEffResolutionMakeHistogram"<<endm; 
    return ;
  }
  gMessMgr->QAInfo() <<"event gen. vertex="<<eg_vertex[0]<<eg_vertex[1]<<eg_vertex[2]<<endm;
  gMessMgr->QAInfo() <<"geant vertex="<<vertex[0]<<vertex[1]<<vertex[2]<<endm;
  
  m_vertex_x->Fill(vertex[0]);
  m_vertex_y->Fill(vertex[1]);
  m_vertex_z->Fill(vertex[2]);
  
  m_vertexX_vertexY->Fill(vertex[0],vertex[1]);
  m_vertexX_vertexZ->Fill(vertex[0],vertex[2]);
  
  Float_t vertex_xy=::sqrt(vertex[0]*vertex[0]+vertex[1]*vertex[1]);
  
  m_vertex_xy->Fill(vertex_xy);
  
  //before loop, initialize 
  TH1F *m_rapidity1=new TH1F("m_rapidity1", "rapidity with vid=1,nfst>5", 30,-3.,3.);
  TH1F *m_rapidity2=new TH1F("m_rapidity2", "rapidity with vid=1,nfst>5,nrec>0", 30,-3.,3.);
  TH1F *m_eff1=new TH1F("m_eff1", "eff=rapidity2/rapidity1", 30,-3.,3.);
  
  
  Float_t  total_ptr=0.0;
  Float_t  total_ptg=0.0;
  Float_t  total_rapidity=0.0;
  Float_t  total_chisqxy=0.0;
  Float_t  total_chisqz=0.0;
  
  Int_t nmctrk=0;
  Float_t mctrkdp=0.0;
  Float_t mctrkdpt=0.0;
  
  // loop over mctrk table, and get the rapidity of this events, and efficiency
  for(Int_t i=0;i<af->GetNRows();i++,wr++){
    Int_t pid=wr->pid;
    Float_t ptr=wr->ptr;
    Float_t ptg=wr->ptg;
    Float_t pzg=wr->pzg;
    Float_t pzr=wr->pzr;
    Float_t pg=::sqrt(ptg*ptg+pzg*pzg); 
    Float_t pr=::sqrt(ptr*ptr+pzr*pzr); 
    
    Int_t vid=wr->vid;
    Int_t nfst=wr->nfst;
    Int_t nrec1=wr->nrec1;
    
    if(vid==1&&nfst>5&&(pg-pzg)!=0.0){
      Float_t rapidity=-0.5*::log((pg+pzg)/(pg-pzg));
      m_rapidity1->Fill(rapidity);
      m_rapidity_total1->Fill(rapidity);
      m_ptg_rapidity_1->Fill(ptg,rapidity);
      if(nrec1>0){
	mctrkdp=fabs(pr-pg)/pg;
	mctrkdpt=fabs(ptr-ptg)/ptg;
	m_rapidity2->Fill(rapidity);
	m_rapidity_total2->Fill(rapidity);
	m_ptg_rapidity_2->Fill(ptg,rapidity);
	m_ptg_rapidity_dpt->Fill(ptg,rapidity,mctrkdpt);
	total_ptr+=ptr;
	total_ptg+=ptg;
	total_rapidity+=rapidity;
	nmctrk++;
      } //nrec1>0
    } //end if vid==1&&nfst>5&&(pr-pzg)!=0.0
    
    //get abs(ptg-ptr)/ptg momentum resolution
    if(vid==1&&nfst>5&&nrec1>0&&ptg>0.0){
      mctrkdp=fabs(pr-pg)/pg;
      mctrkdpt=fabs(ptr-ptg)/ptg;
      m_dpt->Fill(mctrkdpt);
      m_dpt_ptg->Fill(ptg,mctrkdpt);
      m_dp->Fill(mctrkdp);
      m_dp_pg->Fill(pg,mctrkdp);
      if(pid==8||pid==9)m_dp_pg_pion->Fill(pg,mctrkdp);  //for pion
      if(pid==14)m_dp_pg_proton->Fill(pg,mctrkdp);   // for proton
      if(pid==11||pid==12)m_dp_pg_kaon->Fill(pg,mctrkdp); //for kaon
    } //end for momentum resolution
  }  //end loop for i
  
  //loop over evaltrk table, get chisq[2]
  Int_t nevaltrk=0;
  for( Int_t ji=0;ji<aeval->GetNRows();ji++,weval++){
    Float_t chisqxy=weval->chisq[0];
    Float_t chisqz=weval->chisq[1];
    Int_t nfit=weval->nfit;
    
    if(ji==0) {gMessMgr->QAInfo() <<"chisqxy="<<chisqxy<<endm;}
    if(ji==0) {gMessMgr->QAInfo() <<"chisqz="<<chisqz<<endm;}
    if(nfit!=3)chisqxy/=(nfit-3.0);
    if(nfit!=2)chisqz/=(nfit-2.0);
    m_chisqxy->Fill(chisqxy);
    m_chisqz->Fill(chisqz);
    total_chisqxy+=chisqxy;
    total_chisqz+=chisqz;
    nevaltrk++;
  }  //end loop for ji
  
  
  //get efficiency
  m_eff1->Divide(m_rapidity2,m_rapidity1,1.0,1.0); 
  Float_t effm=m_eff1->GetBinContent(10);
  gMessMgr->QAInfo() <<"GetBinContent eff="<<effm<<endm; 
  
  
  //fit efficiency per event
  Double_t par[1];
  TF1 pol_0("pol_0","pol0",-1.0,1.0);
  m_eff1->Fit("pol_0","N","",-1.0,1.0); //fit and non_plot
  pol_0.GetParameters(&par[0]);
  
  Float_t efficiency=par[0];
  gMessMgr->QAInfo() <<"For each event:Fit eff0="<<par[0]<<endm; 
  m_averge_eff->Fill(efficiency);
  
  //get average pt,rapidity,chisqxy,chisqz
  gMessMgr->QAInfo() <<"No. of tracks selected in mctrk="<<nmctrk<<endm;
  gMessMgr->QAInfo() <<"No. of tracks in evaltrk="<<nevaltrk<<endm;
  
  if(!nmctrk){
    gMessMgr->QAInfo() <<"No. of tracks selected in mctrk=0,quit from VertexEffResolutionMakeHistogram "<<endm;     
    return;
  }
  Float_t average_ptr=total_ptr/nmctrk;
  Float_t average_ptg=total_ptg/nmctrk;
  gMessMgr->QAInfo() <<"total_ptr="<<total_ptr<<"average_ptr="<<average_ptr<<endm;
  gMessMgr->QAInfo() <<"total_ptg="<<total_ptg<<"average_ptg="<<average_ptg<<endm;
  Float_t average_rapidity=total_rapidity/nmctrk;
  gMessMgr->QAInfo() <<"total_rapidity="<<total_rapidity<<"  average_rapidity="<<average_rapidity<<endm;

  if(!nevaltrk){
    gMessMgr->QAInfo() <<"No. of tracks selected in evaltrk=0,quit from VertexEffResolutionMakeHistogram "<<endm;  
    return;
  }
  Float_t average_chisqxy=total_chisqxy/nevaltrk;
  gMessMgr->QAInfo() <<"total_chisqxy="<<total_chisqxy<<"  average_chisqxy="<<average_chisqxy<<endm;
  Float_t average_chisqz=total_chisqz/nevaltrk;
  gMessMgr->QAInfo() <<"total_chisqz="<<total_chisqz<<"  average_chisqz="<<average_chisqz<<endm;
  
  //fill histograms
  m_average_ptr->Fill(average_ptr);
  m_average_ptg->Fill(average_ptg);
  m_average_rapidity->Fill(average_rapidity);
  m_average_chisqxy->Fill(average_chisqxy);
  m_average_chisqz->Fill(average_chisqz);
  
  m_vertexXY_average_ptr->Fill(vertex_xy,average_ptr);
  m_vertexXY_average_ptg->Fill(vertex_xy,average_ptg);
  m_vertexXY_average_rapidity->Fill(vertex_xy,average_rapidity);
  m_vertexXY_average_chisqxy->Fill(vertex_xy,average_chisqxy);
  m_vertexXY_average_chisqz->Fill(vertex_xy,average_chisqz);
  
  m_vertexZ_average_ptr->Fill(vertex[2],average_ptr);
  m_vertexZ_average_ptg->Fill(vertex[2],average_ptg);
  m_vertexZ_average_rapidity->Fill(vertex[2],average_rapidity);
  m_vertexZ_average_chisqxy->Fill(vertex[2],average_chisqxy);
  m_vertexZ_average_chisqz->Fill(vertex[2],average_chisqz);
  
  m_vertexXY_eff->Fill(vertex_xy,efficiency);
  m_vertexZ_eff->Fill(vertex[2],efficiency);
  
  delete m_rapidity1;
  delete m_rapidity2;
  delete m_eff1;
  
  gMessMgr->QAInfo() <<"at the end of the VertexEffResolutionMakeHistogram"<<endm;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t St_tpt_Maker::Finish(){
  // if m_tteEvalOn=kTrue, then  calculate  the efficiency and momentum resolution
  if(m_tteEvalOn){VertexEffResolutionFinish();}
  return kStOK;
}
//_____________________________________________________________________________
void St_tpt_Maker::VertexEffResolutionFinish(){
  gMessMgr->QAInfo() <<"begin to run StVertexEffMaker::Finish"<<endm; 
  m_eff_total->Divide(m_rapidity_total2,m_rapidity_total1,1.0,1.0); 
  //fit efficiency for all events
  Double_t par[1];
  TF1 pol_0("pol_0","pol0",-1.0,1.0);
  m_eff_total->Fit("pol_0","N","",-1.0,1.0); //fit and non_plot
  pol_0.GetParameters(&par[0]);
  
  Float_t efficiency=par[0];
  gMessMgr->QAInfo() <<"For all events:Fit eff="<<efficiency<<endm; 
  
  //divide to get efficiency
  m_ptg_rapidity->Divide(m_ptg_rapidity_2,m_ptg_rapidity_1,1.0,1.0); 

  //fit the slices of m_dpt_ptg, m_dp_pg,m_dp_pg_pion, m_dp_pg_kaon,m_dp_pg_proton
  //4 histrograms created, get RMS, for momentum resolution, plot "_2"
  m_dpt_ptg->FitSlicesY();
  m_dp_pg->FitSlicesY();
  m_dp_pg_pion->FitSlicesY();
  m_dp_pg_kaon->FitSlicesY();
  m_dp_pg_proton->FitSlicesY();
  
  //WriteOutHistogram();
}

//_____________________________________________________________________________
void St_tpt_Maker::VertexEffResolutionInit() {
  //create histograms and ntuple for VertexEffResolution
  
  gMessMgr->QAInfo() <<"begin to run St_tpt_Maker::VertexEffResolutionInit"<<endm;

  m_vertex_x=new TH1F("TptTteVertexX", "primary vertex X",50,-0.05, 0.05);
  //m_vertex_x->SetXTitle("primary vertex X");
  
  m_vertex_y=new TH1F("TptTteVertexY", "primary vertex Y",50,-0.05, 0.05);
  m_vertex_y->SetXTitle("primary vertex Y");
  
  m_vertex_z=new TH1F("TptTteVertexZ", "primary vertex Z",100,-50., 50.);
  m_vertex_z->SetXTitle("primary vertex Z");
  
  m_vertex_xy=new TH1F("TptTteVertexXY", "primary vertex Rxy",30,0., 0.03);
  m_vertex_xy->SetXTitle("primary vertex XY");
  
  m_vertexX_vertexY=new TH2F("TptTteVertexXVertexY", "primary vertex X vs. Y",50,-0.05, 0.05,50,-0.05, 0.05);
  m_vertexX_vertexY->SetXTitle("primary vertex X"); m_vertexX_vertexY->SetYTitle("primary vertex Y");
  
  m_vertexX_vertexZ=new TH2F("TptTteVertexXVertexZ", "primary vertex X vs. Z",50,-0.05, 0.05,100,-50., 50.);
  m_vertexX_vertexZ->SetXTitle("primary vertex X"); m_vertexX_vertexZ->SetYTitle("primary vertex Z");
  
  m_average_ptr=new TH1F("TptTteAveragePtr","average rec. pt",100,0.,1.0);
  m_average_ptr->SetXTitle("average rec. pt");
  
  m_average_ptg=new TH1F("TptTteAveragePtg","average mc. pt",100,0.,1.0);
  m_average_ptg->SetXTitle("average mc. pt");
  
  m_average_rapidity=new TH1F("TptTteAverageRapidity","average rec. pseudo_rapidity",100,-1.,1.0);
  m_average_rapidity->SetXTitle("average rec. pseudo_rapidity");

  m_average_chisqxy=new TH1F("TptTteAverageChisqxy","average rec. chisq xy",100,0.,100.0);
  m_average_chisqxy->SetXTitle("average rec. chisq xy");
  
 m_average_chisqz=new TH1F("TptTteAverageChisqz","average rec. chisq z",150,0.,150.0);
 m_average_chisqz->SetXTitle("average rec. chisqz");
 
 m_vertexXY_eff=new TH2F("TptTteVertexXYEff","vertex XY vs. efficiency", 30,0.,0.03,100, 0.,1.5);
 m_vertexXY_eff->SetXTitle("vertex XY"); m_vertexXY_eff->SetYTitle("average rec. efficiency"); 
 m_vertexXY_eff->SetMarkerStyle(21); m_vertexXY_eff->SetMarkerSize(0.7);
 
 m_vertexZ_eff=new TH2F("TptTteVertexZEff","vertex Z vs. efficiency", 100,-50.,50.,100,  0.,1.5);
 m_vertexZ_eff->SetXTitle("vertex Z"); m_vertexZ_eff->SetYTitle("average rec. efficiency"); 
 m_vertexZ_eff->SetMarkerStyle(21); m_vertexZ_eff->SetMarkerSize(0.7);
 
 m_chisqxy=new TH1F("TptTteChisqxy","chisq on xy plan",100,0.,5.);
 m_chisqxy->SetXTitle("rec. chisq xy");
 
 m_chisqz=new TH1F("TptTteChisqz","chisq on z plan",100,0.,5.);
 m_chisqz->SetXTitle("rec. chisq z");
 
 m_vertexXY_average_ptr=new TH2F("TptTteVertexXYAveragePtr","vertex XY vs. average Ptr", 30,0.,0.03,50,0.,1.0);
 m_vertexXY_average_ptg=new TH2F("TptTteVertexXYAveragePtg","vertex XY vs. average Ptg", 30,0.,0.03,50,0.,1.0);
 m_vertexXY_average_rapidity=new TH2F("TptTteVertexXYAverageRapidity","vertex XY vs. average rapidity", 30,0.,0.03,20,-1.,1.);
 m_vertexXY_average_chisqxy=new TH2F("TptTteVertexXYAverageChisqxy","vertex XY vs. average chisqxy", 30,0.,0.03,100,0.,5.);
 m_vertexXY_average_chisqz=new TH2F("TptTteVertexXYAverageChisqz","vertex XY vs. average chisqz", 30,0.,0.03,30,0.,10.);
 m_vertexXY_average_ptr->SetMarkerStyle(21); m_vertexXY_average_ptr->SetMarkerSize(0.7);
 m_vertexXY_average_ptr->SetXTitle("vertex XY"); m_vertexXY_average_ptr->SetYTitle("average rec. pt"); 
 m_vertexXY_average_ptg->SetMarkerStyle(21); m_vertexXY_average_ptg->SetMarkerSize(0.7);
 m_vertexXY_average_ptg->SetXTitle("vertex XY"); m_vertexXY_average_ptg->SetYTitle("average mc. pt"); 
 m_vertexXY_average_rapidity->SetMarkerStyle(21); m_vertexXY_average_rapidity->SetMarkerSize(0.7);
 m_vertexXY_average_rapidity->SetXTitle("vertex XY"); m_vertexXY_average_rapidity->SetYTitle("average  rapidity "); 
 m_vertexXY_average_chisqxy->SetMarkerStyle(21); m_vertexXY_average_chisqxy->SetMarkerSize(0.7);
 m_vertexXY_average_chisqxy->SetXTitle("vertex XY"); m_vertexXY_average_chisqxy->SetYTitle("average  chisqxy"); 
 m_vertexXY_average_chisqz->SetMarkerStyle(21); m_vertexXY_average_chisqz->SetMarkerSize(0.7);
 m_vertexXY_average_chisqz->SetXTitle("vertex XY"); m_vertexXY_average_chisqz->SetYTitle("average  chisqz"); 
 
 m_vertexZ_average_ptr=new TH2F("TptTteVertexZAveragePtr","vertex Z vs. average Ptr", 100,-50.,50.,50,0.,1.);
 m_vertexZ_average_ptg=new TH2F("TptTteVertexZAveragePtg","vertex Z vs. average Ptg", 100,-50.,50.,50,0.,1.);
 m_vertexZ_average_rapidity=new TH2F("TptTteVertexZAverageRapidity","vertex Z vs. average rapidity", 100,-50.,50.,20,-1.,1.);
 m_vertexZ_average_chisqxy=new TH2F("TptTteVertexZAverageChisqxy","vertex Z vs. average chisqxy", 100,-50.,50.,100,0.,5.);
 m_vertexZ_average_chisqz=new TH2F("TptTteVertexZAverageChisqz","vertex Z vs. average chisqz", 100,-50.,50.,50,0.,10.);
 m_vertexZ_average_ptr->SetMarkerStyle(21); m_vertexZ_average_ptr->SetMarkerSize(0.7);
 m_vertexZ_average_ptr->SetXTitle("vertex Z"); m_vertexZ_average_ptr->SetYTitle("average rec. pt");
 m_vertexZ_average_ptg->SetMarkerStyle(21); m_vertexZ_average_ptg->SetMarkerSize(0.7);
 m_vertexZ_average_ptg->SetXTitle("vertex Z"); m_vertexZ_average_ptg->SetYTitle("average mc. pt");
 m_vertexZ_average_rapidity->SetMarkerStyle(21); m_vertexZ_average_rapidity->SetMarkerSize(0.7);
 m_vertexZ_average_rapidity->SetXTitle("vertex Z"); m_vertexZ_average_rapidity->SetYTitle("average  rapidity "); 
 m_vertexZ_average_chisqxy->SetMarkerStyle(21); m_vertexZ_average_chisqxy->SetMarkerSize(0.7);
 m_vertexZ_average_chisqxy->SetXTitle("vertex Z"); m_vertexZ_average_chisqxy->SetYTitle("average  chisqxy"); 
 m_vertexZ_average_chisqz->SetMarkerStyle(21); m_vertexZ_average_chisqz->SetMarkerSize(0.7);
 m_vertexZ_average_chisqz->SetXTitle("vertex Z"); m_vertexZ_average_chisqz->SetYTitle("average  chisqz"); 
 
 m_rapidity_total1=new TH1F("TptTteRapidityTotal1", "rapidity with vid=1,nfst>5", 30,-3.,3.);
 m_rapidity_total2=new TH1F("TptTteRapidityTotal2", "rapidity with vid=1,nfst>5,nrec>0", 30,-3.,3.);
 m_eff_total=new TH1F("TptTteEffTotal", "eff=rapidity_total2/rapidity_total1", 30,-3.,3.);
 
 m_averge_eff=new TH1F("TptTteAvergeEff","effificency per event",50,0.2,1.2);
 
 m_ptg_rapidity=new TH2F("TptTtePtgRapidity","Ptg vs. pseudo_rapidity ", 30,0.,3.,30,-3.,3.);
 m_ptg_rapidity->SetMarkerStyle(21); m_ptg_rapidity->SetMarkerSize(0.7);
 m_ptg_rapidity->SetXTitle("ptg"); m_ptg_rapidity->SetYTitle("pseudo_rapidity");
 
 m_ptg_rapidity_1=new TH2F("TptTtePtgRapidity1","Ptg vs. pseudo_rapidity (nrec1>=0)", 30,0.,3.,30,-3.,3.);
 m_ptg_rapidity_2=new TH2F("TptTtePtgRapidity2","Ptg vs. pseudo_rapidity (nrec1>0)", 30,0.,3.,30,-3.,3.);
 
 m_ptg_rapidity_dpt=new TH3F("TptTtePtgRapidityDpt","Ptg vs. pseudo_rapidity vs. abs(ptr-ptg)/ptg", 30,0.,3.,30,-3.,3.,20,-0.0001,0.0099);
 m_ptg_rapidity_dpt->SetMarkerStyle(21); m_ptg_rapidity_dpt->SetMarkerSize(0.7);
 m_ptg_rapidity_dpt->SetXTitle("ptg"); m_ptg_rapidity_dpt->SetYTitle("pseudo_rapidity");m_ptg_rapidity_dpt->SetZTitle("momentum resolution");
 
 // for momentum resolution
 m_dpt=new TH1F("TptTtePtResolution", "abs(ptr-ptg)/ptg", 20, 0.,0.01);
 m_dpt_ptg=new TH2F("TptTtePtResolutionPg", "abs(ptr-ptg)/ptg vs. ptg", 50,0.,5., 20,-0.0001,0.0999);
 
 m_dp=new TH1F("TptTteMomentumResolution", "abs(pr-pg)/pg", 20, 0.,0.01);
 m_dp_pg=new TH2F("TptTteMomentumResolutionPg", "abs(pr-pg)/pg vs. pg", 20,0.,2., 20,-0.0001,0.0099);
 
 m_dp_pg_pion=new TH2F("TptTteMomentumResolutionPgPion", "abs(pr-pg)/pg vs. pg for pion", 20,0.,2.,20, -0.0001,0.0099);
 m_dp_pg_proton=new TH2F("TptTteMomentumResolutionPgProton", "abs(pr-pg)/pg vs. pg for pion", 20,0.,2., 20,-0.0001,0.0099);
 m_dp_pg_kaon=new TH2F("TptTteMomentumResolutionPgKaon", "abs(pr-pg)/pg vs. pg for pion", 20,0.,2., 20,-0.0001,0.0099);
 
 //init ntuple
 m_vertex_final = new TNtuple("TptTteVertexFinal","vertex information", "vx:vy:vz:aptr:aptg:eff:ay:achisqxy:achisqz");
 
 mevent=0;
 
}

