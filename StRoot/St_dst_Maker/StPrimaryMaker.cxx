//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StPrimaryMaker class ( est + evr + egr )                             //
//                                                                      //
// $Id: StPrimaryMaker.cxx,v 1.33 2000/02/16 15:21:11 genevb Exp $
// $Log: StPrimaryMaker.cxx,v $
// Revision 1.33  2000/02/16 15:21:11  genevb
// Cannot call evr with <1 tracks
//
// Revision 1.32  2000/02/08 21:14:18  genevb
// Handle cases with no tracks.
//
// Revision 1.31  2000/02/02 14:23:47  wdeng
// Add protection in case one runs StPrimaryMaker without running StPreVertexMaker beforehand
//
// Revision 1.30  2000/02/01 17:16:21  wdeng
// Copy the preliminary primary vertex to the first four rows of vertex table.
//
// Revision 1.29  2000/01/31 15:07:30  caines
// Set primary mxtry for fitting to 1 so no testing on chi2 is done
//
// Revision 1.28  2000/01/26 23:41:47  caines
// Prob cuts set to 10 as in global fit
//
// Revision 1.27  1999/12/17 22:27:22  margetis
// selection of primtrk is based on id_start_vertex (dca) not iflag
//
// Revision 1.26  1999/12/17 03:46:08  didenko
// Spiros version
//
// Revision 1.25  1999/12/16 19:00:24  margetis
// copy n_max_point to primtrk variable
//
// Revision 1.24  1999/12/15 01:28:56  nystrand
// changed return code from lmv to be consistent with evr
//
// Revision 1.23  1999/12/10 17:38:41  genevb
// Added fixed vtx functionality, allow lmv and fixed vtx only one vtx entry
//
// Revision 1.22  1999/12/02 03:22:25  perev
// remove redundant m_egr_egrpar
//
// Revision 1.21  1999/11/27 18:21:42  fisyak
// Add test that primary vertex exists
//
// Revision 1.20  1999/11/18 22:18:38  fisyak
//  Spiros decided to open this cut from 1cm (~1sigma for year1) to 3cm (~3sigma).
//
// Revision 1.19  1999/11/16 20:58:42  wdeng
// Spiros's temporary solution to id_start_vertex puzzle
//
// Revision 1.18  1999/11/12 01:45:02  nystrand
// Added call to St_db_Maker->GetDateTime to determine geometry
//
// Revision 1.17  1999/11/10 01:56:35  lbarnby
// fix to allow 'intermediate' primary vertices to be saved plus final one - was previously only saving initial guess
//
// Revision 1.16  1999/10/29 23:23:26  caines
// Removed scenario methods
//
// Revision 1.15  1999/10/27 19:31:16  nystrand
// Added call to lmv
//
// Revision 1.14  1999/10/19 00:11:30  fisyak
// Remove aux tables
//
// Revision 1.13  1999/09/30 13:34:21  wdeng
// Diminish the degree or radian bug
//
// Revision 1.12  1999/09/29 20:29:06  wdeng
// Accommodate dst_track and dst_vertex change
//
// Revision 1.11  1999/09/13 23:28:33  caines
// Changed egrpars so doesn't use SVT only tracks by default
//
// Revision 1.10  1999/09/13 15:17:58  caines
// Changed memset(&row,0,m_evr_evrpar->GetRowSize());  to memset(&row,0,m_egr_egrpar->GetRowSize()); for the egr2 allocation
//
// Revision 1.9  1999/09/13 15:07:05  caines
// Added creation of garb(tphit) and garb(tptrack) so it is possible
// to run with TPC turned off
//
// Revision 1.8  1999/09/12 23:03:03  fisyak
// Move parameters into makers
//
// Revision 1.7  1999/07/17 00:31:24  genevb
// Use StMessMgr
//
// Revision 1.6  1999/07/15 13:57:53  perev
// cleanup
//
// Revision 1.5  1999/07/12 01:49:39  fine
// Clean up
//
// Revision 1.4  1999/07/11 01:55:45  fisyak
// Fix glob->impact
//
// Revision 1.3  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "TMath.h"
#include "StPrimaryMaker.h"

#include "math_constants.h"

#include "StVertexId.h"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "global/St_evr_am_Module.h"
#include "global/St_egr_fitter_Module.h"
#include "global/St_track_propagator_Module.h"
#include "St_db_Maker/St_db_Maker.h"

long lmv(St_dst_track *track, St_dst_vertex *vertex, Int_t mdate);

//class St_tcl_tpcluster;
//class St_scs_cluster;
//class St_ctu_cor;

ClassImp(StPrimaryMaker)
  
  //_____________________________________________________________________________
  StPrimaryMaker::StPrimaryMaker(const char *name):StMaker(name),
  m_evr_evrpar(0),
  m_egr_egrpar(0),
  m_egr2_egrpar(0),
  m_fixedVertex(0)
{
  m_flag      = 2;
}
//_____________________________________________________________________________
StPrimaryMaker::~StPrimaryMaker(){
  UnFixVertex();
}
//_____________________________________________________________________________
void StPrimaryMaker::FixVertex(Float_t x, Float_t y, Float_t z){
  if (m_fixedVertex) {
    m_fixedVertex->x = x;
    m_fixedVertex->y = y;
    m_fixedVertex->z = z;
  } else {
    m_fixedVertex = new dst_vertex_st;
    m_fixedVertex->vtx_id = kEventVtxId;
    m_fixedVertex->n_daughters = 0;
    m_fixedVertex->id = 1;
    m_fixedVertex->iflag = 1;
    m_fixedVertex->det_id = 3;
    m_fixedVertex->id_aux_ent = 0;
    m_fixedVertex->x = x;
    m_fixedVertex->y = y;
    m_fixedVertex->z = z;
    m_fixedVertex->covar[0] = 0.;
    m_fixedVertex->covar[1] = 0.;
    m_fixedVertex->covar[2] = 0.;
    m_fixedVertex->covar[3] = 0.;
    m_fixedVertex->covar[4] = 0.;
    m_fixedVertex->covar[5] = 0.;
    m_fixedVertex->chisq[0] = -1.;
    m_fixedVertex->chisq[1] = -1.;
  }
}
//_____________________________________________________________________________
void StPrimaryMaker::UnFixVertex(){
  if (m_fixedVertex) {
    delete m_fixedVertex;
    m_fixedVertex = 0;
  }
}
//_____________________________________________________________________________
Int_t StPrimaryMaker::Init(){
  // Create tables
  //evr
  //  m_evr_evrpar  = (St_evr_evrpar *) params("evrpars/evr_evrpar");
  m_evr_evrpar = new St_evr_evrpar("evr_evrpar",1);
  {
    evr_evrpar_st row;
    //
    memset(&row,0,sizeof(row));
    row.vcut	 =          3; // distance below where track is marked as default primary ;
    row.cut2	 =          2; // select tracks for 2nd vertex fit ;
    row.cut3	 =        0.5; // select tracks for 3rd vertex fit ;
    row.cutxy	 =          1; // select tracks for vertex fitting ;
    row.cutz	 =         10; // select tracks for vertex fitting ;
    row.ptmin	 =          0; // minimum pt of individual tracks ;
    m_evr_evrpar->AddAt(&row,0);
  }
  AddRunCont(m_evr_evrpar);
  
  // prop
  m_tp_param = new St_egr_propagate("tp_param",1); 
  AddRunCont(m_tp_param);
  m_tp_param->SetNRows(1);
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

  m_egr2_egrpar = new St_egr_egrpar("egr2_egrpar",1);
  {  
    egr_egrpar_st row;
    memset(&row,0,sizeof(row));
    row.mxtry =    1;
    row.minfit =    5;
    row.prob[0] =   10;
    row.prob[1] =   10;
    row.debug[0] =  1;
    row.svtchicut = 0;
    row.usetpc    = 2;
    row.usesvt    = 0;
    row.usevert   = 1;
    row.useglobal = 2;
    m_egr2_egrpar->AddAt(&row,0);
  }
  AddRunCont(m_egr2_egrpar);
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StPrimaryMaker::Make(){
  PrintInfo();  
  
  int iMake = kStOK;
  int iRes = 0;
  
  St_DataSet *match = GetDataSet("match"); 
  St_DataSetIter matchI(match);         
  
  St_dst_track     *globtrk  = (St_dst_track *) matchI("globtrk");
  St_svm_evt_match *evt_match = (St_svm_evt_match *) matchI("evt_match");
  St_dst_track     *primtrk     = 0;   

  St_dst_vertex *vertex = new St_dst_vertex("vertex", 4); 
  AddData(vertex);  
 
  St_DataSet     *preVertex = GetDataSet("preVertex"); 
  St_DataSetIter  preVertexI(preVertex);
  St_dst_vertex  *preVtx  = (St_dst_vertex *) preVertexI("preVertex");

  if( preVtx ) {
    Int_t numRowPreVtx = preVtx->GetNRows();
    vertex->ReAllocate( numRowPreVtx+4 );
    
    dst_vertex_st *preVtxPtr = preVtx->GetTable();
    dst_vertex_st *vertexPtr = vertex->GetTable();
    Int_t sizeToCopy = sizeof(dst_vertex_st) * numRowPreVtx;
    memcpy(vertexPtr, preVtxPtr, sizeToCopy);
    vertex->SetNRows( numRowPreVtx );
  }

  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  St_tte_eval   *evaltrk   = 0;
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
    evaltrk   = (St_tte_eval   *) tpc_tracks("evaltrk");
  }
  if (! evaltrk)    {evaltrk = new St_tte_eval("evaltrk",1); AddGarb(evaltrk);}
  if (! tptrack)    {tptrack = new St_tpt_track("tptrack",1); AddGarb(tptrack);}
  St_DataSet    *tpchits = GetInputDS("tpc_hits");
  St_tcl_tphit     *tphit     = 0;
  if (tpchits) {
    tphit     = (St_tcl_tphit     *) tpchits->Find("tphit");
  }
  if (! tphit)    {tphit = new St_tcl_tphit("tphit",1); AddGarb(tphit);} 
  
  St_DataSet     *svtracks = GetInputDS("svt_tracks");
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  
  St_stk_track   *stk_track   = 0;
  St_sgr_groups  *groups      = 0;
  St_scs_spt     *scs_spt     = 0;
  
  // Case svt tracking performed
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("stk_track");
    groups    = (St_sgr_groups *) svtracks->Find("groups");
  }
  if (svthits) {
    scs_spt     = (St_scs_spt    *)  svthits->Find("scs_spt");
  }
  
  // Case silicon not there
  if (!stk_track) {stk_track = new St_stk_track("stk_track",1); AddGarb(stk_track);}
  if (!groups)    {groups = new St_sgr_groups("groups",1); AddGarb(groups);}
  if (!scs_spt)   {scs_spt = new St_scs_spt("scs_spt",1); AddGarb(scs_spt);}
  // 			Case running est tpc -> Si space point tracking
  if ( !(svtracks && svthits) ){
    groups = new St_sgr_groups("groups",10000); AddGarb(groups);
    stk_track    = (St_stk_track *) m_GarbSet->Find("stk_tracks");
    if( !stk_track){ stk_track = new St_stk_track("stk_tracks",5000); AddGarb(stk_track);}
  } 

  long NGlbTrk = globtrk->GetNRows();

  if (m_fixedVertex) {  // Fixed primary vertex
    gMessMgr->Warning("StPrimaryMaker: --------- WARNING!!! ---------","E-");
    gMessMgr->Warning() << "StPrimaryMaker: Fixing the primary vertex at (" <<
      m_fixedVertex->x << "," <<
      m_fixedVertex->y << "," <<
      m_fixedVertex->z << ")" << endm;
    gMessMgr->Warning("StPrimaryMaker: --------- WARNING!!! ---------","E-");
    // evr with fixed vertex
    vertex->AddAt(m_fixedVertex,0);
    if (NGlbTrk < 1) {
      gMessMgr->Warning() << "Cannot call evr with <1 tracks" << endm; 
      iRes = kSTAFCV_ERR;
    } else {
      if(Debug()) gMessMgr->Debug() <<
          "run_evr: calling evr_am with fixed vertex" << endm;
      iRes = evr_am(m_evr_evrpar,globtrk,vertex);
    }
  } else {  // Primary vertex is not fixed, find it
    // Switch to Low Multiplicity Primary Vertex Finder for multiplicities < 15
    if( NGlbTrk < 15 ){
      // lmv

      St_db_Maker *db = ( St_db_Maker *)GetMaker("db");
      Int_t mdate = db->GetDateTime().GetDate();
      if(Debug()) gMessMgr->Debug() << "run_lmv: calling lmv" << endm;
      iRes = lmv(globtrk,vertex,mdate);
      //   ================================================
      // Do this to solve inconsistency between kSt* and kSTAFCV* return codes
      if( iRes == kStOK ){
        iRes = kSTAFCV_OK;
      }
      else{
        iRes = kSTAFCV_ERR;
      }
    }
    else{    
      // evr
      if (NGlbTrk < 1) {
        gMessMgr->Warning() << "Cannot call evr with <1 tracks" << endm; 
        iRes = kSTAFCV_ERR;
      } else {
        if(Debug()) gMessMgr->Debug() << "run_evr: calling evr_am" << endm;
        iRes = evr_am(m_evr_evrpar,globtrk,vertex);
      }
      //	 ================================================
    }
  
  }  // end section on finding a primary vertex if not fixed
  
  if (iRes !=kSTAFCV_OK) return kStWarn;
  
  // track_propagator
  St_dst_track *globtrk2     = new St_dst_track("globtrk2");
  AddData(globtrk2);
  
  *globtrk2  = *globtrk;		//Copy table
  
  if(Debug()) gMessMgr->Debug() << " Calling track_propagator " << endm;
  
  if (m_tp_param && vertex) {
    egr_propagate_st *tp_param = m_tp_param->GetTable();
    tp_param->iflag =   m_flag;
    if (m_flag == 1 || m_flag == 2) {
      dst_vertex_st *vrtx = vertex->GetTable();
      memcpy(tp_param->x,&vrtx->x,3*sizeof(Float_t));  
    }
  }

  iRes = track_propagator(globtrk,m_tp_param,globtrk2);
  //	 ==============================================
  
  if (iRes !=kSTAFCV_OK) iMake = kStWarn;
  if (iRes !=  kSTAFCV_OK) 
    gMessMgr->Warning() << "Problem on return from Track_Propagator" << endm;
  
  dst_track_st *glob  = globtrk->GetTable();
  dst_track_st *glob2 = globtrk2->GetTable();
  dst_vertex_st *vrtx = vertex->GetTable();
  if( vrtx->vtx_id != kEventVtxId || vrtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++){
      if( vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1 ) break;
    }
  }
  if (vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1) {
    
    Float_t *v0 = &vrtx->x;
    for( Int_t no_rows=0; no_rows<globtrk2->GetNRows() &&
                          no_rows<globtrk->GetNRows(); no_rows++, glob++,glob2++)
      {
	Float_t xStart = glob2->r0 * cos(glob2->phi0 * C_RAD_PER_DEG);
	Float_t yStart = glob2->r0 * sin(glob2->phi0 * C_RAD_PER_DEG);
	Float_t zStart = glob2->z0;
	double qwe = pow(xStart-v0[0],2)+pow(yStart-v0[1],2)+pow(zStart-v0[2],2);
	
	glob->impact = TMath::Sqrt(qwe);
      }
    
    if(Debug()) gMessMgr->Debug() << " finished calling track-propagator" << endm;
    
    // egr2
    if (tphit && stk_track) {
      int nglob = globtrk->GetNRows();
      primtrk = new St_dst_track("primtrk",nglob);
      AddData(primtrk);
      
      if(Debug())
        gMessMgr->Debug() << "Calling EGR_fitter - Second time" << endm;
      iRes = egr_fitter (tphit,    vertex,       tptrack,  evaltrk,
			 scs_spt,m_egr2_egrpar,stk_track,groups,
			 evt_match,primtrk);
      //	   ======================================================
      
      if (iRes !=kSTAFCV_OK) iMake = kStWarn;
      if (iRes !=kSTAFCV_OK){
        gMessMgr->Warning() << "Problem on return from EGR_FITTER" << endm;}
      
      if(Debug())
        gMessMgr->Debug() <<" finished calling egr_fitter - second time" << endm;
    }
  }
  else {
    gMessMgr->Debug() <<" No Primary vertex " << endm;
    return kStWarn;
  }

  // copy id_start_vertex from globtrk to primtrk for all rows
  // copy n_max_point from globtrk to primtrk for all rows
  // calculate impact parameter variable

  //  Int_t keep_vrtx_id;
  dst_vertex_st *myvrtx = vertex->GetTable();
  if( myvrtx->vtx_id != kEventVtxId || myvrtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,myvrtx++){
      if( myvrtx->vtx_id == kEventVtxId && myvrtx->iflag == 1 ) break;
    }
  }
  if (myvrtx->vtx_id == kEventVtxId && myvrtx->iflag == 1) 
    {
      
      Float_t *myv0 = &myvrtx->x;
      
      dst_track_st* globtrkPtr = globtrk->GetTable();
      dst_track_st* primtrkPtr = primtrk->GetTable();  
      for( Int_t i=0; i<primtrk->GetNRows(); i++, globtrkPtr++, primtrkPtr++) 
        {
          primtrkPtr->id_start_vertex = globtrkPtr->id_start_vertex;
          //          if(primtrkPtr->id_start_vertex != 0)
          //  keep_vrtx_id=primtrkPtr->id_start_vertex;
          primtrkPtr->n_max_point = globtrkPtr->n_max_point;
          Float_t xStart = primtrkPtr->r0 * cos(primtrkPtr->phi0 * C_RAD_PER_DEG);
          Float_t yStart = primtrkPtr->r0 * sin(primtrkPtr->phi0 * C_RAD_PER_DEG);
          Float_t zStart = primtrkPtr->z0;
          double qwe = pow(xStart-myv0[0],2)+pow(yStart-myv0[1],2)+pow(zStart-myv0[2],2);
          
          primtrkPtr->impact = TMath::Sqrt(qwe);
        }
    } 
  
  Int_t nrows = primtrk->GetNRows();
  dst_track_st *squeezePtr = primtrk->GetTable();
  Int_t n2rows = 0;
  for (Int_t irow=0;irow<nrows;irow++){
    if(squeezePtr[irow].id_start_vertex == 0) continue;
    //    if(squeezePtr[irow].iflag <= 0) continue;
    squeezePtr[n2rows++]= squeezePtr[irow];    
    //   squeezePtr[n2rows++].id_start_vertex = keep_vrtx_id;    
  }
  nrows = n2rows;
  primtrk->SetNRows(nrows);  
 
  return iMake;
}
//_____________________________________________________________________________


