//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StPrimaryMaker class ( finds primary tracks )                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "StPrimaryMaker.h"

#include "math_constants.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StVertexId.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "global/St_track_propagator_Module.h"
#include "global/St_egr_impactcl_Module.h"
#include "global/St_egr_fitter_Module.h"
#include "global/St_egr_primfit_Module.h"

#ifndef  gufld 
#define gufld   gufld_
extern "C" {void gufld(Float_t *, Float_t *);}
#endif 


ClassImp(StPrimaryMaker)
  
//_____________________________________________________________________________
  StPrimaryMaker::StPrimaryMaker(const char *name):StMaker(name),
  m_egr_egrpar(0),
  m_egr2_egrpar(0)
{
  m_flag = 2;
}
//_____________________________________________________________________________
StPrimaryMaker::~StPrimaryMaker(){
}
//_____________________________________________________________________________
Int_t StPrimaryMaker::Init(){
  // Create tables

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
  
  St_dst_vertex  *vertex  = (St_dst_vertex*) FindByName("vertex"); 
  if (!vertex) {
    gMessMgr->Error("StPrimaryMaker: primary vertex table not found - exiting");
    return kStWarn;
  }

  St_DataSet *match = GetDataSet("match"); 
  St_DataSetIter matchI(match);         
  
  St_dst_track   *globtrk = (St_dst_track *) matchI("globtrk");
  St_dst_track   *EstGlobal = 0;
  EstGlobal = (St_dst_track *) matchI("EstGlobal");
  if (! globtrk) {globtrk = new St_dst_track("globtrk",1); AddGarb(globtrk);}

  St_dst_track   *primtrk = 0;   
  St_dst_track   *EstPrimary =0;
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
  }
  if (! tptrack)    {tptrack = new St_tpt_track("tptrack",1); AddGarb(tptrack);}
  St_DataSet    *tpchits = GetInputDS("tpc_hits");
  St_tcl_tphit     *tphit     = 0;
  if (tpchits) {
    tphit     = (St_tcl_tphit     *) tpchits->Find("tphit");
  }
  if (! tphit)    {tphit = new St_tcl_tphit("tphit",1); AddGarb(tphit);} 
  
  St_sgr_groups     *tpc_groups = (St_sgr_groups *) matchI("tpc_groups");
  St_sgr_groups     *tpc_groupsEst = (St_sgr_groups *) matchI("tpc_groupsEst");
  if (! tpc_groups)    {tpc_groups = new St_sgr_groups("tpc_groups",1); AddGarb(tpc_groups);} 
if (! tpc_groupsEst)    {tpc_groupsEst = new St_sgr_groups("tpc_groupsEst",1); AddGarb(tpc_groupsEst);} 
  
  St_DataSet     *svtracks = GetInputDS("est");
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  
  St_stk_track   *stk_track   = 0;
  St_sgr_groups  *svt_groups  = 0;
  St_svm_evt_match *evt_match = 0;
  St_scs_spt     *scs_spt     = 0;

  // Case svt tracking performed
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("EstSvtTrk");
    svt_groups= (St_sgr_groups *) svtracks->Find("EstGroups");
    evt_match = (St_svm_evt_match *) svtracks->Find("EstMatch");
  }
  if (svthits) {
    scs_spt     = (St_scs_spt    *)  svthits->Find("scs_spt");
  }
  
  // Case silicon not there
  if (!stk_track) {stk_track = new St_stk_track("EstSvtTrk",1); AddGarb(stk_track);}
  if (!svt_groups)    {svt_groups = new St_sgr_groups("EstGroups",1); AddGarb(svt_groups);}
  if (!evt_match)    {evt_match = new St_svm_evt_match("EstMatch",1); AddGarb(evt_match);}
  if (!scs_spt)   {scs_spt = new St_scs_spt("scs_spt",1); AddGarb(scs_spt);}
  // 			Case running est tpc -> Si space point tracking
  if ( !(svtracks && svthits) ){
    svt_groups = new St_sgr_groups("EstGroups",1); AddGarb(svt_groups);
    stk_track    = (St_stk_track *) m_GarbSet->Find("EstSvtTrk");
    if( !stk_track){ stk_track = new St_stk_track("EstSVtTrk",1); AddGarb(stk_track);}
  } 

  long NGlbTrk = 0;
  if (globtrk) NGlbTrk = globtrk->GetNRows();

  // track_propagator
  St_dst_track *globtrk2  = new St_dst_track("globtrk2");
  AddData(globtrk2);
  
  *globtrk2  = *globtrk;		//Copy table
  
  if(Debug()) gMessMgr->Debug(" Calling track_propagator ");
  
  if (m_tp_param && vertex) {
    egr_propagate_st *tp_param = m_tp_param->GetTable();
    tp_param->iflag =   m_flag;
    if (m_flag == 1 || m_flag == 2) {
      dst_vertex_st *vrtx = vertex->GetTable();
      memcpy(tp_param->x,&vrtx->x,3*sizeof(Float_t));  
    }
  }

  iRes = track_propagator(globtrk,m_tp_param,globtrk2);
  if (iRes != kSTAFCV_OK) {
    iMake = kStWarn;
    gMessMgr->Warning("StPrimaryMaker: Problem on return from Track_Propagator");
  }
  
  dst_track_st *glob  = globtrk->GetTable();
  dst_vertex_st *vrtx = vertex->GetTable();
  if( vrtx->vtx_id != kEventVtxId || vrtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++){
      if( vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1 ) break;
    }
  }

  Float_t xval[3] = {0.,0.,0.};
  Float_t bval[3];
  gufld(xval,bval);

  if (vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1) {
    Float_t *pv = &vrtx->x;
    StThreeVectorD primVertex(pv[0],pv[1],pv[2]);
    
    for( Int_t no_rows=0; no_rows<globtrk->GetNRows(); no_rows++, glob++)
      {
	Float_t dip   = atan(glob->tanl);
	Int_t    h    = ((bval[2] * glob->icharge) > 0 ? -1 : 1);
	Float_t phase = glob->psi*degree-h*pi/2;
	Float_t curvature = glob->curvature;
	Float_t x0 = glob->r0 * cos(glob->phi0 * degree);
	Float_t y0 = glob->r0 * sin(glob->phi0 * degree);
	Float_t z0 = glob->z0;
	StThreeVectorD origin(x0, y0, z0);  
	StHelixD globHelix(curvature, dip, phase, origin, h);
	
	glob->impact = globHelix.distance(primVertex);
      }
  
    
    if(Debug()) gMessMgr->Debug(" finished calling track-propagator");
    
    // egr2
    if (tphit && stk_track) {
      primtrk = new St_dst_track("primtrk", NGlbTrk);
      AddData(primtrk);
      

      //calculate impact parameter Confidence Level
      if(Debug())
	gMessMgr->Debug("Calling EGR_impactcl");

      iRes = egr_impactcl (vertex,m_egr2_egrpar,globtrk);
      //     ============================================

      if(Debug())
        gMessMgr->Debug("Calling egr_primfit");
      iRes = egr_primfit(vertex, m_egr2_egrpar, globtrk, primtrk);
      //     ====================================================
      
      if (iRes !=kSTAFCV_OK) iMake = kStWarn;
      if (iRes !=kSTAFCV_OK){
        gMessMgr->Warning("Problem on return from egr_primfit");}
       
      if(Debug())
        gMessMgr->Debug(" finished calling egr_primfit");


     // Fill bit map in prim trk
      

      tcl_tphit_st  *spc   = tphit->GetTable();
      sgr_groups_st *tgroup = tpc_groups->GetTable();
      dst_track_st * track  = primtrk->GetTable();
      
      int spt_id = 0;
      int row = 0,i;
      bool isset;

      //First set all bits in map to zero before doing bitwise ops
      for(i=0;i<primtrk->GetNRows();i++){
      	track[i].map[0] = 0UL;
      	track[i].map[1] = 0UL;
      }

      for( i=0; i<tpc_groups->GetNRows(); i++, tgroup++){
	if( tgroup->id1 != 0 && tgroup->ident >= 0){
	  spt_id = tgroup->id2-1;
	  if( spt_id <0) {
	    cout << spt_id << endl;
            return kStErr;
	  }
          else{
	    row = spc[spt_id].row/100;
	    row = spc[spt_id].row - row*100;
	    if( spc[spt_id].id_globtrk-1 < 0){
	      cout << tgroup->ident << " " << tgroup->id1 << " " << tgroup->id2 << " " << spc[spt_id].id << " " << endl;
              return kStErr;
	    }
	    if( row < 25){
	      isset = track[spc[spt_id].id_globtrk-1].map[0] & 1UL<<(row+7);
	      track[spc[spt_id].id_globtrk-1].map[0] |= 1UL<<(row+7);
	    }
	    else{
	      isset = track[spc[spt_id].id_globtrk-1].map[1] & 1UL<<(row-25);
	      track[spc[spt_id].id_globtrk-1].map[1] |= 1UL<<(row-25);
	    }
	    if (isset) track[spc[spt_id].id_globtrk-1].map[1] |= 1UL<<30; 
	  }
	}
      }
      
      
      // If EstGlobal exists create EstPrimary
      if( EstGlobal){
	
	dst_track_st *glob  = EstGlobal->GetTable();
	for( Int_t no_rows=0; no_rows<EstGlobal->GetNRows(); no_rows++, glob++)
	  {
	    Float_t dip   = atan(glob->tanl);
	    Int_t    h    = ((bval[2] * glob->icharge) > 0 ? -1 : 1);
	    Float_t phase = glob->psi*degree-h*pi/2;
	    Float_t curvature = glob->curvature;
	    Float_t x0 = glob->r0 * cos(glob->phi0 * degree);
	    Float_t y0 = glob->r0 * sin(glob->phi0 * degree);
	    Float_t z0 = glob->z0;
	    StThreeVectorD origin(x0, y0, z0);  
	    StHelixD globHelix(curvature, dip, phase, origin, h);
	    
	    glob->impact = globHelix.distance(primVertex);
	  }
	
	EstPrimary = new St_dst_track("EstPrimary", NGlbTrk);
	AddData(EstPrimary);
	
	
	//calculate impact parameter Confidence Level
	if(Debug())
	  gMessMgr->Debug("Calling EGR_impactcl");
	
	iRes = egr_impactcl (vertex,m_egr2_egrpar,EstGlobal);
	//     ============================================
	
	if(Debug())
	  gMessMgr->Debug("Calling egr_primfit");
	iRes = egr_primfit(vertex, m_egr2_egrpar, EstGlobal, EstPrimary);
	//     ====================================================
	
	if (iRes !=kSTAFCV_OK) iMake = kStWarn;
	if (iRes !=kSTAFCV_OK){
	  gMessMgr->Warning("Problem on return from egr_primfit");}
	
	if(Debug())
	  gMessMgr->Debug(" finished calling egr_primfit");
	
	
	// Fill bit map in EstPrimary
	
	
	spc   = tphit->GetTable();
	tgroup = tpc_groupsEst->GetTable();
	track  = EstPrimary->GetTable();
	
	int spt_id = 0;
	int row = 0,i;
	bool isset;
	
	//First set all bits in map to zero before doing bitwise ops
	for(i=0;i<EstPrimary->GetNRows();i++){
	  track[i].map[0] = 0UL;
	  track[i].map[1] = 0UL;
	}
	
	for( i=0; i<tpc_groupsEst->GetNRows(); i++, tgroup++){
	  if( tgroup->id1 != 0 && tgroup->ident >= 0){
	  spt_id = tgroup->id2-1;
	  if( spt_id <0) {
	    cout << spt_id << endl;
            return kStErr;
	  }
          else{
	    row = spc[spt_id].row/100;
	    row = spc[spt_id].row - row*100;
	    if( spc[spt_id].id_globtrk-1 < 0){
	      cout << tgroup->ident << " " << tgroup->id1 << " " << tgroup->id2 << " " << spc[spt_id].id << " " << endl;
              return kStErr;
	    }
	    if( row < 25){
	      isset = track[spc[spt_id].id_globtrk-1].map[0] & 1UL<<(row+7);
	      track[spc[spt_id].id_globtrk-1].map[0] |= 1UL<<(row+7);
	    }
	    else{
	      isset = track[spc[spt_id].id_globtrk-1].map[1] & 1UL<<(row-25);
	      track[spc[spt_id].id_globtrk-1].map[1] |= 1UL<<(row-25);
	    }
	    if (isset) track[spc[spt_id].id_globtrk-1].map[1] |= 1UL<<30; 
	  }
	  }
	}
	
	
	scs_spt_st *s_spc = scs_spt->GetTable();
	sgr_groups_st *sgroup = svt_groups->GetTable();
	
	for( i=0; i<svt_groups->GetNRows(); i++, sgroup++){
	  
	  if( sgroup->id1 != 0 && sgroup->ident >= 0){
	    spt_id = sgroup->id2-1;
	    row = s_spc[spt_id].id_wafer/1000;
	    if(  s_spc[spt_id].id_globtrk-1 < 0){
	      cout << spt_id << " " << s_spc[spt_id].id_globtrk<< " " << endl;
	      return kStErr;
	    }
	    if( row>7)row=7;
	    track[s_spc[spt_id].id_globtrk-1].map[0] |= (1UL<<row);
	    
	  }
	  
	}
	
      } // End of if EstGlobal
    }
  } else {
    gMessMgr->Debug(" No Primary vertex ");
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
      
      Float_t *pv = &myvrtx->x;
      StThreeVectorD primVertex(pv[0],pv[1],pv[2]);
      
      dst_track_st* globtrkPtr = globtrk->GetTable();
      dst_track_st* primtrkPtr = primtrk->GetTable();  
      for( Int_t i=0; i<primtrk->GetNRows(); i++, globtrkPtr++, primtrkPtr++) 
        {
	  
	  globtrkPtr->id_start_vertex = 0;
	  primtrkPtr->id_start_vertex = globtrkPtr->id_start_vertex;
	  if( globtrkPtr->impact < 3.){
	    primtrkPtr->id_start_vertex = myvrtx->id*10+1;
	  }     
          //          if(primtrkPtr->id_start_vertex != 0)
          //  keep_vrtx_id=primtrkPtr->id_start_vertex;
          primtrkPtr->n_max_point = globtrkPtr->n_max_point;
	  primtrkPtr->map[0] |= (1UL<<0);

	  Float_t dip   = atan(primtrkPtr->tanl);
	  Int_t    h    = ((bval[2] * primtrkPtr->icharge) > 0 ? -1 : 1);
	  Float_t phase = primtrkPtr->psi*degree-h*pi/2;
	  Float_t curvature = primtrkPtr->curvature;
	  Float_t x0 = primtrkPtr->r0 * cos(primtrkPtr->phi0 * degree);
	  Float_t y0 = primtrkPtr->r0 * sin(primtrkPtr->phi0 * degree);
	  Float_t z0 = primtrkPtr->z0;
	  StThreeVectorD origin(x0, y0, z0);  
	  StHelixD primHelix(curvature, dip, phase, origin, h);
	  
          primtrkPtr->impact = primHelix.distance(primVertex);

	  StThreeVectorD lastPoint(primtrkPtr->x_last[0], 
				   primtrkPtr->x_last[1],primtrkPtr->x_last[2]);
	  Float_t primLength = primHelix.pathLength(lastPoint);
	  primtrkPtr->length = (primLength>0) ? primLength : (-primLength);
        }


      if( EstPrimary){
	globtrkPtr = EstGlobal->GetTable();
	primtrkPtr = EstPrimary->GetTable();  
	for( Int_t i=0; i<EstPrimary->GetNRows(); i++, globtrkPtr++, primtrkPtr++) 
	  {
	    globtrkPtr->id_start_vertex = 0;
	    primtrkPtr->id_start_vertex = globtrkPtr->id_start_vertex;
	    if( globtrkPtr->impact < 3.){
	      primtrkPtr->id_start_vertex = myvrtx->id*10+1;
	      }
	    //          if(primtrkPtr->id_start_vertex != 0)
	    //  keep_vrtx_id=primtrkPtr->id_start_vertex;
	    primtrkPtr->n_max_point = globtrkPtr->n_max_point;
	    primtrkPtr->map[0] |= (1UL<<0);
	    
	    Float_t dip   = atan(primtrkPtr->tanl);
	    Int_t    h    = ((bval[2] * primtrkPtr->icharge) > 0 ? -1 : 1);
	    Float_t phase = primtrkPtr->psi*degree-h*pi/2;
	    Float_t curvature = primtrkPtr->curvature;
	    Float_t x0 = primtrkPtr->r0 * cos(primtrkPtr->phi0 * degree);
	    Float_t y0 = primtrkPtr->r0 * sin(primtrkPtr->phi0 * degree);
	    Float_t z0 = primtrkPtr->z0;
	    StThreeVectorD origin(x0, y0, z0);  
	    StHelixD primHelix(curvature, dip, phase, origin, h);
	    
	    primtrkPtr->impact = primHelix.distance(primVertex);
	    
	    StThreeVectorD lastPoint(primtrkPtr->x_last[0], 
				     primtrkPtr->x_last[1],primtrkPtr->x_last[2]);
	    Float_t primLength = primHelix.pathLength(lastPoint);
	    primtrkPtr->length = (primLength>0) ? primLength : (-primLength);
	  }
      } // Fill EstPrimary info if it exists

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
  printf("%s end, nPrimTR=%d\n",GetName(),nrows);
  
  
   if( EstPrimary){
    
    nrows = EstPrimary->GetNRows();
     dst_track_st *squeezePtr = EstPrimary->GetTable();
     n2rows = 0;
     for (Int_t irow=0;irow<nrows;irow++){
       if(squeezePtr[irow].id_start_vertex == 0) continue;
       squeezePtr[n2rows++]= squeezePtr[irow];    
     }
     EstPrimary->SetNRows(n2rows); 
     cout << nrows << " Initial primary rows and " << n2rows << " good rows" << endl;  
   }
  
 return iMake;
  }
//_____________________________________________________________________________
// $Id: StPrimaryMaker.cxx,v 1.78 2003/08/07 00:19:52 caines Exp $
// $Log: StPrimaryMaker.cxx,v $
// Revision 1.78  2003/08/07 00:19:52  caines
// Write out the TPC only vertex as a calibration vertex if est vertex found
//
// Revision 1.77  2002/05/16 01:59:19  caines
// Send in differnt group tables for the TPC and est refit so flagging of hits correct
//
// Revision 1.76  2002/04/24 21:15:20  caines
// Fix EstPrimary.id_start_vertex flagging so the tracks get saved to the primary vertex in StEvent and it doesnt crash
//
// Revision 1.75  2002/04/17 23:56:43  jeromel
// Changes by Helen for the SVT in egr implementation.
//
// Revision 1.74  2002/02/22 01:42:54  caines
// Correct track-hit correlations for SVT
//
// Revision 1.73  2002/02/18 19:48:20  genevb
// Separation of primary vertex and track finding, other minor changes
//
//
// For older cvs log information, examine (via CVS browser or checkout)
// version 1.72
//
