 //////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMatchMaker class ( svm + egr )                                     //
//                                                                      //
// $Id: StMatchMaker.cxx,v 1.37 2001/05/01 18:00:41 lbarnby Exp $
// $Log: StMatchMaker.cxx,v $
// Revision 1.37  2001/05/01 18:00:41  lbarnby
// Zero globtrk map before filling
//
// Revision 1.36  2001/02/28 18:25:33  caines
// Update bit map for SVT
//
// Revision 1.35  2001/02/02 16:09:07  caines
//  Remove usage of svm and get svt tracks from est branch
//
// Revision 1.34  2001/01/29 21:01:06  caines
// Remove est calls from StMatchMaker no longer used
//
// Revision 1.33  2000/11/01 00:53:01  lbarnby
// Move field dependent set up from Init into InitRun
//
// Revision 1.32  2000/10/23 19:44:13  lbarnby
// Fix units for B
//
// Revision 1.31  2000/10/20 21:21:35  lbarnby
// Set up copying into globtrk table (NO refit) if field-off run
//
// Revision 1.30  2000/09/07 19:00:53  lbarnby
// Explicitly set Kalman mass hypothesis to pion (whad been commented out) rather than relying on default inside Kalman module
//
// Revision 1.29  2000/08/07 14:39:40  caines
// Add to dst a copy of tpc and svt tracks called CpyTrk
//
// Revision 1.28  2000/07/10 17:06:54  caines
// Take out some hardwired numbers for svm
//
// Revision 1.27  2000/06/29 04:12:35  lbarnby
// fix to track length calculation for very small tanl tracks
//
// Revision 1.26  2000/06/26 22:13:18  fisyak
// remove params
//
// Revision 1.25  2000/06/22 16:57:41  wdeng
// Move globtrk length calculation from StPrimaryMaker to StMatchMaker.
//
// Revision 1.24  2000/05/17 00:11:31  caines
// Make sure ALL hits acknowledged as on track
//
// Revision 1.23  2000/04/29 21:33:28  caines
// More protection for flagged tpc hits on tracks
//
// Revision 1.22  2000/04/29 19:57:22  caines
// Protection for zero global tracks and tpc hits not on tpc tracks
//
// Revision 1.21  2000/04/20 20:38:50  caines
// More fixing for the -1 problem
//
// Revision 1.20  2000/03/29 14:33:40  caines
// Fixed topology map for TPC only
//
// Revision 1.19  2000/03/10 21:54:18  lbarnby
// Turn on Kalman fitter as default for global tracks
//
// Revision 1.18  2000/03/09 23:31:17  lbarnby
// Protection against no TPC hits when creating tpc_groups
//
// Revision 1.17  2000/03/01 14:48:09  caines
// Removed references to scs_cluster
//
// Revision 1.16  2000/02/25 03:28:58  caines
// Stuff to fill bit map, cov correctly
//
// Revision 1.15  2000/02/25 02:38:27  caines
// Stuff to fill bit map, cov correctly
//
// Revision 1.14  2000/02/02 21:37:37  lbarnby
// CC5
//
// Revision 1.13  1999/11/27 18:21:41  fisyak
// Add test that primary vertex exists
//
// Revision 1.12  1999/11/16 20:58:41  wdeng
// Spiros's temporary solution to id_start_vertex puzzle
//
// Revision 1.11  1999/10/29 23:23:25  caines
// Removed scenario methods
//
// Revision 1.10  1999/10/01 21:16:03  wdeng
// Take out dst auxiliary table.
//
// Revision 1.9  1999/09/13 15:06:23  caines
// Added creation of garb(tphit) and garb(tptrack) so it is possible
// to run with TPC turned off
//
// Revision 1.8  1999/09/12 23:03:03  fisyak
// Move parameters into makers
//
// Revision 1.7  1999/07/28 02:18:19  caines
// Add in kalman filter flags
//
// Revision 1.6  1999/07/17 00:31:24  genevb
// Use StMessMgr
//
// Revision 1.5  1999/07/15 13:57:52  perev
// cleanup
//
// Revision 1.4  1999/07/08 19:09:51  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"

#if !defined(ST_NO_NAMESPACES)
using namespace units;
#endif
#include "TMath.h"
#include "StMatchMaker.h"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "tables/St_tcl_tpcluster_Table.h"
#include "tables/St_ctu_cor_Table.h"

#include "tables/St_svm_evt_match_Table.h" 
 

#include "global/St_egr_fitter_Module.h"
#define gufld   gufld_
extern "C" {void gufld(Float_t *, Float_t *);}


class St_tcl_tpcluster;
class St_ctu_cor;

ClassImp(StMatchMaker)
  
  //_____________________________________________________________________________
  StMatchMaker::StMatchMaker(const char *name):StMaker(name),
  m_egr_egrpar(0)
{
  drawinit=kFALSE;
  m_svtchicut = 0;
  m_useglobal = 4;
  m_usesvt    = 0;
  m_usetpc    = 4;
  m_usevert   = 0;
  m_flag      = 2;
}
//_____________________________________________________________________________
StMatchMaker::~StMatchMaker(){
}
//_____________________________________________________________________________
Int_t StMatchMaker::Init(){
  // Create tables
  //egr 
  m_egr_egrpar = new St_egr_egrpar("egr_egrpar",2);
  {
    egr_egrpar_st row;
//
    memset(&row,0,m_egr_egrpar->GetRowSize());
    row.debug[0]	 =          1; // flags for debug printing ;
    row.debug[1]	 =          0;
    row.debug[2]	 =          0;
    row.debug[3]	 =          0;
    row.debug[4]	 =          0;
    row.debug[5]	 =          0;
    row.debug[6]	 =          0;
    row.debug[7]	 =          0;
    row.debug[8]	 =          0;
    row.minfit	 =          2; // min no. of points on track ;
    row.mxtry	 =         10; // max no. of attempts to fit ;
    //Kalman - switch on by default
    if(m_Mode == 0){
    row.usetpc = 4; 
    row.useglobal = 4;
    gMessMgr->Info() << "Kalman fitting turned ON as default" << endm;
    }
    else{
    row.useglobal = 2; // set if to usematching to be used ;
    row.usetpc	 =  1; // set if TPC used in refit ;
    //row.useglobal = 2; // set if to usematching to be used ;
    //row.usetpc	 =  1; // set if TPC used in refit ;  

    gMessMgr->Info() << "Kalman fitting turned OFF" << endm;
    }
    row.useemc	 =          0; // set if EMC used in refit ;
    row.usesvt	 =          0; // set if SVT used in refit ;
    row.usetof	 =          0; // set if TOF used in refit ;
  // Helix

    row.usevert	 =          0; // Set if primary vertex used in refit ;
    row.prob[0]	 =         10; // probability cut in fit ;
    row.prob[1]	 =         10;
    row.svtchicut	 =  0; // SVT chi2 cut for adding SVT-only tracks ;
  //  row.svtchicut = m_svtchicut;
  //  row.useglobal = m_useglobal;
  //  row.usetpc    = m_usetpc;
  //  row.usesvt    = m_usesvt; 
  //  row.usevert   = m_usevert;
   m_egr_egrpar->AddAt(&row,0);
  //Use this as the GEANT pid to be used for the kalman filter for now 
    row.useglobal = 8;
    m_egr_egrpar->AddAt(&row,1);
  }


  AddRunCont(m_egr_egrpar);
 

  St_DataSetIter  svtpars(GetInputDB("svt"));
  m_svt_shape      = (St_svg_shape   *) svtpars("svgpars/shape");
  m_svt_config     = (St_svg_config  *) svtpars("svgpars/config");
  m_svt_geom       = (St_svg_geom    *) svtpars("svgpars/geom");
  m_srs_activea    = (St_srs_activea *) svtpars("srspars/srs_activea");
  m_srspar         = (St_srs_srspar  *) svtpars("srspars/srs_srspar");
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StMatchMaker::InitRun(int runnumber){
  // Set up copying of TPC tracks (no refit) if field off (B very small)
  Float_t x[3] = {0,0,0};
  Float_t b[3];
  gufld(x,b);
  Double_t B = b[2]*kilogauss;
  gMessMgr->Info() << "StMatchMaker::InitRun:B field is " << B/kilogauss << " kilogauss" << endm;
  if (B/kilogauss < 0.005){
    gMessMgr->Info() <<
      "StMatchMaker::InitRun: No field run: no refit, copy tracks into globtrk table" << endm;
    egr_egrpar_st *egr_egrpar = m_egr_egrpar->GetTable();
    egr_egrpar->usetpc = 1;
    egr_egrpar->usesvt = 1;
    egr_egrpar->useglobal  = 0;
  }
  return StMaker::InitRun(runnumber);
}
//_____________________________________________________________________________
Int_t StMatchMaker::Make(){
  PrintInfo();  
  
  int iMake = kStOK;
  int iRes = 0, i;
  const Float_t deltaZCut = 1; //(cm) Cut used in track length calc.

  St_dst_track     *globtrk     = new St_dst_track("globtrk",20000);  
  AddData(globtrk);
  
  dst_track_st* globtrkPtr  = globtrk->GetTable();
  for(  i=0; i<globtrk->GetTableSize(); i++, globtrkPtr++) {
    globtrkPtr->id_start_vertex = 0;
  } 
 
  St_dst_vertex *vertex = new St_dst_vertex("vertex",1); 
  AddGarb(vertex);   
  
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
 
  }
 
  if (! tptrack)    {tptrack = new St_tpt_track("tptrack",1); AddGarb(tptrack);}
  St_DataSet    *tpchits = GetInputDS("tpc_hits");
  St_tcl_tphit     *tphit     = 0;
  St_tcl_tpcluster *tpcluster = 0;
  if (tpchits) {
    tphit     = (St_tcl_tphit     *) tpchits->Find("tphit");
    tpcluster = (St_tcl_tpcluster *) tpchits->Find("tpcluster");
  }
  if (! tpcluster)    {tpcluster = new St_tcl_tpcluster("tpcluster",1); AddGarb(tpcluster);}
 if (! tphit)    {tphit = new St_tcl_tphit("tphit",1); AddGarb(tphit);}
  
  St_DataSet     *svtracks = GetInputDS("est");
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  
  St_stk_track   *stk_track   = 0;
  St_sgr_groups  *svt_groups  = 0;
  St_scs_spt     *scs_spt     = 0;
  St_svm_evt_match *evt_match = 0; 
  
  // Case svt tracking performed
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("EstSvtTrk");
    svt_groups= (St_sgr_groups *) svtracks->Find("EstGroups");
    evt_match=  (St_svm_evt_match *) svtracks->Find("EstMatch");
    
  }
  if (svthits) {
    scs_spt     = (St_scs_spt    *)  svthits->Find("scs_spt");
  }
  
  
  // Case silicon not there
  if (!stk_track) {stk_track = new St_stk_track("stk_track",1); AddGarb(stk_track);}
  if (!svt_groups)    {svt_groups = new St_sgr_groups("svt_groups",1); AddGarb(svt_groups);}
 if (!evt_match) { evt_match = new St_svm_evt_match("evt_match",1); AddGarb(evt_match);}

  if (!scs_spt)   {scs_spt = new St_scs_spt("scs_spt",1); AddGarb(scs_spt);}

  St_DataSet *ctf = GetInputDS("ctf");
  St_ctu_cor *ctb_cor = 0;
  if (!ctf) {
    if(Debug()) gMessMgr->Debug() << "St_ctf_Maker has not been called " << endm;
  } else {
    ctb_cor = (St_ctu_cor *)ctf->Find("ctb_cor"); 
    if (! ctb_cor) {ctb_cor = new St_ctu_cor("ctb_cor",1); AddGarb(ctb_cor);}
  }
  
  
 
  // Create groups table for tpc
  St_sgr_groups *tpc_groups;
  if (tphit->GetNRows() != 0){
    tpc_groups  = new St_sgr_groups("tpc_groups",tphit->GetNRows());   
    AddData(tpc_groups); 
  }
  else {
    tpc_groups = new St_sgr_groups("tpc_groups",1);
    AddGarb(tpc_groups);
  }
  
  // egr

   tcl_tphit_st  *spc   = tphit->GetTable();
   sgr_groups_st *tgroup = tpc_groups->GetTable();
   tpt_track_st  *ttrack = tptrack->GetTable();
   int count = 0;

   for( i=0; i<tphit->GetNRows(); i++, spc++){
     
       tgroup->id1 = spc->track;
       tgroup->id2 = i+1;
       //if( spc->flag !=0) {
       // tgroup->ident = -1;
       // }
       if( tgroup->id1 !=0){
	 if( ttrack[((int)tgroup->id1/1000)-1].flag < 0){
	 tgroup->ident = -10;
	 }
	 else{
	   tgroup->ident = 0;
	 }
       }
       else{
	 tgroup->ident = 0;
       }
       count++;
       tgroup++;
   }
   
   tpc_groups->SetNRows(count);

   //First call egr and make a copy of the tpc and svt tracks for evaluation
   //purposes - see exactly what the trackers do before re-fitting touches them
   
   if(Debug()){
     //

     //Change params so just a copy is made of the tracks
     egr_egrpar_st *egr_egrpar = m_egr_egrpar->GetTable();
     int usetpc = egr_egrpar->usetpc;
     egr_egrpar->usetpc = 1;
     int usesvt = egr_egrpar->usesvt;
     egr_egrpar->usesvt = 1;
     int useglobal = egr_egrpar->useglobal;
     egr_egrpar->useglobal  = 0;

     // Allocate some space for the tracks
     
     int Nrows = tptrack->GetNRows()+ stk_track->GetNRows();
     St_dst_track     *CpyTrk     = new St_dst_track("CpyTrk",Nrows);  
     AddData(CpyTrk);
     
     iRes = egr_fitter (tphit,    vertex,      tptrack , tpc_groups,
			scs_spt,m_egr_egrpar,stk_track, svt_groups,
			evt_match,CpyTrk);
     //	 ======================================================
     
     // Set pointers back as they were

     egr_egrpar->usetpc = usetpc;
     egr_egrpar->usesvt = usesvt;
     egr_egrpar->useglobal  = useglobal; 

     if (iRes !=kSTAFCV_OK) iMake = kStWarn;
     if (iRes !=kSTAFCV_OK) {
       gMessMgr->Warning() << "Problem on return from EGR_FITTER" << endm;}
      gMessMgr->Debug() << " finished calling egr_fitter" << endm;
   }
   

  //  Now call egr for real

  iRes = egr_fitter (tphit,    vertex,      tptrack , tpc_groups,
		     scs_spt,m_egr_egrpar,stk_track, svt_groups,
		     evt_match,globtrk);
  //	 ======================================================
  
  if (iRes !=kSTAFCV_OK) iMake = kStWarn;
  if (iRes !=kSTAFCV_OK) {
    gMessMgr->Warning() << "Problem on return from EGR_FITTER" << endm;}
  if(Debug()) gMessMgr->Debug() << " finished calling egr_fitter" << endm;

  // globtrk length calculation  
  dst_track_st *globtrkPtr1  = globtrk->GetTable();
  for( Int_t no_rows=0; no_rows<globtrk->GetNRows(); no_rows++, globtrkPtr1++)
    {
      //Convenient place to also zero globtrk bitmap
      globtrkPtr1->map[0]= 0UL; globtrkPtr1->map[1]= 0UL;
      if( globtrkPtr1->iflag<0 ) { globtrkPtr1->length = 0; continue; }
      Float_t dip   = atan(globtrkPtr1->tanl);
      Int_t    h    = (globtrkPtr1->icharge > 0 ? -1 : 1);
      Float_t phase = globtrkPtr1->psi*degree-h*pi/2;
      Float_t curvature = globtrkPtr1->curvature;
      Float_t x0 = globtrkPtr1->r0 * cos(globtrkPtr1->phi0 * degree);
      Float_t y0 = globtrkPtr1->r0 * sin(globtrkPtr1->phi0 * degree);
      Float_t z0 = globtrkPtr1->z0;
      StThreeVectorD origin(x0, y0, z0);  
      StHelixD globHelix(curvature, dip, phase, origin, h);
            
      Float_t globLength;
      //default is larger than cut, beware curvature=0 (straight) tracks
      Float_t deltaZ = 2*deltaZCut;
      if (curvature > 1E-20) deltaZ= (2*M_PI*globtrkPtr1->tanl)/curvature;
      deltaZ = (deltaZ >= 0) ? deltaZ : (-deltaZ); 
      //If case where dist. moved in z after 1 period is less than cut (1 cm) 
      //use alternative pathLength method (prevents large false lengths)     
      if (deltaZ >= deltaZCut) {
	StThreeVectorD lastPoint(globtrkPtr1->x_last[0], 
				globtrkPtr1->x_last[1],globtrkPtr1->x_last[2]);
	globLength = globHelix.pathLength(lastPoint); 
      }
      
      else {
	globLength = 
	  globHelix.pathLength(globtrkPtr1->x_last[0],globtrkPtr1->x_last[1]);
	  }
      globtrkPtr1->length = (globLength>0) ? globLength : (-globLength);  
    }
  
  // Fill bit map in glob trk

  spc   = tphit->GetTable();
  tgroup = tpc_groups->GetTable();
  dst_track_st * track  = globtrk->GetTable();
  
  int spt_id = 0;
  int row = 0;
  bool isset;
  
  for( i=0; i<tpc_groups->GetNRows(); i++, tgroup++){

    if( tgroup->id1 != 0 && tgroup->ident > -5){
      spt_id = tgroup->id2-1;
      row = spc[spt_id].row/100;
      row = spc[spt_id].row - row*100;

      if(  spc[spt_id].id_globtrk-1 < 0){
	cout << spt_id << " " << spc[spt_id].id_globtrk<< " " << endl;
	assert(0);
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


  scs_spt_st *s_spc = scs_spt->GetTable();
  sgr_groups_st *sgroup = svt_groups->GetTable();
    
  for( i=0; i<svt_groups->GetNRows(); i++, sgroup++){

    if( sgroup->id1 != 0){
      spt_id = sgroup->id2-1;
      row = s_spc[spt_id].id_wafer/1000;
      if(  s_spc[spt_id].id_globtrk-1 < 0){
	cout << spt_id << " " << s_spc[spt_id].id_globtrk<< " " << endl;
	assert(0);
      }
      if( row>7)row=7;
      track[s_spc[spt_id].id_globtrk-1].map[0] |= (1UL<<row);
      
    }
    
  }
  

  return iMake;
}

//_____________________________________________________________________________

