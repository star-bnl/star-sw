// $Id: St_stk_Maker.cxx,v 1.21 2000/06/23 16:52:51 fisyak Exp $
// $Log: St_stk_Maker.cxx,v $
// Revision 1.21  2000/06/23 16:52:51  fisyak
// remove params
//
// Revision 1.20  2000/06/01 21:10:01  caines
// Turn off mc track filling as default
//
// Revision 1.19  2000/05/24 14:19:03  caines
// Use prevertex for search not geant + find own vertex for alignment
//
// Revision 1.18  2000/01/31 23:54:37  caines
// Add code for SVT vtx finding - Not yet switched on
//
// Revision 1.17  1999/07/20 04:59:02  caines
// Temporary fix using geant vtx for tracking
//
// Revision 1.16  1999/07/15 13:58:21  perev
// cleanup
//
// Revision 1.15  1999/07/15 00:19:59  caines
// Switch to turn tracking on/off
//
// Revision 1.14  1999/05/05 19:31:52  fisyak
// Add sanity check and simplify
//
// Revision 1.13  1999/04/21 16:05:30  fisyak
// move stk_stkpar into Maker
//
// Revision 1.12  1999/03/13 00:26:48  perev
// New maker schema
//
// Revision 1.11  1999/02/25 20:29:48  caines
// Changed Histo names
//
// Revision 1.10  1999/02/16 21:17:54  caines
// Added QA histograms
//
// Revision 1.9  1999/02/16 18:15:46  fisyak
// Check in the latest updates to fix them
//
// Revision 1.8  1999/01/02 19:08:21  fisyak
// Add ctf
//
// Revision 1.7  1998/10/31 00:26:21  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:46  perev
// cleanup
//
// Revision 1.5  1998/09/23 20:23:12  fisyak
// Prerelease SL98h
//
// Revision 1.4  1998/09/15 20:55:26  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.3  1998/08/26 12:15:10  fisyak
// Remove asu & dsl libraries
//
// Revision 1.2  1998/08/18 14:05:03  fisyak
// Add to bfc dst
//
// Revision 1.1  1998/08/12 13:09:04  fisyak
// Add stk_Maker
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_stk_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "math_constants.h"
#include "St_stk_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "svt/St_stk_am_Module.h"
#include "svt/St_ste_am_Module.h"
#include "svt/St_stk_am_Module.h"
#include "svt/St_stk_am_init_Module.h"
#include "svt/St_sgr_am_Module.h"
#include "svt/St_spr_svt_Module.h"
#include "tables/St_dst_vertex_Table.h"
#include "TH1.h"
#include "TH2.h"

long sft_main( St_scs_spt *scs_spt, St_dst_vertex *vertex);

ClassImp(St_stk_Maker)
  
//__________________________________________________________________________
  St_stk_Maker::St_stk_Maker(const char *name):
    StMaker(name),
    m_stk_stkpar(0),
    m_pix_info(0),
    m_stk_vtx(0),
    m_stk_vtx_direct(0),
    m_stk_filler(0),
    m_config(0),
    m_geom(0),
    m_sprpar(0),
    m_q_pt(0),
    m_frac_used(0),
    m_x0y0(0),
    m_z0(0),
    m_azimuth(0),
    m_tan_dip(0),
    m_dedx(0),
    m_vtx_z(0)  
{
  m_mode = 2;
  m_method = 2;
  m_fill = 1;
  m_direct = 0;
  m_nlayers = 4;
  m_c1norm[0] = 3.47; m_c1norm[1] =   280.; m_c1norm[2] = -13.7 ;
  m_c2norm[0] = 45.5; m_c2norm[1] = 14200.; m_c2norm[2] = -17.5;
  m_vertex = 0;
  m_chicut[0] = 1e9; m_chicut[1] = 1e9;
  m_chi_filter = 1;
  m_spt_mark = 1;
  m_long_tracks = 1;
  m_th_init = .5;
  m_th_max = 10.0;
  m_nitermax = 7;
  m_niternull = 1000;
  m_sec_factor = 6.0;
 
}
//_____________________________________________________________________________
St_stk_Maker::~St_stk_Maker(){
}
//_____________________________________________________________________________
Int_t St_stk_Maker::Init(){
  // Create tables
  St_DataSetIter       gime(GetDataBase("global/vertices"));
  m_stk_vtx        = (St_stk_vtx *)        gime("stk_vtx");
  m_stk_vtx_direct = (St_stk_vtx_direct *) gime("stk_vtx_direct");
  
  gime.Reset(GetDataBase("svt"));
  
  m_config         = (St_svg_config *)     gime("svgpars/config");
  m_geom           = (St_svg_geom *)       gime("svgpars/geom");
  m_pix_info       = (St_sgr_pixmap *)     gime("sgrpars/pix_info");
  m_sprpar         = (St_spr_sprpar *)     gime("sprpars/sprpar");
  m_stk_stkpar     = (St_stk_stkpar *)     gime("stkpars/stk_stkpar");
  m_stk_filler     = (St_stk_filler *)     gime("stkpars/stk_filler");
  m_stk_stkpar     = new St_stk_stkpar("stk_stkpar",1);
  stk_stkpar_st stk_stkpar;
  stk_stkpar.direct           = m_direct;  // control call to svt_tracking_direct ;
  stk_stkpar.fill             = 1;        // control call to svt_tracking_fill3 ;
  stk_stkpar.long_tracks      = m_long_tracks; // 1=get rid of short tracks ;
  stk_stkpar.method           = m_method;      // fit as prim, secondary, etc. ;
  stk_stkpar.mode             = m_mode;        // track pri, sec, or both, ... ;
  stk_stkpar.ngood            = 9335576;       // number of checked tracks in svt_track ;
  stk_stkpar.nitermax         = m_nitermax;    // max number iteration in fill3 ;
  stk_stkpar.niternull        = m_niternull;   // max number iter without track change ;
  stk_stkpar.nlayers          = m_nlayers;     // number of super layers ;
  stk_stkpar.secondary        = 0;             // open gates for secondaries ;
  stk_stkpar.spt_mark         = m_spt_mark;    // 1 = remove used space points ;
  stk_stkpar.vertex           = m_vertex;      // control call to svt_vertex_direct ;
  stk_stkpar.c1norm[0]        = m_c1norm[0];   // norm factors for chi1 (circle fit) ;
  stk_stkpar.c1norm[1]        = m_c1norm[1]; 
  stk_stkpar.c1norm[2]        = m_c1norm[2];
  stk_stkpar.c2norm[0]        = m_c2norm[0];   // norm factors for chi from phi-z fit ;
  stk_stkpar.c2norm[1]        = m_c2norm[1];
  stk_stkpar.c2norm[2]        = m_c2norm[2];
  stk_stkpar.chi_filter       = m_chi_filter;  // parameter to turn chi filtering on/off ;
  stk_stkpar.chibreak         = 0;             // value of zchi2 that triggers sec fitting ;
  stk_stkpar.vertex           = m_vertex;
  stk_stkpar.chicut[0]        = m_chicut[0];   // cut on chisqr of tracks to be accepted ;
  stk_stkpar.chicut[1]        = m_chicut[1];
  stk_stkpar.sec_factor       = m_sec_factor;  // factor to loosen cones by for secondary ;
  stk_stkpar.th_init          = m_th_init;     // initial th max ;
  stk_stkpar.th_max           = m_th_max;      // largest step ;
  stk_stkpar.th_step          = 0;             // th step ;
  stk_stkpar.ssd_fac          = 1;             // factor to open the cones for the ssd ;
  stk_stkpar.fast_search      = 1;             // flag for the fast search in stk_fillX.F ;
  m_stk_stkpar->AddAt(&stk_stkpar,0);
  
  // Create Histograms
  m_q_pt      = new TH1F("StkChargeOverPt","Charge/pt of reconstructed svt tracks",100,-20.,20.);
  m_frac_used = new TH1F("StkHitsUsed"    ,"Fraction of hits used on svt tracks"  ,100,0.,1.2);
   m_x0y0 = new TH2F("StkX0y0"               ,"X0 vs y0 of svt tracks"  ,100,-30.,30.,100,-30.,30.);
   m_z0 = new TH1F("StkZ0"    ,"Z0 of svt tracks"  ,100,-25.,25.);
  m_azimuth   = new TH1F("StkAzimuth"     ,"Azimuthal distribution of svt tracks" ,60,0.,360.0);
  m_tan_dip   = new TH1F("StkTanDip"      ,"Distribution of the svt dip angle"    ,100,-1.5,1.5);
  m_dedx      = new TH2F("StkDedx"        ,"Svt dE/dx distribution"               ,100,0.,2.0,100,0.,0.01);
   m_vtx_z     = new TH1F("StkSvtVert"        ,"Z SVT - Z TPC Primary vertex resolution"        ,100,-0.1,0.1);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_stk_Maker::Make()
{
  
  const Int_t maxNofTracks = 50000;
  
  St_DataSetIter geant(GetDataSet("geant"));
  St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("g2t_track");
  St_g2t_event   *g2t_event    = (St_g2t_event  *) geant("g2t_event");
  St_g2t_vertex  *g2t_vertex   = (St_g2t_vertex *) geant("g2t_vertex");
  St_g2t_svt_hit *g2t_svt_hit  = (St_g2t_svt_hit *)geant("g2t_svt_hit");
  if (!(g2t_track && g2t_event && g2t_vertex && g2t_svt_hit)) {
    cout << "St_stk_Maker:: No GEANT information" << endl;
    return kStWarn;
  }
  St_sgr_groups *groups      = 0;
  St_sgr_groups *candidate_groups   = 0;
  St_sgr_groups *mcgroups    = 0;
  St_stk_track  *stk_track   = 0;
  St_stk_kine   *stk_kine    = 0;
  St_stk_track  *stk_mctrack = 0;
  St_stk_kine   *stk_mckine  = 0;

  candidate_groups = new St_sgr_groups("candidate_groups",maxNofTracks);
  m_DataSet->Add(candidate_groups);
  groups      = new St_sgr_groups("groups",4*maxNofTracks);
  m_DataSet->Add(groups);
  mcgroups    = new St_sgr_groups("mcgroups",maxNofTracks);  
  m_DataSet->Add(mcgroups);
  stk_track   = new St_stk_track("stk_track",maxNofTracks);
  m_DataSet->Add(stk_track);
  stk_kine    = new St_stk_kine("stk_kine",maxNofTracks/5);
  m_DataSet->Add(stk_kine);
  stk_mctrack = new St_stk_track("stk_mctrack",maxNofTracks/5);
  m_DataSet->Add(stk_mctrack);
  stk_mckine  = new St_stk_kine("stk_mckine",maxNofTracks/5);
  m_DataSet->Add(stk_mckine);
  
  //
  St_scs_spt    *scs_spt      = (St_scs_spt *)GetDataSet("svt_hits/.data/scs_spt");
  if (! scs_spt) {
    cout << "St_stk_Maker:: No SVT space points" << endl;
    return kStWarn;
  }

  // 			exec run_stk  
  Int_t Res_stk_init = stk_am_init(m_stk_stkpar,
				   groups,stk_track,stk_kine,mcgroups,
				   stk_mctrack,stk_mckine);
  if (Res_stk_init !=  kSTAFCV_OK) {
    cout << "Problem on return from STK_AM_INIT" << endl;
  }
 
  //Get pointer to svt vertex
 
  stk_vtx_st *stk_vtx = m_stk_vtx->GetTable();

  //pointer to preVertex dataset
  St_DataSet *preVertex = GetDataSet("preVertex"); 
  
  //iterator
  St_DataSetIter preVertexI(preVertex);
  
  //pointer to preVertex
  St_dst_vertex  *preVtx  = (St_dst_vertex *)preVertexI("preVertex");
  int numRowPreVtx = preVtx->GetNRows();
  preVtx->ReAllocate(numRowPreVtx+1);


  // Find SVT vertex
  Int_t Res_sft= sft_main(scs_spt,preVtx);

  if (Res_sft !=  kSTAFCV_OK) {
	cout << " Problem on return from SFT" << endl;
	
	return kStOK;}


  if (preVtx) {
    dst_vertex_st *preVtxPtr = preVtx->GetTable();
    
    for (Int_t i = 0; i <preVtx->GetNRows();i++,preVtxPtr++) {
      
      if (preVtxPtr->iflag == 101) {
        stk_vtx->x[0] = preVtxPtr->x;
        stk_vtx->x[1] = preVtxPtr->y;
        stk_vtx->x[2] = preVtxPtr->z;
      }
    }
  }
  
  Int_t Res_sgr = sgr_am(m_stk_vtx,m_geom,
			 scs_spt,groups,
			 m_pix_info,candidate_groups,stk_track);
  if (Res_sgr !=  kSTAFCV_OK) {cout << " Problem on return from SGR" << endl;}
 
  Int_t Res_stk =  stk_am(m_stk_stkpar,
			  g2t_track,g2t_event,g2t_vertex,
			  scs_spt,
			  m_stk_vtx,m_stk_vtx_direct,
			  groups,stk_track,stk_kine,mcgroups,
			  stk_mctrack,stk_mckine,
			  m_stk_filler,m_config,m_geom );
  if (Res_stk !=  kSTAFCV_OK) {cout << " Problem on return from STK_AM" << endl;}


  Int_t Res_spr_svt = spr_svt(m_sprpar, stk_kine,
			      m_geom, scs_spt,
			      groups, stk_track);
  if (Res_spr_svt != kSTAFCV_OK) {cout << "Problem on return from SPR_SVT"<< endl;}
  
  MakeHistograms(); //tracking histograms

  return kStOK;
}
// --------------------------------------------------------------------------
void St_stk_Maker::MakeHistograms() {
  
  // 		Create an iterator
  St_DataSetIter svt_tracks(m_DataSet);
  // 		Create iterator for space points
  
  St_DataSetIter hits(GetDataSet("svt_hits"));
  
  
  //		Get the table:
  St_stk_track *spr = 0;
  St_sgr_groups *gpr = 0;
  St_scs_spt *spc = 0;
  //pointer to preVertex dataset
  St_DataSet *preVertex = GetDataSet("preVertex"); 
  
  //iterator
  St_DataSetIter preVertexI(preVertex);
  
  //pointer to preVertex
  St_dst_vertex*  vtx  = (St_dst_vertex *)preVertexI("preVertex");

  
  spr  = (St_stk_track  *) svt_tracks.Find("stk_track");  
  gpr  = (St_sgr_groups *) svt_tracks.Find("groups");
  spc  = (St_scs_spt    *) hits.Find("scs_spt");
  
  if(Debug()) cout << "Filling SVT track histograms" << endl;
  
  if (gpr && spc) 
    m_frac_used->Fill((float)gpr->GetNRows()/(float)spc->GetNRows());
  
  if (spr) {
    stk_track_st *r = spr->GetTable();
    for(Int_t i=0; i<spr->GetNRows();i++,r++){
      Int_t flag = r->flag;
      if(!flag>0) 	continue;
      m_q_pt->Fill(r->invpt);
      m_azimuth->Fill(r->psi);
      m_tan_dip->Fill(r->tanl); 
      m_x0y0->Fill((float)(r->r0*cos(r->phi0*C_RAD_PER_DEG)),(float)(r->r0*sin(r->phi0*C_RAD_PER_DEG)));
      m_z0->Fill(r->z0); 
      
      // Fill dedx
      Float_t pt = 1./r->invpt;
      Float_t pz = r->tanl/r->invpt;
      Float_t p = sqrt(pt*pt+pz*pz);
      
      m_dedx->Fill(p,r->dedx[0]);
    }
  }


  if (vtx) {
    float z_svt=999.;
    float z_tpc=-999.;
    dst_vertex_st *preVtxPtr = vtx->GetTable();
    
    for (Int_t i = 0; i <vtx->GetNRows();i++,preVtxPtr++) {
      
      if (preVtxPtr->iflag == 201) {
	z_svt = preVtxPtr->z;
      }
      else if(preVtxPtr->iflag == 101) {
	z_tpc = preVtxPtr->z;
      }
    }
    m_vtx_z->Fill(z_tpc-z_svt);
  }
    
}

