// $Id: St_stk_Maker.cxx,v 1.7 1998/10/31 00:26:21 fisyak Exp $
// $Log: St_stk_Maker.cxx,v $
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
#include "St_stk_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "svt/St_stk_am_Module.h"
#include "svt/St_ste_am_Module.h"
#include "svt/St_stk_am_Module.h"
#include "svt/St_stk_am_init_Module.h"
#include "svt/St_sgr_am_Module.h"

ClassImp(St_stk_Maker)

//_____________________________________________________________________________
  St_stk_Maker::St_stk_Maker(const char *name, const char *title):
StMaker(name,title),
m_stk_stkpar(0),
m_pix_info(0),
m_stk_vtx(0),
m_stk_vtx_direct(0),
m_stk_filler(0),
m_config(0),
m_geom(0)
{
   drawinit=kFALSE;
   m_mode = 2;
   m_method = 2;
   m_fill = 1;
   m_direct = 1;
   m_nlayers = 3;
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
   m_ifstk = kFALSE;
}
//_____________________________________________________________________________
St_stk_Maker::~St_stk_Maker(){
}
//_____________________________________________________________________________
Int_t St_stk_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));
   m_stk_stkpar = (St_stk_stkpar *) local("svt/stkpars/stk_stkpar");
   stk_stkpar_st *stk_stkpar = m_stk_stkpar->GetTable();
   stk_stkpar->mode = m_mode;
   stk_stkpar->method = m_method;
   stk_stkpar->fill = m_fill;
   stk_stkpar->direct = m_direct;
   stk_stkpar->nlayers = m_nlayers;
   stk_stkpar->c1norm[0] = m_c1norm[0];
   stk_stkpar->c1norm[1] = m_c1norm[1]; 
   stk_stkpar->c1norm[2] = m_c1norm[2];
   stk_stkpar->c2norm[0] = m_c2norm[0];
   stk_stkpar->c2norm[1] = m_c2norm[1];
   stk_stkpar->c2norm[2] = m_c2norm[2];
   stk_stkpar->vertex = m_vertex;
   stk_stkpar->chicut[0] = m_chicut[0];
   stk_stkpar->chicut[1] = m_chicut[1];
   stk_stkpar->chi_filter = m_chi_filter;
   stk_stkpar->spt_mark = m_spt_mark;
   stk_stkpar->long_tracks = m_long_tracks;
   stk_stkpar->th_init = m_th_init;
   stk_stkpar->th_max = m_th_max;
   stk_stkpar->nitermax = m_nitermax;
   stk_stkpar->niternull = m_niternull;
   stk_stkpar->sec_factor = m_sec_factor;

   m_stk_vtx        = (St_stk_vtx *)        local("global/vertices/stk_vtx");
   m_stk_vtx_direct = (St_stk_vtx_direct *) local("global/vertices/stk_vtx_direct");
   m_stk_stkpar     = (St_stk_stkpar *)     local("svt/stkpars/stk_stkpar");
   m_stk_filler     = (St_stk_filler *)     local("svt/stkpars/stk_filler");
   m_config         = (St_svg_config *)     local("svt/svgpars/config");
   m_geom           = (St_svg_geom *)       local("svt/svgpars/geom");
   m_pix_info       = (St_sgr_pixmap *)     local("svt/sgrpars/pix_info");
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_stk_Maker::Make(){
  //  PrintInfo();
  if (!m_DataSet->GetList()){  // event/data/svt/tracks
     St_DataSetIter geant(gStChain->DataSet("geant"));
     St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("g2t_track");
     St_g2t_event   *g2t_event    = (St_g2t_event  *) geant("g2t_event");
     St_g2t_vertex  *g2t_vertex   = (St_g2t_vertex *) geant("g2t_vertex");
     St_g2t_svt_hit *g2t_svt_hit  = (St_g2t_svt_hit *)geant("g2t_svt_hit");
     if (g2t_track && g2t_event && g2t_vertex && g2t_svt_hit) {
       St_sgr_groups *candidate_groups = new St_sgr_groups("candidate_groups",30000);
                                                                          m_DataSet->Add(candidate_groups);
       St_sgr_groups *groups      = new St_sgr_groups("groups",30000);    m_DataSet->Add(groups);
       St_sgr_groups *mcgroups    = new St_sgr_groups("mcgroups",30000);  m_DataSet->Add(mcgroups);
       St_stk_track  *stk_track   = new St_stk_track("stk_track",6000);   m_DataSet->Add(stk_track);
       St_stk_kine   *stk_kine    = new St_stk_kine("stk_kine",6000);     m_DataSet->Add(stk_kine);
       St_stk_track  *stk_mctrack = new St_stk_track("stk_mctrack",6000); m_DataSet->Add(stk_mctrack);
       St_stk_kine   *stk_mckine  = new St_stk_kine("stk_mckine",6000);   m_DataSet->Add(stk_mckine);
       St_ste_teval  *ste_teval   = new St_ste_teval("ste_teval",10000);  m_DataSet->Add(ste_teval);
       St_ste_teff   *ste_teff    = new St_ste_teff("ste_teff",1);        m_DataSet->Add(ste_teff);
     //
       St_DataSet    *geom = gStChain->DataSet("geom");
       St_DataSetIter run(geom);
       St_g2t_gepart *g2t_gepart  = (St_g2t_gepart *) run("g2t_gepart");
       if (!g2t_gepart){
         g2t_gepart   = new St_g2t_gepart("g2t_gepart",1);
         run.Add(g2t_gepart);
       }
     //
       St_DataSetIter data(gStChain->DataSet("svt_hits"));
       St_scs_spt    *scs_spt      = (St_scs_spt *) data("scs_spt");
				      // exec run_stk  
       if (m_ifstk){
         Int_t Res_stk = stk_am(m_stk_stkpar,
                            g2t_track,g2t_event,g2t_vertex,
                            scs_spt,
                            m_stk_vtx,m_stk_vtx_direct,
                            groups,stk_track,stk_kine,mcgroups,
                            stk_mctrack,stk_mckine,
                            m_stk_filler,m_config,m_geom);
				      //eval_stk
         Int_t res_ste = ste_am(g2t_gepart,g2t_svt_hit,g2t_track,g2t_event,g2t_vertex,
                            scs_spt,
                            m_stk_vtx,
                            groups,stk_track,stk_kine,mcgroups,
                            stk_mctrack,stk_mckine,
                            ste_teval,ste_teff);
       }
       else{			      //sgr 
         Int_t Res_stk_init = stk_am_init(m_stk_stkpar,
                                groups,stk_track,stk_kine,mcgroups,
                                stk_mctrack,stk_mckine);
         if (Res_stk_init !=  kSTAFCV_OK) {
	   cout << "Problem on return from STK_AM_INIT" << endl;
         }
         Int_t Res = sgr_am(m_stk_vtx,m_geom,scs_spt,
                       groups,
                       m_pix_info,candidate_groups,stk_track);
  
//        if (staf_status(1) .ne. stafcv_ok) goto NEXT_EVENT
		   //eval
         cout << "Calling STE_AM..." << endl;

         Int_t Res_ste = ste_am(g2t_gepart,g2t_svt_hit,g2t_track,g2t_event,g2t_vertex,
                              scs_spt,m_stk_vtx,
                              groups,stk_track,stk_kine,mcgroups,
                              stk_mctrack,stk_mckine,
                              ste_teval,ste_teff);

         if (Res_ste != kSTAFCV_OK) {
           cout << "Problem on return from STE_AM" << endl;
	 }
       }
     }	  
   } 
  return kStOK;
}
//_____________________________________________________________________________
void St_stk_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_stk_Maker.cxx,v 1.7 1998/10/31 00:26:21 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

