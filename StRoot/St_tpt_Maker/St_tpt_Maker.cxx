// $Id: St_tpt_Maker.cxx,v 1.10 1998/10/31 00:26:23 fisyak Exp $
// $Log: St_tpt_Maker.cxx,v $
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
#include "tpc/St_tte_track_Module.h"
#include "tpc/St_tde_new_Module.h"
#include "tpc/St_tte_Module.h"
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
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_tpt_Maker::Make(){
  //  PrintInfo();
   if (!m_DataSet->GetList()) {// If DataSet list empty then create it
     St_DataSet *tpc_data =  gStChain->DataSet("tpc_hits");
     if (tpc_data) {// Clusters exist -> do tracking
       St_DataSetIter next(tpc_data);
       St_tcl_tphit      *tphit = (St_tcl_tphit     *) next("tphit");
       St_tcl_tpc_index  *index = (St_tcl_tpc_index *) next("index");
       if (!index) {index = new St_tcl_tpc_index("index",200000); next.Add(index);}

       St_tpt_track  *tptrack = new St_tpt_track("tptrack",20000); m_DataSet->Add(tptrack);
       St_tte_mctrk  *mctrk   = new St_tte_mctrk("mctrk",20000);   m_DataSet->Add(mctrk);
       St_tte_eval *evaltrk   = new St_tte_eval("evaltrk",20000);  m_DataSet->Add(evaltrk);
       St_tte_res      *res   = new St_tte_res("res",1);           m_DataSet->Add(res);
       St_DataSetIter geant(gStChain->DataSet("geant"));
       St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("g2t_track");
       St_g2t_tpc_hit *g2t_tpc_hit  = (St_g2t_tpc_hit *)geant("g2t_tpc_hit");
	 //tpt
       if (!m_iftte) Int_t Res_tpt = tpt(m_tpt_pars,tphit,tptrack);
	 //tte_track
       else {
         if (g2t_tpc_hit && g2t_track) Int_t Res_tte_track =  tte_track(tptrack,tphit,g2t_tpc_hit,g2t_track,index,m_type);
       }
	 //tid
       Int_t Res_tde = tde_new(m_tdeparm,tphit,tptrack,m_tpg_pad_plane);
  	 //tte
       if (g2t_tpc_hit && g2t_track) {
         Int_t Res_tte = tte(tptrack,tphit,
                           g2t_tpc_hit,g2t_track,
                           index,m_type,evaltrk,mctrk,res,m_tte_control);
       }
     }
   }
 return kStOK;
}
//_____________________________________________________________________________
void St_tpt_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_tpt_Maker.cxx,v 1.10 1998/10/31 00:26:23 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

