// $Id: St_tpt_Maker.cxx,v 1.1 1998/07/21 00:36:46 fisyak Exp $
// $Log: St_tpt_Maker.cxx,v $
// Revision 1.1  1998/07/21 00:36:46  fisyak
// tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tpt_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_tpt_Maker.h"
#include "StChain.h"
#include "St_DataSet.h"
#include "St_XDFFile.h"
#include "tpc/St_tpt_Module.h"
#include "tpc/St_tde_new_Module.h"
ClassImp(St_tpt_Maker)

//_____________________________________________________________________________
St_tpt_Maker::St_tpt_Maker(){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_tpt_Maker::St_tpt_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_tpt_Maker::~St_tpt_Maker(){
 if (m_DataSet) delete m_DataSet;
 m_DataSet = 0;
}
//_____________________________________________________________________________
void St_tpt_Maker::Clear(Option_t *option){
  if (m_DataSet) {delete m_DataSet; m_DataSet = 0;}
}

//_____________________________________________________________________________
void St_tpt_Maker::Finish(){ 
 Clear();
}
//_____________________________________________________________________________
void St_tpt_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->GetParams());
   St_DataSet *tpc = local("tpc");
   if (! tpc)  tpc = local.Mkdir("tpc");
// tpg parameters
   St_DataSet *tpgpar = local("tpc/tpgpar");
   if (tpgpar){
       St_DataSetIter partable(tpgpar);
       m_tpg_pad_plane = (St_tpg_pad_plane *) partable("tpg_pad_plane");
       if (!(m_tpg_pad_plane)) 
       printf("tpc/tpgpar is not initialized. Please add tss_Maker to your chain\n");
   }
// tpt parameters
   St_DataSet *tptpars = local("tpc/tptpars");
   if (tptpars){
     St_DataSetIter partable(tptpars);
     m_tpt_pars = (St_tpt_pars *) partable("tpt_pars");
     m_tpt_spars = (St_tpt_spars *) partable("tpt_spars");
     if (!(m_tpt_pars && m_tpt_spars)) SafeDelete(tptpars);
   }
   if (!tptpars){
//   Char_t *tpt_pars = "${STAR}/params/tpc/tpt_pars.xdf";
     Char_t *tpt_pars = "/afs/rhic/star/packages/dev/params/tpc/tpt_pars.xdf";
     St_XDFFile::GetXdFile(tpt_pars,tpc);
     tptpars = local("tpc/tptpars");
     St_DataSetIter partable(tptpars);
     m_tpt_pars = (St_tpt_pars *) partable("tpt_pars");
     m_tpt_spars = (St_tpt_spars *) partable("tpt_spars");
   }
// tid parameters
   St_DataSet *tidpars = local("tpc/tidpars");
   if (tidpars){
     St_DataSetIter partable(tidpars);
     m_tdeparm = (St_tdeparm *) partable("tdeparm");
     m_tpipar  = (St_tpipar *)  partable("tpipar");
     if (!(m_tdeparm && m_tpipar)) SafeDelete(tidpars);
   }
   if (!tidpars){
//   Char_t *tid_pars = "$STAR/params/tpc/tid_pars.xdf";
     Char_t *tid_pars = "/afs/rhic/star/packages/dev/params/tpc/tid_pars.xdf";
     St_XDFFile::GetXdFile(tid_pars,tpc);
     tidpars = local("tpc/tidpars");
     St_DataSetIter partable(tidpars);
     m_tdeparm = (St_tdeparm *) partable("tdeparm");
     m_tpipar  = (St_tpipar *)  partable("tpipar");
   }

// Create Histograms    
   StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_tpt_Maker::Make(){
  //  PrintInfo();
   St_DataSetIter tpc_tracks(m_DataSet);
   if (!m_DataSet->GetList()) {// If DataSet list empty then create it
     St_DataSet *tpc_data =  gStChain->Maker("tcl_Maker")->DataSet();
     if (tpc_data) {// Clusters exist -> do tracking
       St_DataSetIter next(tpc_data);
       St_tcl_tphit  *tphit = (St_tcl_tphit *) next("tphit");
       St_tpt_track  *tptrack = (St_tpt_track *) tpc_tracks("tptrack");
       if (!tptrack) {tptrack = new St_tpt_track("tptrack",20000); tpc_tracks.Add(tptrack);}
       Int_t tpt_res = tpt(m_tpt_pars,tphit,tptrack);
       Int_t tde_res = tde_new(m_tdeparm,tphit,tptrack,m_tpg_pad_plane);
     }
   }
 return kSTAFCV_OK;
}
//_____________________________________________________________________________
void St_tpt_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_tpt_Maker.cxx,v 1.1 1998/07/21 00:36:46 fisyak Exp $    *\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

