// $Id: St_tpt_Maker.cxx,v 1.4 1998/08/14 15:25:41 fisyak Exp $
// $Log: St_tpt_Maker.cxx,v $
// Revision 1.4  1998/08/14 15:25:41  fisyak
// Move out tpg from run
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
#include "iostream.h"
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
       if (!m_tpg_pad_plane) {
         cout << " St_run_Maker:tpg_pad_plane does not exist" << endl;
         cout << "TPC geometry parameter tables are incomplete."<< endl;
         SafeDelete(tpgpar);
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
  printf("* $Id: St_tpt_Maker.cxx,v 1.4 1998/08/14 15:25:41 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

