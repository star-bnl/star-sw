// $Id: St_ebye_Maker.cxx,v 1.4 1998/09/23 20:22:57 fisyak Exp $
// $Log: St_ebye_Maker.cxx,v $
// Revision 1.4  1998/09/23 20:22:57  fisyak
// Prerelease SL98h
//
// Revision 1.3  1998/09/15 20:55:20  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.2  1998/08/07 19:26:10  dhammika
// event by event chain in root
//
// Revision 1.1  1998/08/05 14:33:36  fisyak
// Add ebye
//
// Revision 1.2  1998/07/21 01:04:39  fisyak
// Clean up
//
// Revision 1.1  1998/07/21 00:36:46  fisyak
// tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ebye_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include "St_ebye_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"

#include "St_sca_switch_Table.h"
#include "St_sca_filter_const_Table.h"
#include "St_foo_dst_event_summary_Table.h"
#include "St_foo_dst_track_Table.h"

#include "St_particle_Table.h"

#include "ebye/St_sca_filter_Module.h"
#include "ebye/St_sca_runsca_Module.h"

ClassImp(St_ebye_Maker)

//_____________________________________________________________________________
St_ebye_Maker::St_ebye_Maker():
m_sca_switch(0),
m_sca_const(0),
m_sca_filter_const(0),
m_dst_event_summary(0),
m_dsttrack(0),
m_particle(0),
m_sca_in(0),
m_sca_out(0),
m_sca_prior(0),
m_sca_ensemble_ave(0)
{
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_ebye_Maker::St_ebye_Maker(const char *name, const char *title):StMaker(name,title),
m_sca_switch(0),
m_sca_const(0),
m_sca_filter_const(0),
m_dst_event_summary(0),
m_dsttrack(0),
m_particle(0),
m_sca_in(0),
m_sca_out(0),
m_sca_prior(0),
m_sca_ensemble_ave(0)
{
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_ebye_Maker::~St_ebye_Maker(){
 if (m_DataSet) delete m_DataSet;
 m_DataSet = 0;
}
//_____________________________________________________________________________
void St_ebye_Maker::Clear(Option_t *option){
  if (m_DataSet) {delete m_DataSet; m_DataSet = 0;}
}

//_____________________________________________________________________________
void St_ebye_Maker::Finish(){ 
 Clear();
}
//_____________________________________________________________________________
void St_ebye_Maker::Init(){
// Create tables
   St_DataSet *params = gStChain->GetParams();
   St_DataSetIter    local(gStChain->GetParams());
//--  sca_init#make_dir
//--  Check "ebye"
   St_DataSet *ebye = local("ebye");
   SafeDelete(ebye);  

// sca_init#make_tables
//   Char_t *tcl_pars = "${STAR}/params/sca/scaconst.xdf";
//   Char_t *ebye_pars = "${STAR}/params/sca/scaconst.xdf";
   Char_t *ebye_pars = "/afs/rhic/star/packages/SL98f/params/ebye/scaconst.xdf";
   St_XDFFile::GetXdFile(ebye_pars,params);
   ebye = local("ebye");
   if (!ebye) { 
      printf(" Error ** the file \"%s\" has no \"ebye\" dataset\n",ebye_pars);
      return;
   }

   St_DataSetIter scatable(ebye);
   m_sca_switch           = (St_sca_switch *)   scatable("sca_switch");
   m_sca_const            = (St_sca_const *) scatable("sca_const");
   m_sca_filter_const     = (St_sca_filter_const *) scatable("sca_filter_const");

// Set switches to make propir
   sca_switch_st *sca_switch = m_sca_switch->GetTable();
   sca_switch->useDeltaD    = 0;
   sca_switch->useDimension = 0;
   sca_switch->useEntropy   = 0;
   sca_switch->makePrior    = 0;
   sca_switch->doAnalysis   = 0;

// Create Histograms    
   StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::Make(){
  Int_t iret = kSTAFCV_BAD;
//  PrintInfo();


// Create the new tables

   m_sca_in            = new St_sca_in("sca_in",10000);
   m_sca_out           = new St_sca_out("sca_out",10000);
   m_sca_prior         = new St_sca_prior("sca_prior", 300000);
   m_sca_ensemble_ave  = new St_sca_out("sca_ensemble_ave",1);
   m_DataSet->Add(m_sca_in);
   m_DataSet->Add(m_sca_out);
   m_DataSet->Add(m_sca_prior);
   m_DataSet->Add(m_sca_ensemble_ave);

   St_DataSetIter local(gStChain->DataSet());
   m_dst_event_summary = (St_foo_dst_event_summary *) local("output/sumout");
   m_dsttrack = (St_foo_dst_track *) local("output/trkout");
   if (!m_dsttrack || !m_dsttrack->HasData()) {
      return kSTAFCV_BAD;
   }

  iret = sca_filter(m_dst_event_summary 
                   ,m_dsttrack
                   ,m_sca_filter_const
                   ,m_sca_switch
                   ,m_sca_const
                   ,m_sca_in
                   );

  if (iret = kSTAFCV_OK) 
          iret = sca_runsca(m_sca_switch
                           ,m_sca_const
                           ,m_sca_in
                           ,m_sca_ensemble_ave
                           ,m_sca_prior
                           ,m_sca_out
                           );


//Histograms     
 return iret;
}
//_____________________________________________________________________________
void St_ebye_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_ebye_Maker.cxx,v 1.4 1998/09/23 20:22:57 fisyak Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
void St_ebye_Maker::SetDeltaD(Bool_t flag){
   if (!m_sca_switch) return;
// Set switches to make propir
   sca_switch_st *sca_switch = m_sca_switch->GetTable();
   sca_switch->useDeltaD    = flag;
}
//_____________________________________________________________________________
void St_ebye_Maker::SetDimension(Bool_t flag){
   if (!m_sca_switch) return;
// Set switches to make propir
   sca_switch_st *sca_switch = m_sca_switch->GetTable();
   sca_switch->useDimension = flag;
}
//_____________________________________________________________________________
void St_ebye_Maker::SetEntropy(Bool_t flag){
   if (!m_sca_switch) return;
// Set switches to make propir
   sca_switch_st *sca_switch = m_sca_switch->GetTable();
   sca_switch->useEntropy   = flag;
}
//_____________________________________________________________________________
void St_ebye_Maker::SetmakePrior(Bool_t flag){
   if (!m_sca_switch) return;
// Set switches to make propir
   sca_switch_st *sca_switch = m_sca_switch->GetTable();
   sca_switch->makePrior    = flag;
}
//_____________________________________________________________________________
void St_ebye_Maker::SetdoAnalysis(Bool_t flag){
   if (!m_sca_switch) return;
// Set switches to make propir
   sca_switch_st *sca_switch = m_sca_switch->GetTable();
   sca_switch->doAnalysis   = flag;
}

