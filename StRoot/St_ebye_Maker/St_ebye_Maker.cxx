// $Id: St_ebye_Maker.cxx,v 1.13 1999/11/09 20:38:28 fisyak Exp $
// $Log: St_ebye_Maker.cxx,v $
// Revision 1.13  1999/11/09 20:38:28  fisyak
// Change tables names
//
// Revision 1.12  1999/09/24 01:23:37  fisyak
// Reduced Include Path
//
// Revision 1.11  1999/07/15 13:57:59  perev
// cleanup
//
// Revision 1.10  1999/03/12 14:39:40  perev
// New maker schema
//
// Revision 1.9  1999/02/17 21:12:47  dhammika
// prevent deleting dst/run_header for multiple event processing
//
//
// Revision 1.8  1999/01/27 00:17:53  dhammika
// EbyE PKG works for more than one event in ROOT
//
// Revision 1.9  1999/01/26 18:18:04  dhammika
// Fixed ebye Maker and macro. Ebye stuff works for more than one event.
//
// Revision 1.8  1999/01/21 19:13:44  dhammika
// Updated ebye stuff which works for one event only
//
// Revision 1.7  1999/01/05 14:11:08  dhammika
// Updated to be in synch with stardev and the latest SCA V2.0 
//
// Revision 1.6  1998/10/31 00:26:13  fisyak
// Makers take care about branches
//
// Revision 1.5  1998/10/06 18:00:35  perev
// cleanup
//
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

#include <iostream.h>
#include "St_ebye_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"


#include "ebye/St_sca_filter_Module.h"
#include "ebye/St_sca_runsca_Module.h"
#include "ebye/St_sca_makeprior_Module.h"
#include "ebye/St_sca_makeref_Module.h"

ClassImp(St_ebye_Maker)

St_ebye_Maker::St_ebye_Maker(const char *name):
StMaker(name),
m_sca_switch(0),
m_sca_const(0),
m_sca_filter_const(0),
m_sca_prior(0),
m_sca_ensemble_ave(0),
this_run_header(0),
this_event_header(0),
this_dst_track(0),
this_sca_in(0),
this_sca_out(0)
{
}
//_____________________________________________________________________________
St_ebye_Maker::~St_ebye_Maker()
{
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::Init(){
  // Get the run header
  run_header_st *run_header = 0;
  St_DataSet *runhead = GetDataSet("run_summary");
  St_DataSetIter dstrun(runhead);
  if (GetDebug()) runhead->ls("*");
  this_run_header = (St_run_header *) dstrun("run_header");
  if (this_run_header) {
    run_header = this_run_header->GetTable();
    if (run_header)
      cout << " ===> <St_ebye_Maker::Init()>: DST event type = " << run_header->event_type<< endl;
    else
      cout << " ===> <St_ebye_Maker::Init()>: Null pointer to run header table" << endl;
  }
  else {
    cout << " ===> <St_ebye_Maker::Init()>: No DST run header table; create one" << endl;
    this_run_header  = new St_run_header("run_header",1);
    dstrun.Add(this_run_header,"dst");
    run_header = this_run_header->GetTable();
    run_header->exp_run_id =       1;
    this_run_header->AddAt(&run_header,0);
    if(GetDebug())this_run_header->ls("*");
    cout << " ===> <St_ebye_Maker::Init()>: Created run header table" << endl;
  }
  // Create tables
  St_DataSet *params = GetDataBase("params/ebye");
  assert(params);

  if (GetDebug()>2) {
    printf(" ===> <St_ebye_Maker::Init()>: *params = %p\n",params);
  } 
  St_DataSetIter     local(params);

  // I don't know how to prevent chain->Clear deleting dst/run_header
  // at the end of the first event. So temporarily store run_header table 
  // in params/ebye.
  //St_DataSet  *my_run_dst = local.Mkdir("ebye/run");
  //this_run_header  = new St_run_header("run_header",1);
  //local.Add(this_run_header,"params/ebye/run");
  //run_header = this_run_header->GetTable();
  //run_header->exp_run_id =       1;
  //this_run_header->AddAt(&run_header,0);
  //if(GetDebug()){
  //  this_run_header->ls("*");
  //  local.Du();
  //}
  
  // Fixed the above problem with Yuri's help.  
  // Have to load   global.sl, St_global.so and St_run_summary_Maker.so 
  // in ebye.C macro.
  // Feb 4, 1999  Dhammika W.

  St_DataSet *sca = local("sca");
  assert(sca);

  if (GetDebug()>2)printf(" ===> <St_ebye_Maker::Init()>: Begin Iterating sca \n");
  St_DataSetIter scatable(sca);
  if (GetDebug()>2)printf(" ===> <St_ebye_Maker::Init()>: Done Iterating sca \n");

  m_sca_switch           = (St_sca_switch *)       scatable("sca_switch");
  m_sca_const            = (St_sca_const *)        scatable("sca_const");
  m_sca_filter_const     = (St_sca_filter_const *) scatable("sca_filter_const");
  if(GetDebug()>1)m_sca_const->ls("*");
  
  if (GetDebug()>2) printf (" ===> <St_ebye_Maker::Init()>: \n \t m_sca_switch       = %p, \n \t m_sca_const        = %p, \n \t m_sca_filter_const = %p \n", 
		     m_sca_switch,m_sca_const,m_sca_filter_const);
  // Set switches to make propir
  sca_switch_st *sca_switch   = m_sca_switch->GetTable();
  sca_switch->makePrior       = 0;
  sca_switch->makeEnsembleAve = 0;
  sca_switch->doAnalysis      = 0;
  
  // Create Histograms    

  Int_t iret = StMaker::Init();
  if (GetDebug()>2) printf (" ===> <St_ebye_Maker::Init()>: StMaker::Init() returned iret = %d \n",iret);
  return iret;
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::Make(){
  Int_t iret = kStWarn;
  //  PrintInfo();

  if (GetDebug()>2)cout << " ===> <St_ebye_Maker::Make()>: Begin ebye Make" << endl;
  // Create the new tables
  
  this_sca_in            = new St_sca_in("sca_in",10000);
  this_sca_out           = new St_sca_out("sca_out",10000);
  m_DataSet->Add(this_sca_in);
  m_DataSet->Add(this_sca_out);
  
  St_DataSet *dst_set = GetDataSet("dst");     
  if (!dst_set) {
    cout << " ===> <St_ebye_Maker::Make()>: <<< ERROR >>> No DST dataset " << endl;
    return kStWarn;
  }
  else
    if(GetDebug()>1)dst_set->ls("*");
  St_DataSetIter       dsttables(dst_set);
  this_event_header  = (St_event_header *)  dsttables("event_header");
  //this_dst_track         = (St_dst_track *)         dsttables("globtrk");
  this_dst_track         = (St_dst_track *)dsttables.FindObject("globtrk");  
  if (!this_dst_track ){
    cout << " ===> <St_ebye_Maker::Make()>: <<< ERROR >>> NULL pointer this_dst_track" << endl;
    dsttables.Du();  // This line is a new one
    return kStWarn;
  }
  else
    if (GetDebug())dsttables.Du();  // This line is a new one

  if(GetDebug()>1)this_dst_track->ls("*");
  iret = this_dst_track->HasData();
  if (!iret) {
    cout << " ===> <St_ebye_Maker::Make()>: <<< ERROR >>> No DST tracks" << endl;
    return kStWarn;
  }
  if (GetDebug()>2) printf(" ===> <St_ebye_Maker::Make()>: Begin sca_filter \n");
  iret = sca_filter(this_run_header
		    ,this_event_header
		    ,this_dst_track
		    ,m_sca_filter_const
		    ,m_sca_switch
		    ,m_sca_const
		    ,this_sca_in
		    );
  
  if (iret !=  kSTAFCV_OK){
    cout << " ===> <St_ebye_Maker::Make()>: <<< ERROR >>> sca_filter  failed" << endl;
    return kStWarn;
  }
  
  if (GetDebug()>2) printf(" ===> <St_ebye_Maker::Make()>: Begin sca_runsca \n");
  iret = sca_runsca(m_sca_switch
		    ,m_sca_const
		    ,this_sca_in
		    ,m_sca_ensemble_ave
		    ,m_sca_prior
		    ,this_sca_out
		    );
  
  if (iret !=  kSTAFCV_OK){
    cout << " ===> <St_ebye_Maker::Make()>: <<< ERROR >>> sca_runsca  failed " << endl;
    return kStWarn;
  }
  if (GetDebug()>2)cout << " ===> <St_ebye_Maker::Make()>: End ebye Make" << endl;
  //Histograms     
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::SetmakePrior(Bool_t flag){
  if (!m_sca_switch) return kStWarn;
  // Set switches to make propir
  sca_switch_st *sca_switch   = m_sca_switch->GetTable();
  sca_switch->makePrior       = flag;
  // Allocate dynamic memory for prior
  m_sca_prior         = new St_sca_prior("sca_prior", 300000);
  m_DataSet->Add(m_sca_prior);

  return kStOK;
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::SetmakeEnsembleAve(Bool_t flag){
  if (!m_sca_switch) return kStWarn;
  St_DataSet  *calib = GetDataSet("calib");
  if (!calib) {
    cout << " ===> <St_ebye_Maker::Init()>: No calib  Dataset; create calib  " << endl;
    calib  = new St_DataSet("calib");
  }
  if (GetDebug()>2) {
    printf(" ===> <St_ebye_Maker::Init()>: *calib  = %p\n",calib);
  }
  St_DataSetIter      local(calib);
  St_DataSet *ebye  = local("ebye");
  //SafeDelete(ebye);  
  if (! ebye) ebye = local.Mkdir("ebye");
  //Char_t *sca_prior = "${STAR_CALIB}/ebye/sca_prior_dir.xdf";
  Char_t *sca_prior = "/star/u2/dhammika/newupdate/calib/ebye/sca_prior_dir.xdf";
  St_XDFFile::GetXdFile(sca_prior,ebye);
  St_DataSet *scaprior = local("ebye/sca_prior_dir");
  if (!scaprior) { 
    printf(" ===> <St_ebye_Maker::SetmakeEnsembleAve()>: <<< ERROR >>>  the file \"%s\" has no \"sca_prior_dir\" dataset\n",sca_prior);
    return kStWarn;
  }
  
  St_DataSetIter scatable(scaprior);
  m_sca_prior           = (St_sca_prior *)       scatable("sca_prior");
  if (!m_sca_prior)
    cout << " ===> <St_ebye_Maker::SetdoAnalysis()>: <<< ERROR >>> No sca_prior table " << endl;
  else
    if(GetDebug()>1)m_sca_prior->ls("*"); 
  
  // Set switches to make propir
  sca_switch_st *sca_switch   = m_sca_switch->GetTable();
  sca_switch->makeEnsembleAve = flag;

  // Allocate dynamic memory for ensemble average
  m_sca_ensemble_ave  = new St_sca_out("sca_ensemble_ave",1);
  m_DataSet->Add(m_sca_ensemble_ave);

  return kStOK;
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::SetdoAnalysis(Bool_t flag){

  if (GetDebug()>2) printf (" ===> <St_ebye_Maker::SetdoAnalysis()>: Begin\n");
  if (!m_sca_switch) return  kStWarn;
  St_DataSet *calib = GetDataSet("calib");
  if (!calib) {
    cout << " ===> <St_ebye_Maker::SetdoAnalysis()>: No calib  Dataset; create calib  " << endl;
    calib  = new St_DataSet("calib");
  }
  if (GetDebug()>2) {
    printf(" ===> <St_ebye_Maker::SetdoAnalysis()>: *calib  = %p\n",calib);
  }
  St_DataSetIter      local(calib);
  St_DataSet *ebye  = local("ebye");
  //SafeDelete(ebye);  
  if (! ebye) {
    if (GetDebug()>2) printf(" ===> <St_ebye_Maker::SetdoAnalysis()>: calib/ebye doesn't exist. Create it\n");
    ebye = local.Mkdir("ebye");
  }
  //Char_t *sca_prior = "${STAR_CALIB}/ebye/sca_prior_dir.xdf";
  Char_t *sca_prior = "/star/u2/dhammika/newupdate/calib/ebye/sca_prior_dir.xdf";
  St_XDFFile::GetXdFile(sca_prior,ebye);
  //Char_t *sca_ensmave = "${STAR_CALIB}/ebye/sca_ensemble_dir.xdf";
  Char_t *sca_ensmave = "/star/u2/dhammika/newupdate/calib/ebye/sca_ensemble_dir.xdf";
  St_XDFFile::GetXdFile(sca_ensmave,ebye);
  St_DataSet *scaprior = local("ebye/sca_prior_dir");
  if (!scaprior) { 
    printf(" ===> <St_ebye_Maker::SetdoAnalysis()>: <<< ERROR >>> the file \"%s\" has no \"sca_prior_dir\" dataset\n",sca_prior);
    return kStWarn;
  }
  St_DataSetIter priortable(scaprior);
  m_sca_prior           = (St_sca_prior *)    priortable("sca_prior");
  if (!m_sca_prior)
    cout << " ===> <St_ebye_Maker::SetdoAnalysis()>: <<< ERROR >>> No sca_prior table " << endl;
  else
    if(GetDebug()>1)m_sca_prior->ls("*"); 
  St_DataSet *scaref = local("ebye/sca_ensemble_dir");
  if (!scaref) { 
    printf(" ===> <St_ebye_Maker::SetdoAnalysis()>: <<< ERROR >>> the file \"%s\" has no \"sca_ensemble_dir\" dataset\n",sca_ensmave);
    return kStWarn;
  }
  St_DataSetIter ensavetable(scaref);
  m_sca_ensemble_ave    = (St_sca_out *)     ensavetable("sca_ensemble_ave");
  if (!m_sca_ensemble_ave)
    cout << " ===> <St_ebye_Maker::SetdoAnalysis()>: <<< ERROR >>> No sca_ensemble_ave table " << endl;
  else
    if(GetDebug()>1)m_sca_ensemble_ave->ls("*"); 

  // Set switches to make propir
  sca_switch_st *sca_switch   = m_sca_switch->GetTable();
  sca_switch->doAnalysis      = flag;
  if (GetDebug()>2) printf (" ===> <St_ebye_Maker::SetdoAnalysis()>: End\n");
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::SetnEvents(Int_t nEvents){
  if (!m_sca_switch) return kStWarn;
  // Set switches to make propir
  sca_switch_st *sca_switch   = m_sca_switch->GetTable();
  sca_switch->nEvents         = nEvents;
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::PutPrior(){
  Int_t iret = kStWarn;

  iret = sca_makeprior(m_sca_prior);
  if (iret !=  kSTAFCV_OK){
    cout << " ===> <St_ebye_Maker::PutPrior()>: <<< ERROR >>> sca_makeprior  failed" << endl;
    return kStWarn;
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_ebye_Maker::PutEnsembleAve(){
  Int_t iret = kStWarn;

  iret = sca_makeref(m_sca_switch
		     ,m_sca_ensemble_ave);
  if (iret !=  kSTAFCV_OK){
    cout << " ===> <St_ebye_Maker::PutEnsembleAve()>: <<< ERROR >>> sca_makeref failed" << endl;
    return kStWarn;
  }
  return kStOK;
}
//_____________________________________________________________________________
