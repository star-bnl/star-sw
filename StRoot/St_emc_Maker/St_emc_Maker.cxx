// $Id: St_emc_Maker.cxx,v 1.1 1998/12/06 10:23:30 akio Exp $
// $Log: St_emc_Maker.cxx,v $
// Revision 1.1  1998/12/06 10:23:30  akio
// Creation
//
// Revision 1.2  1998/12/06 09:57:12  akio
// put histgrams
//
// Revision 1.1  1998/12/03 22:27:25  akio
// New emc maker
//
// Revision 1.1  1998/11/30 21:18:30  akio
// emc calibration & hit Maker
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_emc_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <math.h>
#include <TH1.h>
#include <TH2.h>
#include "/afs/rhic/star/packages/SL98j/pams/global/inc/math_constants.h"
#include "/afs/rhic/star/packages/SL98j/pams/emc/inc/emc_def.h"
#include "St_emc_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "emc/St_adc_to_energy_Module.h"
ClassImp(St_emc_Maker)

//_____________________________________________________________________________
St_emc_Maker::St_emc_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_emc_Maker::~St_emc_Maker(){
}
//_____________________________________________________________________________
Int_t St_emc_Maker::Init(){
// Create tables
// Create Histograms    
   m_emc_etot  = new TH1F("emc_etot" ,"Total Energy in EMC(log)",100,-1.0,3.0);
   m_emc_energy= new TH2F("emc_energy" ,"EMC Energy",64,-31.0,33.0,120,0.5,120.5);
   m_emc_nhit  = new TH2F("nhit_emc" ,"Number of hit(log) .vs. Detector #",100,0.0,4.5,8,0.5,8.5);
   m_bemc_hit  = new TH2F("bemc_hit" ,"Barrel EMC hits",40,-1.0,1.0,120,0.0,C_2PI);
   m_bprs_hit  = new TH2F("bprs_hit" ,"Barrel PRS hits",40,-1.0,1.0,120,0.0,C_2PI);
   m_bsmde_hit = new TH2F("bsmde_hit","Barrel SMD-Eta hits",300,-1.0,1.0,120,0.0,C_2PI);
   m_bsmdp_hit = new TH2F("bsmdp_hit","Barrel SMD-Phi hits",40,-1.0,1.0,1800,0.0,C_2PI);
   m_eemc_hit  = new TH2F("eemc_hit" ,"Endcap EMC hits",24,-1.0,1.0,60,0.0,C_2PI);
   m_eprs_hit  = new TH2F("eprs_hit" ,"Endcap PRS hits",24,-1.0,1.0,60,0.0,C_2PI);
   m_esmde_hit = new TH2F("esmde_hit","Endcap SMD-u hits",24,-1.0,1.0,60,0.0,C_2PI);
   m_esmdp_hit = new TH2F("esmdp_hit","Endcap SMD-v hits",24,-1.0,1.0,60,0.0,C_2PI);
   return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t St_emc_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
   
    //get calibration data
    St_DataSetIter  local(gStChain->DataSet("calib"));
    m_ped_bemc_h  = (St_emc_calib_header *) local("emc/new/bemc/pedestal/ped_bemc_h");
    m_ped_bemc    = (St_emc_pedestal *)     local("emc/new/bemc/pedestal/ped_bemc");
    m_slp_bemc_h  = (St_emc_calib_header *) local("emc/new/bemc/gain/slp_bemc_h");
    m_slp_bemc    = (St_emc_adcslope *)     local("emc/new/bemc/gain/slp_bemc");

    //For the moment, I copy hit table from emc_raw to emc_hits
    //First get pointers for each table
    St_DataSetIter raw(gStChain->DataSet("emc_raw"));
    St_emc_hits *emc_hits_bemc_raw  = (St_emc_hits *) raw("emc_hits_bemc");
    St_emc_hits *emc_hits_bprs_raw  = (St_emc_hits *) raw("emc_hits_bprs");
    St_emc_hits *emc_hits_bsmde_raw = (St_emc_hits *) raw("emc_hits_bsmde");
    St_emc_hits *emc_hits_bsmdp_raw = (St_emc_hits *) raw("emc_hits_bsmdp");
    St_emc_hits *emc_hits_eemc_raw  = (St_emc_hits *) raw("emc_hits_eemc");
    St_emc_hits *emc_hits_eprs_raw  = (St_emc_hits *) raw("emc_hits_eprs");
    St_emc_hits *emc_hits_esmde_raw = (St_emc_hits *) raw("emc_hits_esmde");
    St_emc_hits *emc_hits_esmdp_raw = (St_emc_hits *) raw("emc_hits_esmdp");    
 
    //Make a copy and add it to the dataset

    St_emc_hits *emc_hits_bemc  = new St_emc_hits("emc_hits_bemc",  4800);m_DataSet->Add(emc_hits_bemc);
    St_emc_hits *emc_hits_bprs  = new St_emc_hits("emc_hits_bprs",  4800);m_DataSet->Add(emc_hits_bprs);
    St_emc_hits *emc_hits_bsmde = new St_emc_hits("emc_hits_bsmde",18000);m_DataSet->Add(emc_hits_bsmde);
    St_emc_hits *emc_hits_bsmdp = new St_emc_hits("emc_hits_bsmdp",18000);m_DataSet->Add(emc_hits_bsmdp);
    St_emc_hits *emc_hits_eemc  = new St_emc_hits("emc_hits_eemc",  1440);m_DataSet->Add(emc_hits_eemc);
    St_emc_hits *emc_hits_eprs  = new St_emc_hits("emc_hits_eprs",  1440);m_DataSet->Add(emc_hits_eprs);
    St_emc_hits *emc_hits_esmde = new St_emc_hits("emc_hits_esmde",10000);m_DataSet->Add(emc_hits_esmde);
    St_emc_hits *emc_hits_esmdp = new St_emc_hits("emc_hits_esmdp",10000);m_DataSet->Add(emc_hits_esmdp);

    emc_hits_bemc_raw->CopySet(*emc_hits_bemc);
    emc_hits_bprs_raw->CopySet(*emc_hits_bprs);
    emc_hits_bsmde_raw->CopySet(*emc_hits_bsmde);
    emc_hits_bsmdp_raw->CopySet(*emc_hits_bsmdp);
    emc_hits_eemc_raw->CopySet(*emc_hits_eemc);
    emc_hits_eprs_raw->CopySet(*emc_hits_eprs);
    emc_hits_esmde_raw->CopySet(*emc_hits_esmde);
    emc_hits_esmdp_raw->CopySet(*emc_hits_esmde);

    cout << "adc_to_energy:";
    Int_t Res_emc = adc_to_energy(m_ped_bemc_h, m_ped_bemc, 
				  m_slp_bemc_h, m_slp_bemc, emc_hits_bemc);
    if (Res_emc != kSTAFCV_OK) {
      cout << endl << "***** Problem with adc_to_energy(BEMC) *****" << endl;
      return kStErr;
    }
    cout << " BEMC" << endl;
  }
  MakeHistograms(); // TPC slow simulator Histograms
  return kStOK;
}
//_____________________________________________________________________________
void St_emc_Maker::MakeHistograms(){
  Int_t   i, n, nhit;
  Float_t e, etot;
  emc_hits_st *hit;
  if (m_DataSet) {
    St_DataSetIter table(m_DataSet);
    St_emc_hits *bemc = (St_emc_hits *)table("emc_hits_bemc");
    St_emc_hits *bprs = (St_emc_hits *)table("emc_hits_bprs");
    St_emc_hits *bsmde= (St_emc_hits *)table("emc_hits_bsmde");
    St_emc_hits *bsmdp= (St_emc_hits *)table("emc_hits_bsmdp");
    St_emc_hits *eemc = (St_emc_hits *)table("emc_hits_eemc");
    St_emc_hits *eprs = (St_emc_hits *)table("emc_hits_eprs");
    St_emc_hits *esmde= (St_emc_hits *)table("emc_hits_esmde");
    St_emc_hits *esmdp= (St_emc_hits *)table("emc_hits_esmdp");

    etot=0.0; nhit=0;
    if(bemc) {
      n=bemc->GetNRows(); hit = bemc->GetTable();
      for(i=0; i<n ; i++){
	e = hit[i].energy; if(e>0.0){nhit++; etot+=e;}
      }
    }
    m_emc_nhit->Fill(log10((float)nhit),(float)BEMC);
    cout << "# of hits in BEMC " << nhit << endl;

    nhit=0;
    if(bprs) {
      n=bprs->GetNRows(); hit = bprs->GetTable();
      for(i=0; i<n ; i++){if(hit[i].energy>0.0){nhit++;}}
    }
    m_emc_nhit->Fill(log10((float)nhit),(float)BPRS);
    cout << "# of hits in BPRS " << nhit << endl;

    nhit=0;
    if(bsmde) {
      n=bsmde->GetNRows(); hit = bsmde->GetTable();
      for(i=0; i<n ; i++){if(hit[i].energy>0.0){nhit++;}}
    }
    m_emc_nhit->Fill(log10((float)nhit),(float)BSMDE);
    cout << "# of hits in BSMDE " << nhit << endl;

    nhit=0;
    if(bsmdp) {
      n=bsmdp->GetNRows(); hit = bsmdp->GetTable();
      for(i=0; i<n ; i++){if(hit[i].energy>0.0){nhit++;}}
    }
    m_emc_nhit->Fill(log10((float)nhit),(float)BSMDP);
    cout << "# of hits in BSMDP " << nhit << endl;

    nhit=0;
    if(eemc) {
      n=eemc->GetNRows(); hit = eemc->GetTable();
      for(i=0; i<n ; i++){
	e = hit[i].energy; if(e>0.0){nhit++; etot+=e;}
      }
    }
    m_emc_nhit->Fill(log10((float)n),(float)EEMC);
    cout << "# of hits in EEMC " << nhit << endl;

    nhit=0;
    if(eprs) {
      n=eprs->GetNRows(); hit = eprs->GetTable();
      for(i=0; i<n ; i++){if(hit[i].energy>0.0){nhit++;}}
    }
    m_emc_nhit->Fill(log10((float)nhit),(float)EPRS);
    cout << "# of hits in EPRS " << nhit << endl;

    nhit=0;
    if(esmde) {
      n=esmde->GetNRows(); hit = esmde->GetTable();
      for(i=0; i<n ; i++){if(hit[i].energy>0.0){nhit++;}}
    }
    m_emc_nhit->Fill(log10((float)nhit),(float)ESMDE);
    cout << "# of hits in ESMDE " << nhit << endl;

    nhit=0;
    if(esmdp) {
      n=esmdp->GetNRows(); hit = esmdp->GetTable();
      for(i=0; i<n ; i++){if(hit[i].energy>0.0){nhit++;}}
    }
    m_emc_nhit->Fill(log10((float)nhit),(float)ESMDP);
    cout << "# of hits in ESMDP " << nhit << endl;

    m_emc_etot->Fill(log10(etot));
    cout << "Total energy in Barrel+Endcap EMC " << etot << " GeV" << endl;
  }
}
//_____________________________________________________________________________
void St_emc_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_emc_Maker.cxx,v 1.1 1998/12/06 10:23:30 akio Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}








