// $Id: St_emc_Maker.cxx,v 1.1 1998/12/03 22:27:25 akio Exp $
// $Log: St_emc_Maker.cxx,v $
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
#include <TH1.h>
#include <TH2.h>
#include "/afs/rhic/star/packages/SL98j/pams/global/inc/math_constants.h"
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
   m_emc_etot  = new TH1F("emc_etot" ,"Total Energy in EMC",100,0.0,1000.0);
   m_emc_nhit  = new TH2F("emc_nhit" ,"Number of hit .vs. Detector #",100,0.0,10000.0,8,1.0,9.0);
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
  cout << "**** Starting St_emc_Maker::Make" << endl;
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
   
    //get calibration data
    St_DataSetIter  local(gStChain->DataSet("calib"));
    m_ped_bemc_h  = (St_emc_calib_header *) local("emc/new/bemc/pedestal/ped_bemc_h");
    m_ped_bemc    = (St_emc_pedestal *)     local("emc/new/bemc/pedestal/ped_bemc");
    m_slp_bemc_h  = (St_emc_calib_header *) local("emc/new/bemc/gain/slp_bemc_h");
    m_slp_bemc    = (St_emc_adcslope *)     local("emc/new/bemc/gain/slp_bemc");

    //For the moment, I copy hit table from emc_raw to emc_hits
    //First get pointers for each table
    cout << "**** getting tables from emc_raw" << endl;
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

    cout << "**** maging new tables and add it in emc_hit" << endl;
    St_emc_hits *emc_hits_bemc  = new St_emc_hits("emc_hits_bemc",  4800);m_DataSet->Add(emc_hits_bemc);
    St_emc_hits *emc_hits_bprs  = new St_emc_hits("emc_hits_bprs",  4800);m_DataSet->Add(emc_hits_bprs);
    St_emc_hits *emc_hits_bsmde = new St_emc_hits("emc_hits_bsmde",18000);m_DataSet->Add(emc_hits_bsmde);
    St_emc_hits *emc_hits_bsmdp = new St_emc_hits("emc_hits_bsmdp",18000);m_DataSet->Add(emc_hits_bsmdp);
    St_emc_hits *emc_hits_eemc  = new St_emc_hits("emc_hits_eemc",  1440);m_DataSet->Add(emc_hits_eemc);
    St_emc_hits *emc_hits_eprs  = new St_emc_hits("emc_hits_eprs",  1440);m_DataSet->Add(emc_hits_eprs);
    St_emc_hits *emc_hits_esmde = new St_emc_hits("emc_hits_esmde",10000);m_DataSet->Add(emc_hits_esmde);
    St_emc_hits *emc_hits_esmdp = new St_emc_hits("emc_hits_esmdp",10000);m_DataSet->Add(emc_hits_esmdp);

    cout << "**** copying table contents from emc_raw" << endl;
    emc_hits_bemc_raw->CopySet(*emc_hits_bemc);
    emc_hits_bprs_raw->CopySet(*emc_hits_bprs);
    emc_hits_bsmde_raw->CopySet(*emc_hits_bsmde);
    emc_hits_bsmdp_raw->CopySet(*emc_hits_bsmde);
    emc_hits_eemc_raw->CopySet(*emc_hits_eemc);
    emc_hits_eprs_raw->CopySet(*emc_hits_eprs);
    emc_hits_esmde_raw->CopySet(*emc_hits_esmde);
    emc_hits_esmdp_raw->CopySet(*emc_hits_esmde);

    //cout << "**** copying tables from emc_raw" << endl;
    //*emc_hits_bemc  = *emc_hits_bemc_raw;  
    //*emc_hits_bprs  = *emc_hits_bprs_raw;  
    //*emc_hits_bsmde = *emc_hits_bsmde_raw; 
    //*emc_hits_bsmdp = *emc_hits_bsmdp_raw; 
    //*emc_hits_eemc  = *emc_hits_eemc_raw;  
    //*emc_hits_eprs  = *emc_hits_eprs_raw;  
    //*emc_hits_esmde = *emc_hits_esmde_raw; 
    //*emc_hits_esmdp = *emc_hits_esmdp_raw; 
    
    //cout << "**** copying tables from emc_raw to here and add it to dataset" << endl;
    //St_emc_hits *emc_hits_bemc  = new St_emc_hits(*emc_hits_bemc_raw) ;m_DataSet->Add(emc_hits_bemc); 
    //St_emc_hits *emc_hits_bprs  = new St_emc_hits(*emc_hits_bprs_raw) ;m_DataSet->Add(emc_hits_bprs); 
    //St_emc_hits *emc_hits_bsmde = new St_emc_hits(*emc_hits_bsmde_raw);m_DataSet->Add(emc_hits_bsmde);
    //St_emc_hits *emc_hits_bsmdp = new St_emc_hits(*emc_hits_bsmdp_raw);m_DataSet->Add(emc_hits_bsmdp);
    //St_emc_hits *emc_hits_eemc  = new St_emc_hits(*emc_hits_eemc_raw) ;m_DataSet->Add(emc_hits_eemc); 
    //St_emc_hits *emc_hits_eprs  = new St_emc_hits(*emc_hits_eprs_raw) ;m_DataSet->Add(emc_hits_eprs); 
    //St_emc_hits *emc_hits_esmde = new St_emc_hits(*emc_hits_esmde_raw);m_DataSet->Add(emc_hits_esmde);
    //St_emc_hits *emc_hits_esmdp = new St_emc_hits(*emc_hits_esmdp_raw);m_DataSet->Add(emc_hits_esmdp);

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
  Float_t etot=0.0;
  if (m_DataSet) {
    St_DataSetIter next(m_DataSet);
  }  
}
//_____________________________________________________________________________
void St_emc_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_emc_Maker.cxx,v 1.1 1998/12/03 22:27:25 akio Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

