// $Id: St_ems_Maker.cxx,v 1.18 2000/06/23 19:57:06 fisyak Exp $
// $Log: St_ems_Maker.cxx,v $
// Revision 1.18  2000/06/23 19:57:06  fisyak
// remove access to params
//
// Revision 1.17  2000/03/30 18:36:02  akio
// delete stlib stuff
//
// Revision 1.16  2000/03/29 20:25:33  akio
// Add StEvent
//
// Revision 1.15  1999/11/13 02:32:57  fisyak
// Take calb_calg directly from geant w/o Cint macros
//
// Revision 1.14  1999/07/19 13:23:03  fisyak
// New Maker scheme
//
// Revision 1.13  1999/07/15 13:58:03  perev
// cleanup
//
// Revision 1.12  1999/07/01 15:59:12  pavlinov
// added QA histograms and new switches for control of maker
//
// Revision 1.11  1999/03/20 22:36:35  perev
// maker new schema
//
// Revision 1.10  1999/03/04 01:17:24  akio
// change calib name
//
// Revision 1.9  1999/03/03 17:34:15  akio
// small corrections
//
// Revision 1.8  1999/03/03 04:12:14  fisyak
// replace kStErr to kStWarn
//
// Revision 1.7  1999/02/17 15:54:18  fisyak
// Adjust geometry parameters from GEANT
//
// Revision 1.6  1999/02/16 18:15:44  fisyak
// Check in the latest updates to fix them
//
// Revision 1.5  1998/12/15 22:38:45  akio
// Add some comments
//
// Revision 1.4  1998/12/06 10:25:45  akio
// re-commit
//
// Revision 1.3  1998/12/06 09:57:13  akio
// put histograms
//
// Revision 1.2  1998/12/03 22:30:09  akio
// Include dep_e_toadc and emc_adc_sim
//
// Revision 1.1  1998/11/30 21:18:30  fisyak
// ems raw data Maker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ems_Maker is class for begin_html <FONT COLOR="RED">EMC Simulation</FONT> end_html dataset//
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <assert.h>
#include <math.h>
#include "St_ems_Maker.h"
#include "St_DataSetIter.h"
#include "emc/St_ems_interface2_Module.h"
#include "emc/St_dep_e_toadc_Module.h"
#include "emc/St_emc_adc_sim_Module.h"
#include "emc/St_dep_e_toadc_Module.h"

#include "tables/St_ems_hits_Table.h"

#include "St_ObjectSet.h"
#include "StEmcCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"
#include "StEmcClusterCollection.h"
#include "StEmcCluster.h"
#include "StEmcPoint.h"
#include "StEnumerations.h"

ClassImp(St_ems_Maker)

const TString detname[] = {
  "bemc", "bprs", "bsmde", "bsmdp",
  "eemc", "eprs", "esmde", "esmdp"
};

//_____________________________________________________________________________
St_ems_Maker::St_ems_Maker(const char *name):StMaker(name){
// Use Constructor from StMaker
   drawinit     = kFALSE;
   mBEMC        = 1;  // BEMC on
   mEEMC        = 1;  // EEMC on
   mHistControl = 0;  // Hist off
}
//_____________________________________________________________________________
St_ems_Maker::~St_ems_Maker() { }
//_____________________________________________________________________________
Int_t St_ems_Maker::Init(){

  St_DataSet *emcPar = GetInputDB("emc");
  assert(emcPar);   
  St_DataSetIter       local(emcPar);
  m_ems_control     = (St_ems_control *)      local("ems/ems_control");
  m_control_toadc   = (St_control_toadc *)    local("ems/control_toadc");
  m_ems_cal_control = (St_ems_cal_control *)  local("cal/ems_cal_control");
  m_org_ped_bemc_h  = (St_emc_calib_header *) local("cal/org_ped_bemc_h");
  m_org_ped_bemc    = (St_emc_pedestal *)     local("cal/org_ped_bemc");
  m_org_slp_bemc_h  = (St_emc_calib_header *) local("cal/org_slp_bemc_h");
  m_org_slp_bemc    = (St_emc_adcslope *)     local("cal/org_slp_bemc");

  St_DataSet *geo = GetDataSet("geom");
  assert(geo);
  m_calb_calg   = (St_calb_calg   *) geo->Find("calb_calg"); 
  assert(m_calb_calg);

  if(mHistControl){
    if(mBEMC) { 
      for(Int_t i=0; i<4; i++) {
        if(!mGeom[i]) {mGeom[i] = new StEmcGeom(i+1); mGeom[i]->printGeom();}
        bookHistograms(i);
      }
    }
    if(mEEMC) { for(Int_t i=4; i<8; i++) bookHistograms(i);}
  }

  printmBEMC(); printmEEMC();  printNameOfTables();

  return StMaker::Init();
}
//_____________________________________________________________________________
void St_ems_Maker::bookHistograms(const Int_t i){

  const char *title_h[] = {
    "Barrel EMC hits","Barrel PRS hits","Barrel SMD-Eta hits","Barrel SMD-Phi hits",
    "Endcap EMC hits","Endcap PRS hits","Endcap SMD-u hits",  "Endcap SMD-v hits"};
  const char *title_e[] = {
    "Barrel EMC energy","Barrel PRS energy","Barrel SMD-Eta energy","Barrel SMD-Phi energy",
    "Endcap EMC energy","Endcap PRS energy","Endcap SMD-u energy",  "Endcap SMD-v energy"};
  const char *title_adc[] = {
    "Barrel EMC adc dist.","Barrel PRS adc dist.","Barrel SMD-Eta adc dist.","Barrel SMD-Phi adc dist.",
    "Endcap EMC adc dist.","Endcap PRS adc dist.","Endcap SMD-u adc dist.",  "Endcap SMD-v adc dist."};
  const Int_t   nx[] = {40,40,300,20,12,12,12,12};
  const Float_t xl[] = {-1.0,-1.0,-1.0,-1.0, 0.5 , 0.5, 0.5, 0.5};
  const Float_t xu[] = { 1.0, 1.0, 1.0, 1.0, 12.5,12.5,12.5,12.5};
  const Int_t   ny[] = {120, 120, 60, 900, 60, 60, 60, 60};
  if(!m_nhit){
    m_nhit = new TH2F("EmcNHitsVsDet(ems)" ,"Number of hit(log) .vs. Detector #",100,0.0,4.5,8,0.5,8.5);
    m_etot = new TH2F("EmcEtotVsDet(ems)" ,"Total energy(log) .vs. Detector #",100,-4.0,4.5,8,0.5,8.5);
  }

  TString name_h   = detname[i] + "Hits(ems)";
  TString name_e   = detname[i] + "Energy(ems)";
  TString name_adc = detname[i] + "ADC(ems)";
  Float_t rpi = M_PI + 0.00001; 
  m_hits[i]   = new TH2F(name_h,title_h[i], nx[i],xl[i],xu[i], ny[i],-rpi, rpi);
  m_energy[i] = new TH2F(name_e,title_e[i], nx[i],xl[i],xu[i], ny[i],-rpi, rpi);
  m_adc[i]    = new TH1F(name_adc,title_adc[i], 5001, -0.5, 5000.5);
}
//_____________________________________________________________________________
void St_ems_Maker::makeHistograms(const Int_t det, St_emc_hits *emc_hit){

  Float_t energysum=0.0, etsum=0.0; 
  Float_t E, eta, phi; 
  Int_t nhit=0, id,m,e,s, adc;

  Int_t n = emc_hit->GetNRows();
  //  printf(" =============== Det %i n %i  =============== \n",det,n);
  if(n>0){
    emc_hits_st *hit = emc_hit->GetTable();
     
    for(Int_t i = 0; i<n; i++){
       m   = (Int_t)hit[i].module; 
       e   = (Int_t)hit[i].eta;
       s   = (Int_t)hit[i].sub;
       adc = (Int_t)hit[i].adc;  // For testing only
       E   =        hit[i].energy;
       //       printf(" i %i module %i  etaBin %i Sub %i ADC %i Energy %f \n", i, m, e, s, adc, E);

       Int_t check=mGeom[det-1]->getId(m, e, s, id);   // Check bound of index
       if(check == 0){
         Int_t ieta=mGeom[det-1]->getEta(m, e, eta); 
         Int_t iphi=mGeom[det-1]->getPhi(m, s, phi);
         if(E>0.0 && ieta==0 && iphi==0){
           m_hits[det-1]->Fill(eta,phi); 
           m_energy[det-1]->Fill(eta,phi,E); 
           m_adc[det-1]->Fill((float)adc); 
           nhit      += 1; 
           energysum += E; 
           etsum     += E/cosh(eta);  // Et = E*sin(theta); sin(theta) = 1./cos(eta)
         }
       }
       else printf(" <E> St_ems_Maker::makeHistograms => bad index det %i m %i e %i s %i id %i \n"
                  , det, m, e, s, id);
    }
    m_nhit->Fill(log10((Double_t)nhit), (Float_t)det);
    m_etot->Fill(log10((Double_t)energysum), (Float_t)det);
  }
}
//_____________________________________________________________________________
Int_t St_ems_Maker::Make(){

  //  Find  Geant  Tables
  St_DataSet *gea = GetDataSet("geant");
  if (!gea) return kStWarn;
  St_DataSetIter geant(gea);
  St_g2t_event  *g2t_event  = (St_g2t_event  *) geant("g2t_event");
  St_g2t_vertex *g2t_vertex = (St_g2t_vertex *) geant("g2t_vertex");
  St_g2t_track  *g2t_track  = (St_g2t_track  *) geant("g2t_track");
  St_g2t_emc_hit *g2t_emc_hit = (St_g2t_emc_hit *) geant("g2t_emc_hit");
  St_g2t_emc_hit *g2t_smd_hit = (St_g2t_emc_hit *) geant("g2t_smd_hit");
  St_g2t_emc_hit *g2t_eem_hit = (St_g2t_emc_hit *) geant("g2t_eem_hit");
  St_g2t_emc_hit *g2t_esm_hit = (St_g2t_emc_hit *) geant("g2t_esm_hit");

  //		If there is no g2t_hit table, create dummy one 
  TString tmpgarb = fName+"/.garb";
  St_DataSet *garb = GetDataSet(tmpgarb); // Garbage place for ems maker
  if (!garb) return kStWarn;
  if (!g2t_emc_hit) {g2t_emc_hit = new St_g2t_emc_hit("g2t_emc_hit",1); garb->Add(g2t_emc_hit);}
  if (!g2t_smd_hit) {g2t_smd_hit = new St_g2t_emc_hit("g2t_smd_hit",1); garb->Add(g2t_smd_hit);} 
  if (!g2t_eem_hit) {g2t_eem_hit = new St_g2t_emc_hit("g2t_eem_hit",1); garb->Add(g2t_eem_hit);} 
  if (!g2t_esm_hit) {g2t_esm_hit = new St_g2t_emc_hit("g2t_esm_hit",1); garb->Add(g2t_esm_hit);}

  // Define Output Tables for ems Maker
  TString tmpdata = fName+"/.data";
  St_DataSet *dataEms = GetDataSet(tmpdata);
  if (!dataEms) return kStWarn;

  St_ems_hits *ems_hits_bemc = new St_ems_hits("ems_hits_bemc", 9600);dataEms->Add(ems_hits_bemc);
  St_ems_hits *ems_hits_bsmd = new St_ems_hits("ems_hits_bsmd",36000);dataEms->Add(ems_hits_bsmd);
  St_ems_hits *ems_hits_eemc = new St_ems_hits("ems_hits_eemc",10000);dataEms->Add(ems_hits_eemc);
  St_ems_hits *ems_hits_esmd = new St_ems_hits("ems_hits_esmd",20000);dataEms->Add(ems_hits_esmd);

//	Calling emc_interface2 which get hit from g2t_hit, store in ems_hit
  Int_t Res_ems =  ems_interface2 (g2t_event, g2t_vertex, g2t_track,
				   g2t_emc_hit, g2t_smd_hit, g2t_eem_hit, g2t_esm_hit,
				   m_calb_calg, m_ems_control,
				   ems_hits_bemc, ems_hits_bsmd, ems_hits_eemc, ems_hits_esmd);

  if (Res_ems != kSTAFCV_OK) {
    cout << "***** Problem with ems_interface2 *****" << endl; return kStWarn;
  }

  //    Calling dep_e_toadc which convert geant deposit energy to ADC
  if(mBEMC){
    St_emc_hits *emc_hits_bemc  = new St_emc_hits("emc_hits_bemc",  4800);dataEms->Add(emc_hits_bemc);
    St_emc_hits *emc_hits_bprs  = new St_emc_hits("emc_hits_bprs",  4800);dataEms->Add(emc_hits_bprs);
    St_emc_hits *emc_hits_bsmde = new St_emc_hits("emc_hits_bsmde",18000);dataEms->Add(emc_hits_bsmde);
    St_emc_hits *emc_hits_bsmdp = new St_emc_hits("emc_hits_bsmdp",18000);dataEms->Add(emc_hits_bsmdp);
    
    Res_ems = dep_e_toadc(m_ems_control,m_control_toadc,ems_hits_bemc,emc_hits_bemc,emc_hits_bprs);
    if (Res_ems != kSTAFCV_OK) {
      cout << endl << "***** Problem with dep_e_toadc (BEMC/BPRS) *****" << endl; return kStWarn;
    }
    
    Res_ems = dep_e_toadc(m_ems_control,m_control_toadc,ems_hits_bsmd,emc_hits_bsmde,emc_hits_bsmdp);
    if (Res_ems != kSTAFCV_OK) {
      cout << endl << "***** Problem with dep_e_toadc (BSMD) *****" << endl; return kStWarn;
    } 
    
    if(mBEMC > 1){
  //    Calling emc_adc_sim which gives pedestal and slope variations.
  //    Only for BEMC now 
      Res_ems = emc_adc_sim(m_ems_control, m_ems_cal_control,
                            m_org_ped_bemc_h, m_org_ped_bemc, 
	  	          m_org_slp_bemc_h, m_org_slp_bemc, emc_hits_bemc);
      if (Res_ems != kSTAFCV_OK) {
        cout << endl << "***** Problem with emc_adc_sim (BEMC) *****" << endl;  return kStWarn;      
      }
    }
    if(mHistControl) {
      makeHistograms(1, emc_hits_bemc);
      makeHistograms(2, emc_hits_bprs);
      makeHistograms(3, emc_hits_bsmde);
      makeHistograms(4, emc_hits_bsmdp);
    }
  
  }
 
  if(mEEMC){
    St_emc_hits *emc_hits_eemc  = new St_emc_hits("emc_hits_eemc",  1440);dataEms->Add(emc_hits_eemc);
    St_emc_hits *emc_hits_eprs  = new St_emc_hits("emc_hits_eprs",  1440);dataEms->Add(emc_hits_eprs);
    St_emc_hits *emc_hits_esmde = new St_emc_hits("emc_hits_esmde",10000);dataEms->Add(emc_hits_esmde);
    St_emc_hits *emc_hits_esmdp = new St_emc_hits("emc_hits_esmdp",10000);dataEms->Add(emc_hits_esmdp);
    Res_ems = dep_e_toadc(m_ems_control,m_control_toadc,ems_hits_eemc,emc_hits_eemc,emc_hits_eprs);
    if (Res_ems != kSTAFCV_OK) {
      cout << endl << "***** Problem with dep_e_toadc (EEMC/EPRS) *****" << endl; return kStWarn;
    }
    Res_ems = dep_e_toadc(m_ems_control,m_control_toadc,ems_hits_esmd,emc_hits_esmde,emc_hits_esmdp);
    if (Res_ems != kSTAFCV_OK) {
      cout << endl << "***** Problem with dep_e_toadc (ESMD) *****" << endl;  return kStWarn;
    }
  }

  fillStEvent();

  return kStOK;
}
//_____________________________________________________________________________
Int_t St_ems_Maker::fillStEvent(){
  unsigned int  i, j, k;
  

  //Create StEmcHitCollection  
  StEmcCollection* emc = new StEmcCollection();

  //Add it to dataset as ObjectSet 
  AddData(new St_ObjectSet("EmcCollection" , emc));

  //Loop over detectors
  int nModule[8] = {120, 120, 120, 120, 24, 24, 24, 24}; // temp.->database
  St_DataSet *emcRaw = GetDataSet("emc_raw/.data"); 
  if(!emcRaw) return kStWarn;
  St_DataSetIter itr(emcRaw);
  for(i=0; i<8; i++){
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    TString name = "emc_hits_" + detname[i];
    St_emc_hits  *table = (St_emc_hits *) itr(name);
    cout << i << " " << name << " " << table << endl;
    if(table){
      //Create StEmcDetector
      StEmcDetector* detector = new StEmcDetector(id, nModule[i]);
      emc->setDetector(detector);
      //Get table
      emc_hits_st *t = table->GetTable();
      //Create StEmcHit
      for(j=0; j<table->GetNRows(); j++){
	StEmcRawHit* hit = new StEmcRawHit(id, 
			  	           t[j].module, t[j].eta, t[j].sub,
				           t[j].adc, t[j].energy);
	detector->addHit(hit);
      }	
    }
  } 
  return kStOK;
}
//_____________________________________________________________________________
void St_ems_Maker::printNameOfTables(){
  Char_t *path="/bfc/.make/.data/params/emc/";
  printf("**** Name of control table for ST_ems_Maker ****\n");
  printf("** Type=St_ems_control       Name=%sems/%s \n", 
	 (const char*) m_ems_control->Path(),    m_ems_control->GetName());     
  printf("** Type=St_control_toadc     Name=%sems/%s \n", 
	 (const char*) m_control_toadc->Path(),  m_control_toadc->GetName());   
  printf("** Type=St_ems_cal_control   Name=%scal/%s \n", 
	 (const char*) m_ems_cal_control->Path(),m_ems_cal_control->GetName()); 
  printf("** Type=St_emc_calib_header  Name=%scal/%s \n", 
	 (const char*) m_org_ped_bemc_h->Path(), m_org_ped_bemc_h->GetName());  
  printf("** Type=St_emc_pedestal      Name=%scal/%s \n", 
	 (const char*) m_org_ped_bemc->Path(),   m_org_ped_bemc->GetName());    
  printf("** Type=St_emc_calib_header  Name=%scal/%s \n", 
	 (const char*) m_org_slp_bemc_h->Path(), m_org_slp_bemc_h->GetName());  
  printf("** Type=St_emc_adcslope      Name=%scal/%s \n", 
	 (const char*) m_org_slp_bemc->Path(),   m_org_slp_bemc->GetName());    
  printf("** Type=St_emc_adcslope      Name=%scal/%s \n", 
	 (const char*) m_org_slp_bemc->Path(),   m_org_slp_bemc->GetName());          

  path="/bfc/.make/.data/params/geant/";
  printf("\n** Type=St_emc_adcslope      Name=%s%s \n", 
	 (const char*)   m_calb_calg->Path(),      m_calb_calg->GetName());
  printf("**** \n");
}
//_____________________________________________________________________________
void St_ems_Maker::printmBEMC(){
  if(!mBEMC) cout<<" BEMC out of CHAIN  mBEMC="<<mBEMC<<endl;
  else       cout<<" BEMC     in CHAIN  mBEMC="<<mBEMC<<endl;
}
//_____________________________________________________________________________
void St_ems_Maker::printmEEMC(){
  if(!mEEMC) cout<<" EEMC out of CHAIN  mEEMC="<<mEEMC<<endl;
  else       cout<<" EEMC     in CHAIN  mEEMC="<<mEEMC<<endl;
}



