//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcSimulatorMaker is class for begin_html <FONT COLOR="RED">EMC Simulation</FONT> end_html dataset
//
//                                                                      
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <assert.h>
#include <math.h>
#include <TRandom.h>
#include <StMessMgr.h>

#include "StEmcSimulatorMaker.h"
#include "StEmcSimpleSimulator.h"
#include "StEmcPmtSimulator.h"
#include "StPmtSignal.h"
#include "StMcCalorimeterHit.hh"
#include "StMcEmcHitCollection.hh"
#include "StEmcCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"

#include "tables/St_g2t_emc_hit_Table.h"
#include "tables/St_controlEmcSimulatorMaker_Table.h"

ClassImp(StEmcSimulatorMaker)

TDataSet  *geaIn;
St_g2t_emc_hit *g2t_emc_hit, *g2t_smd_hit;

#include "../StEmcUtil/emcDetectorName.h"

St_controlEmcSimulatorMaker* controlMaker; controlEmcSimulatorMaker_st* controlTable;

St_controlEmcPmtSimulator* pmtSimulator; controlEmcPmtSimulator_st* pmtTable;

StEmcSimulatorMaker::StEmcSimulatorMaker(const char *name):StMaker(name)
{
  //
  // Default value of parameters for this maker
  //
   mBEMC        = 2;  // >0 BEMC yon
   mEEMC        = 0;  // EEMC  of
   mHistControl = 1;  // Hist  on
   SetDebug(0);       // Debug of
   gMessMgr->SetLimit("StEmcSimulator",100);

}

StEmcSimulatorMaker::~StEmcSimulatorMaker() { /* nothing */}

Int_t StEmcSimulatorMaker::Init()
{
  //
  // Get data from StarDb ( if exist)
  // 
  TDataSet *simEmcPar = GetInputDB("emc/simulator");
  if(simEmcPar){

    TDataSetIter  local(simEmcPar);
    controlMaker = (St_controlEmcSimulatorMaker*) local("controlEmcSimulatorMaker");
    if(controlMaker){
      controlTable = controlMaker->GetTable();
      mBEMC        = controlTable[0].bemc;
      mEEMC        = controlTable[0].eemc;
      mHistControl = controlTable[0].hist;
      SetDebug(controlTable[0].debug);
      gMessMgr->SetLimit("StEmcSimulator",(Int_t)controlTable[0].messLimit);
    }

    pmtSimulator = (St_controlEmcPmtSimulator*) local("Simulator");
    if(pmtSimulator && pmtSimulator->GetNRows() == 4) {
      pmtTable = pmtSimulator->GetTable();
    }

  }

  if(mBEMC > 0) { 
    printmBEMC(); 
    for(Int_t i=BEMC-1; i<BSMDP; i++,pmtTable++) {
      if(!mGeom[i]) {
        mGeom[i] = new StEmcGeom(i+1);
      }

      if(!mGeom[i]) {
        gMessMgr->Error() << "\n Geometry for detector "<<i+1<<" undefined \n " << endm;
        assert(0);
      }
      else if(Debug() == 1) mGeom[i]->printGeom();

      // Initialise the simulators
      if(mBEMC > 1) {
        if(i<BPRS){
	  StEmcPmtSimulator* pmt;
          pmt = new StEmcPmtSimulator(i+1);
          if(pmtTable) {
            pmt->setControl(pmtTable);
            mSimulator[i] = pmt;
          }
        }
        else {
	  StEmcSimpleSimulator* simple;
	  simple = new StEmcSimpleSimulator(i+1);
          if(pmtTable) {
            simple->setControl(pmtTable);
            mSimulator[i] = simple;
          }
        }
        if(mSimulator[i]) mSimulator[i]->print();
      }

      if(mHistControl) bookHistograms(i);

    }
  }

  if(mEEMC){ /* nothing */ }

  saveRunco();

  return StMaker::Init();
}

void StEmcSimulatorMaker::saveRunco()
{
  if(controlMaker) {
    St_controlEmcSimulatorMaker* copyControl=new St_controlEmcSimulatorMaker((*controlMaker));
    copyControl->SetName(controlMaker->GetName()); 
    AddRunco((TDataSet*)copyControl);
  }

  if(pmtSimulator){
    St_controlEmcPmtSimulator* copyPmt=new St_controlEmcPmtSimulator((*pmtSimulator));
    copyPmt->SetName(pmtSimulator->GetName());
    AddRunco((TDataSet*)copyPmt);
  }
}

void StEmcSimulatorMaker::bookHistograms(const Int_t i)
{
  const Char_t* tit[]={"Barrel ","Endcap "};

  const Int_t   nx[] = {40,40,300,20,12,12,12,12};
  const Float_t xl[] = {-1.0,-1.0,-1.0,-1.0, 0.5 , 0.5, 0.5, 0.5};
  const Float_t xu[] = { 1.0, 1.0, 1.0, 1.0, 12.5,12.5,12.5,12.5};
  const Int_t   ny[] = {120, 120, 60, 900, 60, 60, 60, 60};
  if(!m_nhit){
    m_nhit = new TH2F("EmcNHitsVsDet(ems)" ,"Number of hit(log) .vs. Detector #",100,0.0,4.5,8,0.5,8.5);
    m_etot = new TH2F("EmcEtotVsDet(ems)" ,"Total energy(log) .vs. Detector #",100,-4.0,4.5,8,0.5,8.5);
  }

  TString name_h   = detname[i] + "Hits";
  TString name_e   = detname[i] + "Energy";
  TString name_adc = detname[i] + "Adc";
  Int_t ind = (i<BSMDP) ? 0 : 1;
  TString title_h  = tit[ind] + detname[i] + " hits dist.";
  TString title_e  = tit[ind] + detname[i] + " energy dist.";
  TString title_adc= tit[ind] + detname[i] + " ADC dist.";

  Float_t rpi = M_PI + 0.00001; 
  m_hits[i]   = new TH2F(name_h,title_h, nx[i],xl[i],xu[i], ny[i],-rpi, rpi);
  m_energy[i] = new TH2F(name_e,title_e, nx[i],xl[i],xu[i], ny[i],-rpi, rpi);
  m_adc[i]    = new TH1F(name_adc,title_adc, 5001, -0.5, 5000.5);
}

void StEmcSimulatorMaker::makeHistograms(const Int_t det)
{
  Float_t energysum=0.0, etsum=0.0; 
  Float_t E, eta, phi; 
  Int_t nhit=0, id,m,e,s, adc;

  St_emc_hits* emc_hits = mEmcRawHits[det-1];
  Int_t n = emc_hits->GetNRows();

  if(n>0){
    emc_hits_st *hit = emc_hits->GetTable();
     
    for(Int_t i = 0; i<n; i++){
       m   = (Int_t)hit[i].module; 
       e   = (Int_t)hit[i].eta;
       s   = (Int_t)hit[i].sub;
       adc = (Int_t)hit[i].adc;  // For testing only
       E   =        hit[i].energy;

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
       else gMessMgr->Warning()<<"StEmcSimulatorMaker::makeHistograms=>bad index det "
			      <<det<<" m "<<m<<" e "<<e<<" s "<<s<<" id "<<id<<endm;         
    }
    m_nhit->Fill(log10((Double_t)nhit), (Float_t)det);
    m_etot->Fill(log10((Double_t)energysum), (Float_t)det);
  }
}

Int_t StEmcSimulatorMaker::Make()
{
  //  Find  Geant  Tables
  geaIn = GetDataSet("geant"); // Input from fzin file
  if (geaIn == 0) {
    geaIn = GetDataSet("event/geant/Event"); // Input from xdf file
    if (geaIn == 0) {
      gMessMgr->Error()<<"Geant Data didn't find in event/geant/Event and geant directories"<<endm;
      return kStWarn;
    }
  }

  Int_t retBemc, retEemc;
  if(mBEMC) retBemc = makeBemc();
  if(mEEMC) retEemc = makeEemc();

  fillStEvent();

  return retBemc;
}

Int_t StEmcSimulatorMaker::makeBemc()
{
  //
  // Transition from g2t table to StMcEvent's table style 
  //
  for(Int_t i=BEMC-1; i<BSMDP; i++) {
    TString nameHits = detname[i] + "McHits";
    mEmcMcHits[i] = new StMcEmcHitCollection(nameHits.Data());
    m_DataSet->Add(mEmcMcHits[i]);
  }

  g2t_emc_hit = (St_g2t_emc_hit *) geaIn->Find("g2t_emc_hit");
  if (g2t_emc_hit && g2t_emc_hit->GetTableSize()>0){ // O"k
    makeBemcAndBprsMcHits();
  }
  else gMessMgr->Warning()<<
  "StEmcSimulatorMaker::makeBemc=>table g2t_emc_hit isn't found in dataset or size is 0 "<< 
       geaIn->GetName()<<endm;

  g2t_smd_hit = (St_g2t_emc_hit *) geaIn->Find("g2t_smd_hit");
  if (g2t_smd_hit && g2t_smd_hit->GetTableSize()>0){ // O"k
    makeBsmdeAndBsmdpMcHits();
  }
  else gMessMgr->Warning()<<
  "StEmcSimulatorMaker::makeBemc=>table g2t_smd_hit isn't found in dataset or size is 0 " << 
       geaIn->GetName() << endm;

  if(Debug() == 1){
    for(UInt_t i=BEMC-1; i<BSMDP; i++){mEmcMcHits[i]->print();} 
  }

  if(mBEMC >= 2) { 
    makeAllRawHitsForBemc();
  }
  
  if(mHistControl) {
    makeHistograms(BEMC);
    makeHistograms(BPRS);
    makeHistograms(BSMDE);
    makeHistograms(BSMDP);
  }
  
  return kStOK;
}

Int_t StEmcSimulatorMaker::makeBemcAndBprsMcHits()
{
  //
  // Decode g2t_emc_hit and fill Mc Hits for BRMC and BPRS.
  // See StMcEventMaker::fillBemc() method.
  //
  Int_t module, eta, sub, detector; 
  Float_t de;
  StMcCalorimeterHit *emchBemc, *emchBprs;

  g2t_emc_hit_st *hit = g2t_emc_hit->GetTable();
  Int_t nhits         = g2t_emc_hit->GetNRows();

  for(Int_t ihit=0; ihit<nhits; ihit++,hit++) { 
    mGeom[BEMC-1]->getVolIdBemc(hit->volume_id, module,eta,sub,detector);
    de   = hit->de;

    if (detector == BEMC || detector == BPRS) {
      emchBemc = new StMcCalorimeterHit(module,eta,sub,de); // Don't trace for track
      emchBprs = 0;                                         // For safety
      StMcEmcHitCollection::EAddHit bemcNew = mEmcMcHits[BEMC-1]->addHit(emchBemc);

      if     (bemcNew == StMcEmcHitCollection::kNew){ 
        if(detector == 2) emchBprs = new StMcCalorimeterHit(module,eta,sub,de);
      }
      else if(bemcNew == StMcEmcHitCollection::kAdd){ 
        emchBprs = emchBemc;
      }
      else if(bemcNew == StMcEmcHitCollection::kErr){ 
	delete emchBemc;
        emchBprs = 0;
        gMessMgr->Warning()<<" Bad hit in Bemc collection " << endm;
      }

      if(detector == BPRS && emchBprs) {
        StMcEmcHitCollection::EAddHit bprsNew = mEmcMcHits[BPRS-1]->addHit(emchBprs);
        if(bprsNew != StMcEmcHitCollection::kNew) delete emchBprs;
      }

    }
  }
  return kStOk;
}

Int_t StEmcSimulatorMaker::makeBsmdeAndBsmdpMcHits()
{
  //
  // Decode g2t_emc_hit and fill Mc Hits for BRMC and BPRS => see StMcEventMaker  
  // See StMcEventMaker::fillBsmd() method.
  //
  Int_t module, eta, sub, detector; 
  Float_t de;
  StMcCalorimeterHit *emchBsmd;

  g2t_emc_hit_st *hit = g2t_smd_hit->GetTable();
  Int_t nhits         = g2t_smd_hit->GetNRows();

  for(Int_t ihit=0; ihit<nhits; ihit++,hit++) { 
    mGeom[BSMDE-1]->getVolIdBsmd(hit->volume_id, module,eta,sub,detector);
    de   = hit->de;

    if (detector == BSMDE || detector == BSMDP) {
      emchBsmd = new StMcCalorimeterHit(module,eta,sub,de); // Don't trace for track
      StMcEmcHitCollection::EAddHit bsmdNew = mEmcMcHits[detector-1]->addHit(emchBsmd);
      if(bsmdNew == StMcEmcHitCollection::kAdd){ 
	delete emchBsmd;
      }
      else if(bsmdNew == StMcEmcHitCollection::kErr){ 
	delete emchBsmd;
        gMessMgr->Warning()<<"StEmcSimulatorMaker::makeBsmdeAndBsmdpMcHits=>bad hit in Bsmd collection " << endm;
      }
    }
    else gMessMgr->Warning()<<" StEmcSimulatorMaker::makeBsmdeAndBsmdpMcHits=>Bad detector value in Bsmd collection =>" << detector <<endm;
  }
  return kStOk;
}
//_____________________________________________________________________________
Int_t StEmcSimulatorMaker::makeAllRawHitsForBemc()
{
  //
  // Transition from deposit energy to adc (and energy) 
  //
  UInt_t  m, eta, sub, adc; Float_t de, energy;
  Float_t rEta;
  emc_hits_st rawHit;

  for(Int_t i=BEMC-1; i<BSMDP; i++) { 
    TString nw = detname[i] + "RawHits"; // Define tables (old style)
    const ULong_t nhits   = mEmcMcHits[i]->numberOfHits();
    mEmcRawHits[i] = new St_emc_hits(nw, nhits);
    m_DataSet->Add(mEmcRawHits[i]);

    rawHit.det = i + 1;
    if(nhits > 0) {
      for(m=0; m<mEmcMcHits[i]->numberOfModules(); m++){ // Cycle on module
        const StMcEmcModuleHitCollection* module = mEmcMcHits[i]->module(m);
        const ULong_t nhm = module->numberOfHits();
        if(nhm>0){
          rawHit.module = m + 1;
          const StSPtrVecMcCalorimeterHit hits = module->hits(); 
          for(UInt_t ihm=0; ihm<nhm; ihm++){
            eta = hits[ihm]->eta();
            sub = hits[ihm]->sub();
            de  = hits[ihm]->dE();

            if(mGeom[i]->getEta(m,eta, rEta) == 0) { 
              adc    = mSimulator[i]->getAdc((Double_t)de, (Double_t)rEta);
              if(adc>0) { // Zero suppression
                energy = mSimulator[i]->getEnergy();

                rawHit.eta    = eta;
                rawHit.sub    = sub;
                rawHit.adc    = adc;
                rawHit.energy = energy;
                mEmcRawHits[i]->AddAt(&rawHit);
              }
            }
            else gMessMgr->Warning()<<"StEmcSimulatorMaker::makeAllRawHitsForBemc() m "
				    <<m<<" eta "<<eta<<" and bad rEta "<<rEta<<endm; 
          }
        }
      }
    }
  }
  return kStOk;
}

Int_t StEmcSimulatorMaker::makeEemc() { return kStOk;}

Int_t StEmcSimulatorMaker::fillStEvent(){

  mEmcCollection = new StEmcCollection();

  AddData(new St_ObjectSet("EmcCollection" , mEmcCollection)); // ??

  for(Int_t i=0; i<MAXDET; i++){ //Loop over detectors
    //    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StDetectorId id = (StDetectorId)(i+kBarrelEmcTowerId);
    St_emc_hits  *table = mEmcRawHits[i];
    if(table){

      StEmcDetector* detector = new StEmcDetector(id, mGeom[i]->NModule());
      mEmcCollection->setDetector(detector);

      emc_hits_st *t = table->GetTable();

      for(Int_t j=0; j<table->GetNRows(); j++){
	StEmcRawHit* hit = new StEmcRawHit(id, 
			  	           t[j].module, t[j].eta, t[j].sub,
				           t[j].adc, t[j].energy);
	detector->addHit(hit);
      }	
    }
  }
  return kStOK;
}

void StEmcSimulatorMaker::printmBEMC(){
  if(!mBEMC) gMessMgr->Info()<<" BEMC out of CHAIN  mBEMC="<<mBEMC<<endm;
  else       gMessMgr->Info()<<" BEMC     in CHAIN  mBEMC="<<mBEMC<<endl;
}

//////////////////////////////////////////////////////////////////////////
// $Id: StEmcSimulatorMaker.cxx,v 1.1 2000/10/23 22:53:14 pavlinov Exp $
// $Log: StEmcSimulatorMaker.cxx,v $
// Revision 1.1  2000/10/23 22:53:14  pavlinov
// First working C++ version
//
//////////////////////////////////////////////////////////////////////////
