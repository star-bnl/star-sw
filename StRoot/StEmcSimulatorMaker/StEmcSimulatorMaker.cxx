//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcSimulatorMaker is class for begin_html <FONT COLOR="RED">EMC Simulation</FONT> end_html dataset
//
//                                                                      
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <assert.h>
#include <math.h>
#include "TROOT.h"
#include <TRandom.h>
#include <TBrowser.h>
#include <TPad.h>
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
#include "StBFChain.h"

#include "tables/St_g2t_emc_hit_Table.h"
#include "tables/St_controlEmcSimulatorMaker_Table.h"
#include "tables/St_ems_hits_Table.h"

ClassImp(StEmcSimulatorMaker)

TDataSet  *geaIn, *ems;
St_g2t_emc_hit *g2t_emc_hit, *g2t_smd_hit;

#include "StEmcUtil/emcDetectorName.h"

St_controlEmcSimulatorMaker* controlMaker; controlEmcSimulatorMaker_st* controlTable;

St_controlEmcPmtSimulator* pmtSimulator; controlEmcPmtSimulator_st* pmtTable;

StEmcSimulatorMaker::StEmcSimulatorMaker(const char *name):StMaker(name)
{
  //
  // Default value of parameters for this maker
  //
   mBEMC        = 2;  // >0 BEMC 
   mEEMC        = 0;  // EEMC  of
   mHistControl = 1;  // Hist  on
   mCompare     = kFALSE;
   gMessMgr->SetLimit("StEmcSimulator",100);
}

StEmcSimulatorMaker::~StEmcSimulatorMaker() 
{
  if(mEmcCollection) delete mEmcCollection; // 25-jan-2001
}

Int_t StEmcSimulatorMaker::Init()
{
  // 21-mar-2001 for comparing
  StBFChain *chain;  // see StEmcGeom::StEmcGeom()
  TList *tl = (TList*)gROOT->GetListOfBrowsables();
  if(tl) chain = static_cast<StBFChain*>(tl->FindObject("bfc")); 
  // Old simulator exist => switch on comparing
  if(chain && chain->GetOption("ems")) mCompare = kTRUE;
  printf("<I> #### Compare mode => %i ##### \n",mCompare);
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
        mGeom[i] = StEmcGeom::getEmcGeom(i+1);
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
  Histograms()->SetName("SimuHist");

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
  //
  // i - array index (det = i + 1) !!! 
  //
  const Char_t* tit[]={"Barrel ","Endcap "};

  const Int_t   nx[] = {40,40,300,20,12,12,12,12};
  const Float_t xl[] = {-1.0,-1.0,-1.0,-1.0, 0.5 , 0.5, 0.5, 0.5};
  const Float_t xu[] = { 1.0, 1.0, 1.0, 1.0, 12.5,12.5,12.5,12.5};
  const Int_t   ny[] = {120, 120, 60, 900, 60, 60, 60, 60};
  if(!m_nhit){
    m_nhit = new TH2F("EmcNHitsVsDet" ,"Number of hit(log) .vs. Detector #",100,0.0,4.5,8,0.5,8.5);
    m_etot = new TH2F("EmcEtotVsDet" ,"Total energy(log) .vs. Detector #",100,-4.0,4.5,8,0.5,8.5);
  }

  gMessMgr->Info()<<" Book hist for detector " << detname[i].Data() <<endm;

  TString name_h   = detname[i] + "Hits";
  TString name_e   = detname[i] + "Energy";
  TString name_adc = detname[i] + "Adc";
  Int_t ind = (i<BSMDP) ? 0 : 1;
  TString title_h  = tit[ind] + detname[i] + " hits dist.";
  TString title_e  = tit[ind] + detname[i] + " energy dist.";
  TString title_adc= tit[ind] + detname[i] + " ADC dist.";

  Float_t rpiMax = M_PI-0.0001, rpiMin = -M_PI-0.0001; // -pi<=phi<pi 
  if(i==2) {
  // For SMDE only
    Int_t neta = mGeom[i]->NEta(), iw1, iw2;
    Float_t* eb = mGeom[i]->Eta();
    TArrayD xb(2*neta+1); 
    xb[neta]   = 0.0;
    for(Int_t ik=0; ik<neta; ik++){
      iw1 = neta + 1 + ik;
      iw2 = neta-ik-1;
      Float_t x1 = eb[ik], x2, xw;
      if(ik<neta-1) {
        x2 = eb[ik+1];
        xw = (x1+x2)*0.5;
      }
      else xw = 0.99;
      xb[iw1] = +xw;
      xb[iw2] = -xw;
      printf(" iw1 %i %f => iw2 %i %f => eta %f\n", iw1,xb[iw1], iw2,xb[iw2], eb[ik]);
    }
    // Be carefull with array size !!! 
    m_hits[i]   = new TH2F(name_h,title_h, xb.GetSize()-1, xb.GetArray(), ny[i],rpiMin,rpiMax);
    m_energy[i] = new TH2F(name_e,title_e, xb.GetSize()-1, xb.GetArray(), ny[i],rpiMin,rpiMax);
  }
  else {
    m_hits[i]   = new TH2F(name_h,title_h, nx[i],xl[i],xu[i], ny[i],rpiMin,rpiMax);
    m_energy[i] = new TH2F(name_e,title_e, nx[i],xl[i],xu[i], ny[i],rpiMin,rpiMax);
  }
  Int_t maxAdc= mGeom[i]->getMaxAdc();
  m_adc[i]    = new TH1F(name_adc,title_adc, maxAdc+1, -0.5, float(maxAdc)+0.5); // ??

  if(mHistControl >= 2) {
    TString nameM    = detname[i] + "M";
    TString titModule= tit[ind] + detname[i] + " #Module dist.";
    mhModule[i]      = new TH1F(nameM,titModule, 121, -0.5, 120.5);
    nameM     = detname[i] + "Sub";
    titModule = tit[ind] + detname[i] + " #Sub. dist.";
    mhSub[i]  = new TH1F(nameM,titModule, 15, 0.5, 15.5);
  }

  if(mCompare){
    TString name=detname[i] + "NDif";
    TString tit=detname[i] + " Diff. of hits number";
    mhDiffNumHits[i] = new TH1F(name,tit, 11,-5.5,+5.5);
    name = detname[i] + "DifDe";
    tit  = detname[i] + " Difference of DE"; 
    mhDiffDe[i] = new TH1F(name,tit, 11,-5.5e-5,+5.5e-5);
  }

}

void StEmcSimulatorMaker::makeHistograms(const Int_t det)
{
  Float_t energysum=0.0, etsum=0.0; 
  Float_t E, eta, phi; 
  Int_t nhit=0, m,e,s, adc;

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

       Int_t ieta=mGeom[det-1]->getEta(m, e, eta); 
       Int_t iphi=mGeom[det-1]->getPhi(m, s, phi);
       if(ieta==0 && iphi==0) {
         if(E){
           m_hits[det-1]->Fill(eta,phi); 
           m_energy[det-1]->Fill(eta,phi,E); 
           m_adc[det-1]->Fill((float)adc); 
           nhit      += 1; 
           energysum += E; 
           etsum     += E/cosh(eta);  // Et = E*sin(theta); sin(theta) = 1./cos(eta)
           if(mHistControl >= 2) {
             if(mhModule[det-1]) mhModule[det-1]->Fill(Axis_t(m));
             if(mhSub[det-1])    mhSub[det-1]->Fill(Axis_t(s));
           }
         }
       }
       else gMessMgr->Warning()<<"StEmcSimulatorMaker::makeHistograms=>bad index det "
			      <<det<<" m "<<m<<" e "<<e<<" s "<<s<<endm;         
    }
    m_nhit->Fill(log10((Double_t)nhit), (Float_t)det);
    m_etot->Fill(log10((Double_t)energysum), (Float_t)det);
  }
}

Int_t StEmcSimulatorMaker::Make()
{
  // change order of searching 
  static Char_t* typeOfFile[2]={"xdf", "fz"};
  static Char_t* nameIn[2]={"event/geant/Event", "geant"};
  //  Find  Geant  directory with hits
  for(Int_t i=0; i<2; i++){
     geaIn = GetDataSet(nameIn[i]);
     if(geaIn) {
        printf("Type of file -> %s : GEANT directory -> %s\n", typeOfFile[i], nameIn[i]);
        break;
     }
  }
  if (geaIn == 0) {
     gMessMgr->Error()<<"Geant Data didn't find in "<< nameIn[0]<<" or "<< nameIn[1]<<endm;
     return kStWarn;
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
  
  if(mCompare) compareOldSimulator();
  
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
  UInt_t  m, mf, eta, sub, adc; Float_t de, energy;
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
        mf = m + 1; // m - C style index; mf - Fortran style index !!!!
        const StMcEmcModuleHitCollection* module = mEmcMcHits[i]->module(mf);
        const ULong_t nhm = module->numberOfHits();
        if(nhm>0){
          rawHit.module = mf;
          const StSPtrVecMcCalorimeterHit hits = module->hits(); 
          for(UInt_t ihm=0; ihm<nhm; ihm++){
            eta = hits[ihm]->eta();
            sub = hits[ihm]->sub();
            de  = hits[ihm]->dE();

            if(mGeom[i]->getEta(mf, eta, rEta) == 0) { // (m => mf) 15-mar-2001
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
            else gMessMgr->Warning()<<"StEmcSimulatorMaker::makeAllRawHitsForBemc() Bad m "
				    <<m<<" or eta "<<eta <<endm; 
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

void StEmcSimulatorMaker::Browse(TBrowser* b)
{
//  Will be see StEmcCollection in browser as separate entity (if unzero)
  if(mEmcCollection) b->Add((TObject*)mEmcCollection);
  TDataSet::Browse(b);
}

void StEmcSimulatorMaker::printmBEMC()
{
  if(!mBEMC) gMessMgr->Info()<<" BEMC out of CHAIN  mBEMC="<<mBEMC<<endm;
  else       gMessMgr->Info()<<" BEMC     in CHAIN  mBEMC="<<mBEMC<<endl;
}

void StEmcSimulatorMaker::compareOldSimulator()
{
  //
  // 21-mar-2001 for comparing "new" and "old" simulator (deposit energy)
  //
  ems = GetDataSet("emc_raw/.data");
  St_ems_hits *bemc;
  ems_hits_st *tab;
  StMcEmcHitCollection* bemcM;
  StMcEmcModuleHitCollection* module;
  StMcCalorimeterHit* hMC;

  Int_t nbemc[4]={0,0,0,0}; 
  Int_t det, m, e, s;
  Float_t de;

  bemc = (St_ems_hits*)ems->FindByName("ems_hits_bemc"); // bemc + bprs
  for(Int_t ibr=1; ibr<=2; ibr++){ // bemc+bprs and bsmde+bsmdp
    tab = bemc->GetTable();

    for(Int_t nh=0; nh<bemc->GetNRows(); nh++){
      det = tab[nh].det;
      m   = tab[nh].module;
      e   = tab[nh].eta;
      s   = tab[nh].sub;
      de  = tab[nh].energy;

      bemcM  = getEmcMcHits(det);
      module = bemcM->module(m);
      nbemc[det-1]++;

      StSPtrVecMcCalorimeterHit& hits = module->hits(); 
      for(Int_t mh=0; mh<hits.size(); mh++){
        hMC = hits[mh];
        if(m==hMC->module() && e==hMC->eta() && s==hMC->sub()) {
          mhDiffDe[det-1]->Fill(de-hMC->dE());
          goto ENDCYCLE;
        }
      }
      printf("<W> Did not find New hit for OLD !!!\n");      
      ENDCYCLE:
      continue;
    }
    bemc = (St_ems_hits*)ems->FindByName("ems_hits_bsmd"); // shower max
  }
  for(Int_t i=0; i<4; i++){
    Int_t det=i+1;
    Int_t nOld = nbemc[i];
    Int_t nNew = getEmcMcHits(det)->numberOfHits();
    mhDiffNumHits[i]->Fill(float(nOld-nNew));
  }
}

void StEmcSimulatorMaker::pictureAllDetectors(Int_t print)
{
  //
  // 22-mar-2001 for convinience
  //
  if(!mC1) mC1 = new TCanvas("mC1","Picture for all detectors",0,25,600,800);
  else     mC1->SetTitle("Picture for all detectors");

  mC1->Clear();
  mC1->Divide(1,2);

  mC1->cd(1); 
  m_nhit->SetLineWidth(4);
  m_nhit->Draw();
  mC1->cd(2);
  m_etot->SetLineWidth(4);
  m_etot->Draw();
  mC1->Update();
  if(print) mC1->Print("ps/newSim/allDetectors.ps");
}

void StEmcSimulatorMaker::pictureForDetector(Int_t det, Int_t logy, Int_t print)
{
  if(!mC1) mC1 = new TCanvas("mC1","Picture for detector",0,25,600,800);
  else     mC1->SetTitle("Picture for detector");

  mC1->Clear();
  mC1->Divide(1,3);

  Int_t i = det-1;
  i = (i<0)?0:((i>3)?3:i);

  mC1->cd(1); 
  m_hits[i]->SetLineWidth(4);
  m_hits[i]->Draw();
  mC1->cd(2);
  m_energy[i]->SetLineWidth(4);
  m_energy[i]->Draw();
  mC1->cd(3);
  m_adc[i]->SetLineWidth(4);
  gPad->SetLogy(logy);
  m_adc[i]->Draw();
  mC1->Update();
  if(print) {
    TString name("ps/newSim/Det");
    name += detname[i] + ".ps";
    mC1->Print(name.Data());
  }
}

void StEmcSimulatorMaker::pictureCompareDe(Int_t print)
{
  //
  // 22-mar-2001 for comparing "new" and "old" simulator (deposit energy)
  //
  if(mCompare){
    if(!mC1) mC1 = new TCanvas("mC1","DE comparing for OLD and NEW Simulator",0,25,600,800);
    else     mC1->SetTitle("DE comparing for OLD and NEW Simulator");

    mC1->Clear();
    mC1->Divide(2,4);
    for(Int_t i=0; i<4; i++){
      mC1->cd(2*i+1); 
      mhDiffNumHits[i]->SetLineWidth(4);
      mhDiffNumHits[i]->Draw();
      mC1->cd(2*i+2);
      mhDiffDe[i]->SetLineWidth(4);
      mhDiffDe[i]->Draw();
    }
    mC1->Update();
    if(print) mC1->Print("ps/newSim/CompareOldNewDe.ps");
  }
  else printf("<I> Picture unavailable : mCompare is zero \n");
}

//////////////////////////////////////////////////////////////////////////
// $Id: StEmcSimulatorMaker.cxx,v 1.8 2002/05/30 17:35:06 pavlinov Exp $
// $Log: StEmcSimulatorMaker.cxx,v $
// Revision 1.8  2002/05/30 17:35:06  pavlinov
// changed the way of searching of GEANT data
//
// Revision 1.7  2001/09/22 00:29:42  pavlinov
// No public constructor for StEmcGeom
//
// Revision 1.6  2001/05/14 01:21:45  pavlinov
// In method StMcEmcHitCollection::module(m) m is module number, not index
//
// Revision 1.5  2001/03/23 19:02:51  pavlinov
// Get pointer to chain via list of browsables
//
// Revision 1.4  2001/03/22 22:04:38  pavlinov
// Clean up for mdc4
//
// Revision 1.3  2001/03/15 17:21:32  pavlinov
// Fixed error for module=1
//
// Revision 1.2  2001/02/02 23:59:59  pavlinov
// New function Browse() and cleanup for new version of BFC
//
// Revision 1.1  2000/10/23 22:53:14  pavlinov
// First working C++ version
//
//////////////////////////////////////////////////////////////////////////
