//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcSimulatorMaker is class for begin_html <FONT COLOR="RED">EMC Simulation</FONT> end_html dataset
//
//                                                                      
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
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
// DB staff 
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_emcRunning_Table.h"
#include "tables/St_smdRunning_Table.h"
#include "tables/St_emcCalibration_Table.h"
#include "tables/St_emcPedestal_Table.h"
#include "tables/St_smdPedestal_Table.h"

#include "tables/St_emcStatus_Table.h"
#include "tables/St_smdStatus_Table.h"
#include "tables/St_emcCalib_Table.h"
#include "tables/St_smdCalib_Table.h"
#include "tables/St_emcPed_Table.h"
#include "tables/St_smdPed_Table.h"

ClassImp(StEmcSimulatorMaker)

TDataSet  *geaIn, *ems;
St_g2t_emc_hit *g2t_emc_hit, *g2t_smd_hit;

#include "StEmcUtil/others/emcDetectorName.h"

St_controlEmcSimulatorMaker* controlMaker; controlEmcSimulatorMaker_st* controlTable;

St_controlEmcPmtSimulator* pmtSimulator; controlEmcPmtSimulator_st* pmtTable;

TDataSet* calibDB=0, *statusDB=0, *status=0, *ped=0;
St_emcStatus* statusEmc=0;   // status for BEMC  or BPRS
emcStatus_st* statusEmcRec=0;
St_smdStatus* statusSmd=0;   // status for BSMDE or BSMDP
smdStatus_st* statusSmdRec=0;
Int_t emcDbDate = 20010924; // 24-09-2001(day-month-year) - date after that emc DB data exist 

St_controlEmcSimulatorMaker *StEmcSimulatorMaker::getControlSimulator() { return controlMaker;} 
St_controlEmcPmtSimulator   *StEmcSimulatorMaker::getControlPmtSimulator() { return pmtSimulator;} 

StEmcSimulatorMaker::StEmcSimulatorMaker(const char *name):StMaker(name)
{
  //
  // Default value of parameters for this maker
  //
   mBEMC        = 2;  // >0 BEMC 
   mEEMC        = 0;  // EEMC  of
   mHistControl = 1;  // Hist  on
   mCompare     = kFALSE;
   mDB          = 0;
   mPrint       = kTRUE;
   gMessMgr->SetLimit("StEmcSimulator",100);
   
   for(int i =0;i<MAXDET;i++)
   {
     mEmcMcHits[i] = NULL;
     mEmcRawHits[i]= NULL;
     mSimulator[i] = NULL; 
     mGeom[i] = NULL;
   }
   mEmcCollection = NULL;
   mDbMaker       = NULL;
   mC1            = NULL;

   m_nhit         = 0;

}
void StEmcSimulatorMaker::Clear(const char *)
{
  delete mEmcCollection;mEmcCollection=0;
  StMaker::Clear();		
}
StEmcSimulatorMaker::~StEmcSimulatorMaker() 
{
  if(mEmcCollection) delete mEmcCollection; // 25-jan-2001
  for (int det=0;det<MAXDET;det++) delete mSimulator[det];  //!
}
Int_t StEmcSimulatorMaker::Init()
{
  // 21-mar-2001 for comparing
  // Old simulator exist => switch on comparing
  
  // there is no ems option in chain anymore AAPSUAIDE (JAN/2003)
  /*StBFChain *chain = (StBFChain*) GetChain();
  if(chain) if(chain->GetOption("ems")) mCompare = kTRUE;
  printf("<I> #### Compare mode => %i ##### \n",mCompare);*/
  
  //
  // Get data from StarDb ( if exist)
  //  
  TDataSet *simEmcPar = GetInputDB("emc/simulator");
  pmtTable =NULL;
  if(simEmcPar){

    TDataSetIter  local(simEmcPar);
    controlMaker = (St_controlEmcSimulatorMaker*) local("controlEmcSimulatorMaker");
    if(controlMaker){
      controlTable = controlMaker->GetTable();
      mBEMC        = controlTable->bemc;
      mEEMC        = controlTable->eemc;
      mHistControl = controlTable->hist;
      SetDebug(controlTable->debug);
      gMessMgr->SetLimit("StEmcSimulator",Int_t(controlTable->messLimit));
      // Db for calibration
      mDbMaker     = (St_db_Maker*)GetMaker("db");
      Int_t dbDate = mDbMaker->GetDateTime().GetDate();
      Int_t detInDb=0;
      for(Int_t i=0; i<4; i++) {
         if (controlTable->keyDB[i] && dbDate>emcDbDate) {
            detInDb++;
            if((Int_t)mDB < controlTable->keyDB[i]) mDB = controlTable->keyDB[i];
         } else {
	    controlTable->keyDB[i] = 0;   // push to no DB mode 
         }
         printf("<I> key for DB : det %2i keyDB %i\n", i+1, controlTable->keyDB[i]); 
      }
      if(mDB) {
        gMessMgr->Info()<<"Calibration db in action for " << detInDb << " detector(s)"<<endm; 
        if(mDbMaker) {
	  // 10-sep-2002 - I can not do that because it changes the global time stamp 
	  //           printf("Time stamp from control table : date %i time %i \n",
	  //           controlTable->dateDB,controlTable->timeDB);  
	  //	   mDbMaker->SetDateTime(controlTable->dateDB, controlTable->timeDB); 
          printf("Time stamp from DB %s\n", mDbMaker->GetDateTime().AsString());
        } else {
           gMessMgr->Error()<<"No DB !! bye " << endm;
           assert(0); 
        }
      }
    }

    pmtSimulator = (St_controlEmcPmtSimulator*) local("Simulator");
    if(pmtSimulator && pmtSimulator->GetNRows() == 4) {
      pmtTable = pmtSimulator->GetTable();
    }

  }

  if(mBEMC > 0) { 
    printmBEMC(); 
    for(Int_t i=BEMC-1; i<BSMDP; i++) {
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
          pmt->setPrint(mPrint);
          if(pmtTable) {
            pmt->setControl(pmtTable);
            }
            mSimulator[i] = pmt;
        }
        else {
	  StEmcSimpleSimulator* simple;
	  simple = new StEmcSimpleSimulator(i+1);
    simple->setPrint(mPrint);
          if(pmtTable) {
            simple->setControl(pmtTable);
            }
            mSimulator[i] = simple;
        }
        if(mSimulator[i]) mSimulator[i]->print();
      }

      if(mHistControl) bookHistograms(i);
    
    if(pmtTable) pmtTable++;

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
    controlMaker = copyControl;              // go away from DB - 31-may-2002 !! 
    controlTable = controlMaker->GetTable();
  }

  if(pmtSimulator){
    St_controlEmcPmtSimulator* copyPmt=new St_controlEmcPmtSimulator((*pmtSimulator));
    copyPmt->SetName(pmtSimulator->GetName());
    AddRunco((TDataSet*)copyPmt);
    pmtSimulator = copyPmt;
    pmtTable     = pmtSimulator->GetTable();
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
  const Int_t   binEnergySum[4] = {4000, 1000, 500, 500};
  const Float_t energySum[4] = {100., 10., 50., 50.};
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
      if(Debug()>=10) printf(" iw1 %i %f => iw2 %i %f => eta %f\n", iw1,xb[iw1], iw2,xb[iw2], eb[ik]);
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
    if(i<4) {
      // this is only for checking
       name_e        = detname[i] + "EnergySum";
       title_e       = tit[ind] + detname[i] + " energy dist(sum)";
       mEnergySum[i] = new TH1F(name_e, title_e, binEnergySum[i], 0.0, energySum[i]);
    }
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
	 // E could be negative  after subtraction of pedestal - 3-jun-2002
         // If pedType=0, then E must be positive.
         m_hits[det-1]->Fill(eta,phi); 
         m_energy[det-1]->Fill(eta,phi,E); 
         m_adc[det-1]->Fill(Axis_t(adc)); 
         nhit      += 1; 
         energysum += E; 
         etsum     += E/cosh(eta);  // Et = E*sin(theta); sin(theta) = 1./cos(eta)
         if(mHistControl >= 2) {
           if(mhModule[det-1]) mhModule[det-1]->Fill(Axis_t(m));
           if(mhSub[det-1])    mhSub[det-1]->Fill(Axis_t(s));
         }
       }
       else if(mPrint) gMessMgr->Warning()<<"StEmcSimulatorMaker::makeHistograms=>bad index det "<<det<<" m "<<m<<" e "<<e<<" s "<<s<<endm;         
    }
    m_nhit->Fill(log10((Double_t)nhit), (Float_t)det);
    m_etot->Fill(log10((Double_t)energysum), (Float_t)det);
    if(mHistControl >= 2) mEnergySum[det-1]->Fill(Double_t(energysum));
  }
}

Int_t StEmcSimulatorMaker::Make()
{
//VPunused    TDataSet *simEmcPar = GetInputDB("emc/simulator");
  // Changed the order of searching - xdf first.
  static Char_t* typeOfFile[3]={"xdf", "geant.root", "fz"};
  static Char_t* nameIn[3]={"event/geant/Event", "geantBranch", "geant"};
  //  Find  Geant  directory with hits
  for(Int_t i=0; i<3; i++){
     geaIn = GetDataSet(nameIn[i]);
     if(geaIn) {
        if(Debug()>=2) if(mPrint) printf("Type of file -> %s : GEANT directory -> %s\n", typeOfFile[i], nameIn[i]);
        break;
     }
  }
  if (!geaIn) {
     if(Debug()>=2) if(mPrint) gMessMgr->Error()<<"Geant Data didn't find in "<< nameIn[0]<<" or "<< nameIn[1]<<endm;
     return kStWarn;
  }

  Int_t retBemc, retEemc;
  if(mBEMC) retBemc = makeBemc();
  if(mEEMC) retEemc = makeEemc();

  fillStEvent();

  return retBemc;   // wait !! and what about retEemc ??
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
  if (g2t_emc_hit ){ // O"k
     if (g2t_emc_hit->GetNRows()>0) {
        makeBemcAndBprsMcHits();
     }  else if(mPrint) gMessMgr->Warning()<< " makeBemc() => table g2t_emc_hit is empty " << endm;
  }  else if(mPrint) gMessMgr->Warning()<< " makeBemc() => table g2t_emc_hit isn't found " << endm;

  g2t_smd_hit = (St_g2t_emc_hit *) geaIn->Find("g2t_smd_hit");
  if (g2t_smd_hit){ 
     if (g2t_smd_hit->GetNRows()>0){
        makeBsmdeAndBsmdpMcHits();
     }  else if(mPrint) gMessMgr->Warning()<< " makeBemc() => table g2t_smd_hit is empty " << endm;  
  }  else if(mPrint) gMessMgr->Warning()<< " makeBemc() => table g2t_smd_hit isn't found " << endm;

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
  StMcCalorimeterHit *emchBemc = NULL, *emchBprs = NULL;

  g2t_emc_hit_st *hit = g2t_emc_hit->GetTable();
  Int_t nhits         = g2t_emc_hit->GetNRows();

  for(Int_t ihit=0; ihit<nhits; ihit++,hit++) 
  { 
    mGeom[BEMC-1]->getVolIdBemc(hit->volume_id, module,eta,sub,detector);
    de = hit->de;

    if (detector == BEMC || detector == BPRS) 
    {
      emchBemc = new StMcCalorimeterHit(module,eta,sub,de); // Don't trace for track
      //if (emchBprs) delete emchBprs;  
      emchBprs = 0;         // For safety
      StMcEmcHitCollection::EAddHit bemcNew = mEmcMcHits[BEMC-1]->addHit(emchBemc);

      if (bemcNew == StMcEmcHitCollection::kNew)
      { 
        if(detector == BPRS) emchBprs = new StMcCalorimeterHit(module,eta,sub,de);
      }
      else if(bemcNew == StMcEmcHitCollection::kAdd)
      { 
        emchBprs = emchBemc;
      }
      else if(bemcNew == StMcEmcHitCollection::kErr)
      { 
	delete emchBemc;
        emchBprs = 0;
        if(mPrint) gMessMgr->Warning()<<" Bad hit in Bemc collection " << endm;
      }

      if(detector == BPRS && emchBprs) 
      {
        StMcEmcHitCollection::EAddHit bprsNew = mEmcMcHits[BPRS-1]->addHit(emchBprs);
        if(bprsNew != StMcEmcHitCollection::kNew) delete emchBprs;
      }
      //if ( emchBemc) delete emchBemc; 

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
        if(mPrint) gMessMgr->Warning()<<"StEmcSimulatorMaker::makeBsmdeAndBsmdpMcHits=>bad hit in Bsmd collection " << endm;
      }
    }
    else if(mPrint) gMessMgr->Warning()<<" StEmcSimulatorMaker::makeBsmdeAndBsmdpMcHits=>Bad detector value in Bsmd collection =>" << detector <<endm;
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
    
  St_emcCalib*  calib=0;        // calibration table
  emcCalib_st*  calibRec=0;
  
  St_smdCalib*  calibSmd=0;     // calibration table
  smdCalib_st*  calibSmdRec=0;
  
  St_emcPed*    pedEmc=0;       // pedestal for BEMC  or BPRS
  emcPed_st*    pedEmcRec=0;
  
  St_smdPed*    pedSmd=0;       // pedestal for BSMDE or BSMDP
  smdPed_st*    pedSmdRec=0;
  
  Int_t   pedType=0, cellID=1, cellInd=0, statusALL=0;
  Float_t pedMean=0, pedRMS=0, calCoef=0;  

  for(Int_t i=BEMC-1; i<BSMDP; i++) 
  { 
    status = ped = calib = 0;
    TString dbName = "Calibrations/emc/y3"+detname[i];
    statusDB = GetInputDB(dbName.Data());

    TString nw = detname[i] + "RawHits"; // Define tables (old style)
    const ULong_t nhits   = mEmcMcHits[i]->numberOfHits();
    mEmcRawHits[i] = new St_emc_hits(nw, nhits);
    m_DataSet->Add(mEmcRawHits[i]);

    rawHit.det = i + 1;
    if (nhits > 0) 
    {
      // DB staff
      if (controlTable->keyDB[i] >=1) 
      { // 15-may-2002
        status   = getStatus(i, statusDB);
        if(status)
        {
          if(i<BPRS) 
          {
            statusEmc    = (St_emcStatus*)status;
            statusEmcRec = statusEmc->GetTable(0); // one row 
          } 
          else 
          {
            statusSmd    = (St_smdStatus*)status;
            statusSmdRec = statusSmd->GetTable(0); // one row 
          }
        } 
        else
        {
          if(mPrint) printf("No status table for detector %i -> %s\n", i, detname[i].Data());
          assert(0);
        }

        calibDB   = GetInputDB(dbName.Data());
        TString tableName = detname[i]+"Calib";

        if(i<BPRS) 
        {
          calib = (St_emcCalib*)calibDB->Find(tableName.Data());
          calibRec = calib->GetTable();
        }
        else 
        {
          calibSmd = (St_smdCalib*)calibDB->Find(tableName.Data());
          calibSmdRec = calibSmd->GetTable();
        }
        if (calib || calibSmd) 
        {
	        if(Debug()>=2) 
          {
            if(mPrint) printf("Calibration table for %s keyDB %i\n", detname[i].Data(), controlTable->keyDB[i]); // ??
            controlMaker->Print(0,1);
          }
        } 
        else 
        {
          if(Debug()>=2) if(mPrint) printf("No calibration table for detector %i -> %s\n", i, detname[i].Data());
          assert(0);
        }

        if (controlTable->keyDB[i] >= 2) 
        {
	        tableName = detname[i] + "Ped";
          ped = calibDB->Find(tableName.Data());
          if (ped) 
          {
            if(i<BPRS) 
            {
              pedEmc = (St_emcPed*)ped;
              pedEmcRec = pedEmc->GetTable();
            }
            else       
            {
              pedSmd = (St_smdPed*)ped;
              pedSmdRec = pedSmd->GetTable();
            }
          }  
          else 
          {
            if(mPrint) printf("No pedestal table for detector %i -> %s\n", i, detname[i].Data());
            assert(0);
          }
        }
        else 
        {
	        if(GetEventNumber() <=1 ) if(mPrint) gMessMgr->Info()<<"No pedestal DB for detector "<<i+1<< " keyDB "<< (Int_t)controlTable->keyDB[i]<< endm;
        }
      }

      for(m=0; m<mEmcMcHits[i]->numberOfModules(); m++)
      {
        mf = m + 1; // m - C style index; mf - Fortran style index !!!!
        const StMcEmcModuleHitCollection* module = mEmcMcHits[i]->module(mf);
        const ULong_t nhm = module->numberOfHits();
        if(nhm>0)
        {
          rawHit.module = mf;
          const StSPtrVecMcCalorimeterHit hits = module->hits(); 
          for(UInt_t ihm=0; ihm<nhm; ihm++)
          {
            eta = hits[ihm]->eta();
            sub = hits[ihm]->sub();
            de  = hits[ihm]->dE();

            if(mGeom[i]->getEta(mf, eta, rEta) == 0)  // (m => mf) 15-mar-2001
            {
              if(status && (calib || calibSmd || ped)) // use DB
              {
		            if(mGeom[i]->getId(mf,eta,sub, cellID) == 0) 
                {
                  cellInd  = cellID - 1; // C++ index
                  if(i<BPRS) statusALL = Int_t(statusEmcRec->Status[cellInd]);
                  else       statusALL = Int_t(statusSmdRec->Status[cellInd]);
                  
                  if(statusALL!=1) continue;               // bad common status - skip this hit

                  if(i<BPRS) calCoef  = calibRec[0].AdcToE[cellInd][1]; // AdcToE[0] - discard now
                  else calCoef = calibSmdRec[0].AdcToE[cellInd][1];
                  
                  if(Debug()>=2) if(mPrint) printf("det %i cellID %4i m %3i eta %3i sub %2i AdcToE[0][0] %f AdcToE[1][0] %f \n",
		                                    i+1, cellID, mf, eta, sub, calibRec->AdcToE[0][0], calibRec->AdcToE[1][0]); //VP
                  if(calCoef < 1.e-7) 
                  {
		                if(mPrint) printf("det %i cellID %4i m %3i eta %3i sub %2i c %f \n", i+1,cellID,mf,eta,sub,calCoef);
                  }
                  if(ped) 
                  {
		                pedType = 1;      // gauss dustribution
                    if(i<BPRS) 
                    {
                      pedMean   = pedEmcRec[0].AdcPedestal[cellInd]; 
                      pedRMS    = pedEmcRec[0].AdcPedestalRMS[cellInd]; 
                    } 
                    else 
                    {
                      pedMean   = pedSmdRec[0].AdcPedestal[cellInd][0]; 
                      pedRMS    = pedSmdRec[0].AdcPedestalRMS[cellInd][0]; 
                    }
		              } else pedType = 0; // no pedestal
                  mSimulator[i]->setParameters(calCoef, pedType, pedMean, pedRMS);
		            } else continue; // skip this hit
	            } else if(mPrint) gMessMgr->Warning()<<"StEmcSimulatorMaker::makeAllRawHitsForBemc() => no DB "
		                                    <<"det "<<i+1<<"status "<<status<<" calib "<<calib<<" ped "<<ped<<endm;

              adc    = mSimulator[i]->getAdc((Double_t)de, (Double_t)rEta);
              if(adc>0) // Zero suppression
              {
                energy = mSimulator[i]->getEnergy();
                rawHit.eta    = eta;
                rawHit.sub    = sub;
                rawHit.adc    = adc;
                rawHit.energy = energy;
                mEmcRawHits[i]->AddAt(&rawHit);
              }
            } else if(mPrint) gMessMgr->Warning()<<"StEmcSimulatorMaker::makeAllRawHitsForBemc() Bad m "
				                              <<m<<" or eta "<<eta <<endm; 
          } // for(UInt_t ihm=0; ihm<nhm; ihm++)
        }
      }
    } else if(mPrint) gMessMgr->Warning()<<"StEmcSimulatorMaker -> no hits for detector " << i + 1 << endm;; 
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

TDataSet* StEmcSimulatorMaker::getStatus(const Int_t ind, TDataSet* statusDB)
{ // service routine 
  if(!statusDB || ind<0 || ind>3) return 0;
  TString   tableName = detname[ind]+"Status";
  TDataSet* status    = statusDB->Find(tableName.Data());
  return status;
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
      for(Int_t mh=0; mh<(Int_t)hits.size(); mh++){
        hMC = hits[mh];
        if(m==hMC->module() && e==hMC->eta() && s==hMC->sub()) {
          mhDiffDe[det-1]->Fill(de-hMC->dE());
          goto ENDCYCLE;
        }
      }
      if(mPrint) printf("<W> Did not find New hit for OLD !!!\n");      
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

void StEmcSimulatorMaker::printSimulator(Int_t det)
{
  Int_t ind1=det-1, ind2 = det-1;
  if(det==0) { // all detector
    ind1 = 0;
    ind2 = 3;
  }
  if(ind1<0) ind1 = 0;
  if(ind2>3) ind2 = 3;
  for(Int_t ind=ind1; ind<=ind2; ind++){
    Int_t det = ind + 1;
    if(mSimulator[ind]) {
      mSimulator[ind]->print(); 
    } else {
      printf("Simulator for detector %1i undefined\n", det);
    }
  }
}

void StEmcSimulatorMaker::printStatusTable(Int_t det, Int_t hist)
{  
  Int_t ind=det-1, detID, m, e, s, cellIn=0, cellOut=0;
  Float_t eta, phi;
  if(ind<0) ind = 0;
  if(ind>3) ind = 3;
  TH2F* hIdStatus=0;

  status   = getStatus(ind, statusDB);
  if(hist) {
     hIdStatus = (TH2F*)m_hits[ind]->Clone();
     TString name("hStatus");
     name += detname[ind];
     hIdStatus->SetName(name.Data());
     name = "status for detector " + detname[ind];
     hIdStatus->SetTitle(name.Data());
     hIdStatus->Reset();
  }
  if(ind<2) {
    statusEmc    = (St_emcStatus*)status;
    statusEmcRec = statusEmc->GetTable();
    for(Int_t i=0; i<4800; i++){
      detID = i + 1;
      if(statusEmcRec[0].Status[i]==1) {
	cellIn++;
        mGeom[ind]->getBin(detID, m, e, s);
        printf(" ID %4.4i m %2.2i e %2.2i s %1i    ", detID, m, e, s);
        if(hist){
           mGeom[ind]->getEtaPhi(detID, eta, phi);
           hIdStatus->Fill(eta, phi, 1.);
        }
        if(cellIn%3 == 0) printf("\n");
      } else {
        cellOut++;
        if(Debug() >= 10) {
           printf(" ID %4.4i -> status %i\n", detID, Int_t(statusEmcRec[0].Status[i]));
          if(cellIn%4 == 0) printf("\n");
        }
      }
    }
  } else {
    for(Int_t i=0; i<18000; i++){
      detID = i + 1;
      statusSmd    = (St_smdStatus*)status;
      statusSmdRec = statusSmd->GetTable();
      if(statusSmdRec[0].Status[i]==1) {
	cellIn++;
        mGeom[ind]->getBin(detID, m, e, s);
        if(hist){
           mGeom[ind]->getEtaPhi(detID, eta, phi);
           hIdStatus->Fill(eta, phi, 1.);
        }
        printf(" ID %4.4i m %2.2i e %3.3i s %2.2i  ", detID, m, e, s);
        if(cellIn%4 == 0) printf("\n");
      } else {
        cellOut++;
        if(Debug() >= 10) {
          printf(" ID %4.4i -> status %i ", detID, Int_t(statusEmcRec[0].Status[i]));
          if(cellIn%4 == 0) printf("\n");
        }
      }
    }
  }
  printf("\n Table name %s : DB: date %i : time %i \n", 
  status->GetName(), controlTable->dateDB, controlTable->timeDB);
  printf("Detector %i : cells(in) %i : cells(out) %i -> sum %i \n", det, cellIn, cellOut, cellIn+cellOut);
  if(Debug()>=2) {
     controlMaker->Print(0,1);
     //     cout<<"control table " << (*controlTable) << endl;
  }
  if(hist) hIdStatus->Draw();
}

//////////////////////////////////////////////////////////////////////////
// $Id: StEmcSimulatorMaker.cxx,v 1.26 2004/04/09 21:33:53 perev Exp $
// $Log: StEmcSimulatorMaker.cxx,v $
// Revision 1.26  2004/04/09 21:33:53  perev
// Cleanup. destructor of maker more deleting
//
// Revision 1.25  2004/04/08 21:35:12  perev
// Leak off
//
// Revision 1.24  2003/10/01 00:43:16  pavlinov
// Change searching order for Geant hits
//
// Revision 1.23  2003/09/30 01:28:49  jeromel
// Undo correction until logic reshape
//
// Revision 1.22  2003/09/28 03:06:01  jeromel
// restored leak_assign (logic needs to be modified to get rid of it)
//
// Revision 1.21  2003/09/28 01:57:55  jeromel
// LEAK_SCOPE and LEAK_ASSIGN removed
//
// Revision 1.20  2003/09/23 15:19:52  suaide
// fixed bugs and modifications for embedding
//
// Revision 1.18  2003/09/02 17:58:00  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.17  2003/04/30 20:36:47  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.16  2003/01/23 03:09:02  jeromel
// Include modif
//
// Revision 1.15  2003/01/17 21:21:28  suaide
// small bug fixed to compile on Solaris
//
// Revision 1.14  2003/01/17 00:44:20  suaide
// Added new EMC database scheme
//
// Revision 1.13  2002/09/17 18:37:01  pavlinov
// mDbMaker was zero
//
// Revision 1.12  2002/09/16 22:14:50  pavlinov
// No DB for EMC before 24-09-2001
//
// Revision 1.11  2002/09/10 16:51:32  pavlinov
// Discard line with mDbMaker->SetDateTime
//
// Revision 1.10  2002/06/04 16:09:36  pavlinov
// added option with DB(pedestal ans calibration  coefficients
//
// Revision 1.9  2002/06/03 23:35:10  pavlinov
// Last correction without DB for ped and calib. coeff.
//
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
