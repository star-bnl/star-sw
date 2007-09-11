TH1 *btow_adc, *bprs_adc, *smde_adc, *smdp_adc;
TH1 *btow_energy, *bprs_energy, *smde_energy, *smdp_energy;
TCanvas *c;

void testSimulatorMaker(int nEvents=1000, const char *geantFile = "/star/u/kocolosk/tmp01/photon_5_7_09.geant.root") {
    gROOT->Macro("LoadLogger.C");
    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StMcEvent"); 
    gSystem->Load("StMcEventMaker"); 
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StMcAnalysisMaker");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcSimulatorMaker");
    
    StChain *chain = new StChain("StChain");
    
    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(geantFile);
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");             //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch

    StMcEventMaker* mcEventMaker = new StMcEventMaker();
    
    St_db_Maker* dbMaker = new St_db_Maker("StarDb","MySQL:StarDb","$STAR/StarDb");
    
    //pick up average status table in 2005
    dbMaker->SetDateTime(20050506,214129);
    
    //if you want to use ideal DB values for some detector
    dbMaker->SetFlavor("sim","bprsCalib");
    
    StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker();
        
    //emcSim->setCheckStatus(kBarrelEmcTowerId, false);
    
    //emcSim->setDoZeroSuppression(kBarrelEmcTowerId, false);
    
    //emcSim->setSimulatorMode(kBarrelEmcTowerId, StEmcVirtualSimulator::kTestMode);
    //emcSim->setSimulatorMode(kBarrelEmcPreShowerId, StEmcVirtualSimulator::kTestMode);
    //emcSim->setSimulatorMode(kBarrelSmdEtaStripId, StEmcVirtualSimulator::kTestMode);
    //emcSim->setSimulatorMode(kBarrelSmdPhiStripId, StEmcVirtualSimulator::kTestMode);
    
    emcSim->setMakeFullDetector(kBarrelEmcTowerId, false);
    emcSim->setMakeFullDetector(kBarrelEmcPreShowerId, false);
    emcSim->setMakeFullDetector(kBarrelSmdEtaStripId, false);
    emcSim->setMakeFullDetector(kBarrelSmdPhiStripId, false);
    
    chain->Init();
    
    /*controlEmcSimulatorMaker_st* simControl = emcSim->getControlSimulator()->GetTable();
    simControl->keyDB[0] = 2;
    simControl->keyDB[1] = 2;
    simControl->keyDB[2] = 2;
    simControl->keyDB[3] = 2;
    simControl->makeFullDet[0] = 0;
    simControl->makeFullDet[1] = 0;
    simControl->makeFullDet[2] = 0;
    simControl->makeFullDet[3] = 0;
    simControl->pedCutOff[2] = 1.5;
    simControl->pedCutOff[3] = 1.5;*/
    
    int i=0;
    
    btow_adc = new TH1F("btow_adc","",200,0.,400.);
    btow_energy = new TH1F("btow_energy","",200,0.,4.);
    
    bprs_adc = new TH1F("bprs_adc","",200,0.,200.);
    bprs_energy = new TH1F("bprs_energy","",200,0.,4.);
    
    smde_adc = new TH1F("smde_adc","",200,0.,200.);
    smde_energy = new TH1F("smde_energy","",200,0.,4.);
    
    smdp_adc = new TH1F("smdp_adc","",200,0.,200.);
    smdp_energy = new TH1F("smdp_energy","",200,0.,4.);
    
    while(i<nEvents && chain->Make() == kStOk) {
        StEmcCollection *coll = emcSim->getEmcCollection();
        
        StEmcDetector *btow = coll->detector(kBarrelEmcTowerId);
        StEmcDetector *bprs = coll->detector(kBarrelEmcPreShowerId);
        StEmcDetector *smde = coll->detector(kBarrelSmdEtaStripId);
        StEmcDetector *smdp = coll->detector(kBarrelSmdPhiStripId);
        
        for(int i=1; i<=120; i++) {
            StSPtrVecEmcRawHit btow_hits = btow->module(i)->hits();
            StSPtrVecEmcRawHit bprs_hits = bprs->module(i)->hits();
            StSPtrVecEmcRawHit smde_hits = smde->module(i)->hits();
            StSPtrVecEmcRawHit smdp_hits = smdp->module(i)->hits();
            
            for(int j=0; j<btow_hits.size(); j++) {
                btow_adc->Fill( (btow_hits[j])->adc() );
                btow_energy->Fill( (btow_hits[j])->energy() );
            }
            
            for(int j=0; j<bprs_hits.size(); j++) {
                bprs_adc->Fill( (bprs_hits[j])->adc() );
                bprs_energy->Fill( (bprs_hits[j])->energy() );
            }
            
            for(int j=0; j<smde_hits.size(); j++) {
                smde_adc->Fill( (smde_hits[j])->adc() );
                smde_energy->Fill( (smde_hits[j])->energy() );
            }
            
            for(int j=0; j<smdp_hits.size(); j++) {
                smdp_adc->Fill( (smdp_hits[j])->adc() );
                smdp_energy->Fill( (smdp_hits[j])->energy() );
            }
        }
        
        i++; chain->Clear();
    }
    
    chain->ls(3);
    chain->Finish();
    
    c = new TCanvas("c","",600,800);
    c->Divide(2,4);
    
    TVirtualPad *pad = c->cd(1);
    btow_adc->Draw();
    
    pad = c->cd(2);
    pad->SetLogy();
    btow_energy->Draw();
    
    c->cd(3);
    bprs_adc->Draw();
    
    pad = c->cd(4);
    pad->SetLogy();
    bprs_energy->Draw();
    
    pad = c->cd(5);
    smde_adc->Draw();
    
    pad = c->cd(6);
    pad->SetLogy();
    smde_energy->Draw();
    
    c->cd(7);
    smdp_adc->Draw();
    
    pad = c->cd(8);
    pad->SetLogy();
    smdp_energy->Draw();
}