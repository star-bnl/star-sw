TH1 *btow_adc, *bprs_adc, *smde_adc, *smdp_adc;
TH1 *btow_energy, *bprs_energy, *smde_energy, *smdp_energy;
TCanvas *c;

void testSimulatorMaker(int nEvents=1000, const char *geantFile = "/star/institutions/mit/common/simu/photon_5_7_09.geant.root") {
    gROOT->Macro("LoadLogger.C");
    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StMcEvent"); 
    gSystem->Load("StMcEventMaker"); 
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StDaqLib");
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
    
    //emcSim->setSimulatorMode(kBarrelEmcTowerId, StEmcVirtualSimulator::kTestMode);
    //emcSim->setSimulatorMode(kBarrelEmcPreShowerId, StEmcVirtualSimulator::kTestMode);
    //emcSim->setSimulatorMode(kBarrelSmdEtaStripId, StEmcVirtualSimulator::kTestMode);
    //emcSim->setSimulatorMode(kBarrelSmdPhiStripId, StEmcVirtualSimulator::kTestMode);
    
    emcSim->setMaximumAdc(kBarrelSmdEtaStripId, 850.0);
    emcSim->setMaximumAdcSpread(kBarrelSmdEtaStripId, 15.0);
    
    emcSim->setMaximumAdc(kBarrelSmdPhiStripId, 850.0);
    emcSim->setMaximumAdcSpread(kBarrelSmdPhiStripId, 15.0);
    
    chain->Init();
    
    btow_adc = new TH1F("btow_adc","",200,0.,2000.);
    btow_energy = new TH1F("btow_energy","",200,0.,60.);
    
    bprs_adc = new TH1F("bprs_adc","",200,0.,1200.);
    bprs_energy = new TH1F("bprs_energy","",200,0.,30.);
    
    smde_adc = new TH1F("smde_adc","",200,0.,1200.);
    smde_energy = new TH1F("smde_energy","",200,0.,30.);
    
    smdp_adc = new TH1F("smdp_adc","",200,0.,1200.);
    smdp_energy = new TH1F("smdp_energy","",200,0.,30.);
    
    int i=0;
    while(i<nEvents && chain->Make() == kStOk) {
        StEmcCollection *coll = emcSim->getEmcCollection();
        
        StEmcDetector *btow = coll->detector(kBarrelEmcTowerId);
        StEmcDetector *bprs = coll->detector(kBarrelEmcPreShowerId);
        StEmcDetector *smde = coll->detector(kBarrelSmdEtaStripId);
        StEmcDetector *smdp = coll->detector(kBarrelSmdPhiStripId);
        
        for(int m=1; m<=120; m++) {
            StSPtrVecEmcRawHit btow_hits = btow->module(m)->hits();
            StSPtrVecEmcRawHit bprs_hits = bprs->module(m)->hits();
            StSPtrVecEmcRawHit smde_hits = smde->module(m)->hits();
            StSPtrVecEmcRawHit smdp_hits = smdp->module(m)->hits();
            
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
    pad->SetLogy();
    btow_adc->Draw();
    
    pad = c->cd(2);
    pad->SetLogy();
    btow_energy->Draw();
    
    pad = c->cd(3);
    pad->SetLogy();
    bprs_adc->Draw();
    
    pad = c->cd(4);
    pad->SetLogy();
    bprs_energy->Draw();
    
    pad = c->cd(5);
    pad->SetLogy();
    smde_adc->Draw();
    
    pad = c->cd(6);
    pad->SetLogy();
    smde_energy->Draw();
    
    pad = c->cd(7);
    pad->SetLogy();
    smdp_adc->Draw();
    
    pad = c->cd(8);
    pad->SetLogy();
    smdp_energy->Draw();
}
