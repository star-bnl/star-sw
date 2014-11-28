TH2 *h1fast, *h1full;
TH1 *h2fast, *h2full, *h3fast, *h3full;
TCanvas *c;
TLegend *leg;

void testPmtSignal(float pmtGain=1.5e+6, float cathodeNoise=0.0, float dynodeNoise=0.0) {
    gROOT->Macro("LoadLogger.C");
    gSystem->Load("StChain");
    gSystem->Load("StMcEvent");
    gSystem->Load("StEmcSimulatorMaker");
    cout << "loaded libraries" << endl;
    
    gStyle->SetOptStat(0);
    
    StPmtSignal p(pmtGain, cathodeNoise, dynodeNoise);
    int maxAdc                  = 4096;
    float maxEnergy             = 60.0;
    float photoElectronsPerMIP  = 63.0;
    float energyPerMIP          = 0.0198;
    float samplingFraction      = 14.1;
    float totalGain = (maxAdc / maxEnergy) * (energyPerMIP / photoElectronsPerMIP) * samplingFraction;
    p.setTotalGain(totalGain); // ADC / photoElectron
    p.setPedestalMean(30.0);
    p.setPedestalRMS(1.5);
    
    h1fast = new TH2D("h1fast","<ADC> / photoElectrons",50, 0.0, 16000.0, 50, 0.0, 4000.0);
    h1full = new TH2D("h1full","<ADC> / photoElectrons",50, 0.0, 16000.0, 50, 0.0, 4000.0);
    
    h2fast = new TH1D("h2fast","ADC distributions for 400 photoElectrons",50, 125.0, 175.0);
    h2full = new TH1D("h2full","ADC distributions for 400 photoElectrons",50, 125.0, 175.0);
    
    h3fast = new TH1D("h3fast","ADC distributions for 8000 photoElectrons",50, 2350.0, 2550.0);
    h3full = new TH1D("h3full","ADC distributions for 8000 photoElectrons",50, 2350.0, 2550.0);
    
    for(int i=0; i<16000; i++) { 
        if(i%1000 == 0) cout << "processing " << i << endl;
        h1fast->Fill(i, p.getAdc(i, StPmtSignal::kFastSimulator));
        h1full->Fill(i, p.getAdc(i, StPmtSignal::kFullSimulator));
        
        h2fast->Fill(p.getAdc(400, StPmtSignal::kFastSimulator));
        h2full->Fill(p.getAdc(400, StPmtSignal::kFullSimulator));
        
        h3fast->Fill(p.getAdc(8000, StPmtSignal::kFastSimulator));
        h3full->Fill(p.getAdc(8000, StPmtSignal::kFullSimulator));
    }
    
    h1fast->SetXTitle("nPhotoElectrons");
    h1fast->SetYTitle("ADC");
    
    h2fast->SetXTitle("ADC");
    h3fast->SetXTitle("ADC");
    
    h1full->SetLineColor(kRed);
    h2full->SetLineColor(kRed);
    h3full->SetLineColor(kRed);
    
    c = new TCanvas("c","",900,300);
    c->Divide(3,1);
    
    c->cd(1);
    h1fast->Draw("box");
    h1full->Draw("box same");
    
    leg = new TLegend(0.1,0.7,0.5,0.9);
    leg->AddEntry(h1fast,"fastSimulator");
    leg->AddEntry(h1full,"fullSimulator");
    leg->Draw();
    
    TVirtualPad * pad = c->cd(2);
    pad->SetLogy();
    h2fast->Draw();
    h2full->Draw("same");
    
    pad = c->cd(3);
    pad->SetLogy();
    h3fast->Draw();
    h3full->Draw("same");
}