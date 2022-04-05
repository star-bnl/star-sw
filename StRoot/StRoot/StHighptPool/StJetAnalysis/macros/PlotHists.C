#include <iostream>

void PlotHists(const char* infile, const char* outfile)
{
    TFile* ifile = new TFile(infile,"READ");

    TH2* dEtaVsdPhi = dEtaVsdPhi;
    
    TFile* ofile = new TFile(outfile,"RECREATE");
    TH1* dEta = dEtaVsdPhi->ProjectionY("dEta");
    dEta->SetLineWidth(2);
    dEta->SetXTitle("#Delta#eta");
    dEta->SetTitle("#Delta#eta, pt>2 GeV, Trigger Pt>3 GeV");
    TH1* dPhi = dEtaVsdPhi->ProjectionX("dPhi");
    dPhi->SetLineWidth(2);
    dPhi->SetXTitle("#Delta#phi");
    dPhi->SetTitle("#Delta#phi, pt>2 GeV, Trigger Pt>3 GeV");

    dEtaVsdPhi->SetTitle("#Delta#phi vs. #DeltaEta, pt>2 GeV, Trigger Pt>3 GeV");
    dEtaVsdPhi->SetXTitle("#Delta#eta");
    dEtaVsdPhi->SetYTitle("#Delta#phi");
    dEtaVsdPhi->SetLineWidth(2);

    /*
      c1 = new TCanvas();
      gStyle->SetPalette(1,0);
      c1->SetLogz();
      dEtaVsdPhi->Draw("colz");

      TCanvas* c2 = new TCanvas("c2");
      dEta->Draw();
      
      TCanvas* c3 = new TCanvas("c3");
      dPhi->Draw();
    */

    dEtaVsdPhi->Write();
    ofile->Write();
    ofile->Close();
}
