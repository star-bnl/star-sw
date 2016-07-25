void AdcNorms(TChain *TpcT) {
  // root.exe *0.root 'Chain.C("TpcT")' HardWarePosition.C+ 'AdcNorms.C(chain)'
  if (! TpcT) return;
  TCanvas *c1 = new TCanvas("Adc","Adc");
  //  gROOT->ProcessLine(".L HardWarePosition.C+");
  c1->Divide(2,2);
  const Char_t *Names[2] = {"Inner","Outer"};
  for (Int_t i = 0; i < 2; i++) {
    TString plot(Form("fRcHit.mCharge:fAdcSum>>%sQvsAdc",Names[i]));
    TString cut("fAdcSum<2e3&&fRcHit.mCharge<5e-5&&padrow(fRcHit.mHardwarePosition)");
    if (i == 0) cut += "<13";
    else        cut += ">13";
    c1->cd(2*i+1)->SetLogz(1);
    TpcT->Draw(plot,cut,"colz");
    TH2* QvsAdc = (TH2 *) gDirectory->Get(Form("%sQvsAdc",Names[i]));
    QvsAdc->ProfileX()->Draw();
    TProfile *QvsAdc_pfx = (TProfile *) gDirectory->Get(Form("%sQvsAdc_pfx",Names[i]));
    QvsAdc_pfx->SetMarkerStyle(20);
    QvsAdc_pfx->Fit("pol1","er","",400,2000);
    QvsAdc->Draw("colz");
    QvsAdc_pfx->Draw("sames");
    c1->cd(2*i+2);
    QvsAdc->ProjectionX()->Draw();
  }
}
