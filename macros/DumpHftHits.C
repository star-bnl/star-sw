/*
  root.exe -q -b -x DumpHftHits.C >& DumpHftHits.log &
  Int_t det = 1; 
  HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x)>>al(90,-180,180)",Form("det==%i",det),"");
  HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x)>>us(90,-180,180)",Form("det==%i&&us>0",det),"");
  al->Fit("pol0");
  TEfficiency *eff = new TEfficiency(*us,*al);
  eff->Draw();
 HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x):z>>az(100,-10,10,90,-180,180)",Form("x*x+y*y>25&&det==%i",det),"colz");
 HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x):z>>az(100,-10,10,90,-180,180)","x*x+y*y>25&&det==1","colz");

gStyle->SetOptStat(0)
 HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x):z>>azI(100,-10,10,90,-180,180)","x*x+y*y<25&&det==1","colz");
 HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x):z>>azO(100,-10,10,90,-180,180)","x*x+y*y>25&&det==1","colz");

 HftHit->Draw("TMath::Floor(volumeID/1e6)>>Sector(10,0.5,10.5)","det==1","colz")
 HftHit->Draw("TMath::Floor((volumeID/1e4)%100)>>Ladder(4,0.5,4.5)","det==1","colz")
 HftHit->Draw("TMath::Floor((volumeID/1e2)%100)>>Sensor(10,0.5,10.5)","det==1","colz")

 HftHit->Draw("TMath::Floor((volumeID/1e2)%100):4*(TMath::Floor(volumeID/1e6)-1)+TMath::Floor((volumeID/1e4)%100)-1>>SvsSL(40,-0.5,39.5,10,0.5,10.5)","det==1","colz")
 */
class StBFChain;
StBFChain *chain = 0;
void DumpHftHits(Int_t noEvents = 1000000, const Char_t *InFile = "./*event.root", const Char_t *OutFile = "Hft.root") {
  gROOT->LoadMacro("bfc.C");
  TString Chain("in,analysis,detDb,mysql,NoDefault");
  chain = bfc(-1,Chain.Data(),InFile,0,OutFile);
  StMaker *mk = chain->Maker("detDb");
  mk->SetActive(kFALSE);
  for (Int_t i = 0; i < noEvents; i++) {
    if (chain->MakeEvent()) break;
    StAnalysisMaker::instance()->DumpHftHits();
  }
}
//________________________________________________________________________________
TH1F *Efficiency(Int_t det=1, Int_t color=1) {
  TNtuple *HftHit = (TNtuple *) gDirectory->Get("HftHit");
  if (! HftHit) return 0;
  TH1F *al = new TH1F(Form("al%i",det),"al",90,-180,180); al->Sumw2();
  TH1F *us = new TH1F(Form("us%i",det),"us",90,-180,180); us->Sumw2();
  HftHit->Draw(Form("TMath::RadToDeg()*TMath::ATan2(y,x)>>al%i",det),Form("det==%i",det),"goff");
  HftHit->Draw(Form("TMath::RadToDeg()*TMath::ATan2(y,x)>>us%i",det),Form("det==%i&&us>0",det),"goff");
  TH1F *ef = new TH1F(Form("ef%i",det),"ef",90,-180,180); ef->Sumw2(); ef->SetMarkerColor(color);
  if (det == 1) ef->SetTitle("Pixel Efficiency");
  else if (det == 2) ef->SetTitle("Ist Efficiency");
  else if (det == 3) ef->SetTitle("Sst Efficiency");
  ef->SetXTitle("#phi ^{o}");
  ef->Divide(us,al,1,1,"b");
  ef->SetStats(0);
  return ef;
}
//________________________________________________________________________________
void Draw(TFile *f0, TFile *f1, Int_t det = 1) {
  if (! f0 || ! f1) return;
  TString t0(f0->GetName()); t0.ReplaceAll(".root","");
  TString t1(f1->GetName()); t1.ReplaceAll(".root","");
  f0->cd(); TH1F *e0 = Efficiency(det);
  f1->cd(); TH1F *e1 = Efficiency(det,2);
  TLegend *l = new TLegend(0.6,0.2,0.8,0.4);
  l->AddEntry(e0,t0);
  l->AddEntry(e1,t1);
  TCanvas *c = new TCanvas(Form("c%i",det),Form("c%i",det));
  e0->SetMinimum(0);
  e0->SetMaximum(1);
  e0->Draw();
  e1->Draw("same");
  l->Draw();
}
#if 0
Double_t sector(Double_t phi) {
  Double_t i = (phi - 72)/36 + 1;
  if (i < 0) i ;
}
#endif
