/*
  root.exe -q -b -x DumpHftHits.C >& DumpHftHits.log &
  Int_t det = 1; 
  HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x)>>al(90,-180,180)",Form("det==%i",det),"");
  HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x)>>us(90,-180,180)",Form("det==%i&&us>0",det),"");
  al->Fit("pol0");
  TEfficiency *eff = new TEfficiency(*us,*al);
  eff->Fit(pol0);
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
