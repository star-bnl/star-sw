/*
  root.exe -q -b -x DumpHftHits.C >& DumpHftHits.log &
  Int_t det = 3; 
  HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x)>>al(90,-180,180)",Form("det==%i",det),"");
  HftHit->Draw("TMath::RadToDeg()*TMath::ATan2(y,x)>>us(90,-180,180)",Form("det==%i&&us>0",det),"");
  al->Fit("pol0");
  TEfficiency *eff = new TEfficiency(*us,*al);
  eff->Fit(pol0);
  eff->Draw();
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
