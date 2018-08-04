/*
  FPE_OFF
  root.exe -q -b -x kfpAnalysis.C
*/
class StGoodTrigger;
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "/net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2016") {
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "st_physics_adc_17125034_raw_1000007.femtoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2016") {
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "/star/data01/pwg_tasks/picoDs/*picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2011") {
void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "/star/data01/pwg_tasks/picoDst/st_physics_12126105_raw_3020002.picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2011") {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  //  lMuDst(0,input,"ry2016,RpicoDst,mysql,PicoAnalysis,quiet,nodefault",output);
  lMuDst(0,input,"r" triggerSet ",RpicoDst,kfpAna,mysql,nodefault,quiet",output);
  maker = (StPicoDstMaker *) StMaker::GetTopChain()->Maker("PicoDst");
  if (! maker) return;
  maker->SetStatus("*",1);
  TChain *tree = maker->chain();
  Long64_t nentries = tree->GetEntries();
  if (nentries <= 0) return;
  Long64_t nevent = N;
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  new StGoodTrigger(triggerSet);
  chain->SetAttr(".Privilege",1,"StPicoDstMaker::*")
  chain->EventLoop(nevent);
#endif
  
}
