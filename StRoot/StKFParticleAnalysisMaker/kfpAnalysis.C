/*
  FPE_OFF
  root.exe -q -b -x kfpAnalysis.C
*/
class StGoodTrigger;
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "/net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2016") {
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "st_physics_adc_17125034_raw_1000007.femtoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2016") {
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "/star/data01/pwg_tasks/picoDs/*picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2011") {
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "./st_mtd_19110005_raw_3500041.picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2018") {
void kfpAnalysis(Int_t N = 100000, const Char_t *input = "/gpfs01/star/pwg_tasks/tfg02/2010/11GeV/*picoDst.root", const Char_t *output = "picoAna2011AuAu11.root", const Char_t *triggerSet = "y2011") {
//void kfpAnalysis(Int_t N = 1000, const Char_t *input = "/gpfs01/star/pwg_tasks/tfg02/2010/11GeV/st_physics_11148001_raw_1010001.picoDst.root", const Char_t *output = "picoAna2011AuAu11.root", const Char_t *triggerSet = "y2011") {
//void kfpAnalysis(Int_t N = 1000, const Char_t *input = "/gpfs01/star/pwg/fisyak/Pico/2010AuAu11/11148001.picoDst.root", const Char_t *output = "picoAna2011AuAu11.root", const Char_t *triggerSet = "y2011") {
#if 0
void kfpAnalysis(Int_t N = 1000, 
		 //		 const Char_t *input = "/star/rcf/test/dev/daq_sl302.stica/Wed/year_2014/AuAu200_production_low_2014/st_physics_15164004_raw_2000022.MuDst.root",
		 //		 const Char_t *input = "/star/data75/reco/27GeV_production_2018/FullField/P18ih/2018/158/19158009/st_physics_19158009_raw_3000013.MuDst.root",
		 //		 const Char_t *input = "root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/AuAu27_production_2011/FullField/P11id/2011/172/12172024:st_physics_12172024_raw_4010001.MuDst.root",
		 const Char_t *output = "picoAna2011AuAu27.root", const Char_t *triggerSet = "y2014") {
#endif
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  Bool_t iMuDst = kFALSE;
  if (TString(input).Contains("MuDst")) iMuDst = kTRUE;
  gROOT->LoadMacro("lMuDst.C");
  TString Chain("r");
  Chain += triggerSet;
  if (iMuDst) Chain += ",RMuDst";
  else        Chain += ",RpicoDst";
  Chain += ",kfpAna,mysql,nodefault,quiet";
  //  lMuDst(0,input,"ry2016,RpicoDst,mysql,PicoAnalysis,quiet,nodefault",output);
  lMuDst(0,input,Chain,output);
  TTree *tree = 0;
  if (iMuDst) {
    StKFParticleAnalysisMaker *ana = ( StKFParticleAnalysisMaker *) chain->Maker("KFParticleAnalysis");
    if (! ana) return;
    ana->AnalyseMuDst();
    StMuDstMaker * MuMk = (StMuDstMaker *) StMaker::GetTopChain()->Maker("MuDst");
    if (! MuMk) return;
    MuMk->SetStatus("*",1);
    tree = MuMk->chain();
  } else {
    StPicoDstMaker * picoMk = (StPicoDstMaker *) StMaker::GetTopChain()->Maker("PicoDst");
    if (! picoMk) return;
    picoMk->SetStatus("*",1);
    tree = picoMk->chain();
  }
  if (! tree ) {
    cout << "No MuDst/PicoDst tree. Exit." << endl;
  }
  Long64_t nentries = tree->GetEntries();
  cout << "no. events in tree " <<nentries << endl;
  if (nentries <= 0) return;
  Long64_t nevent = N;
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  //  new StGoodTrigger(triggerSet);
  //  chain->SetAttr(".Privilege",1,"StPicoDstMaker::*");
  chain->EventLoop(nevent);
#endif
  
}
