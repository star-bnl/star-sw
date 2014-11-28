/*
  root.exe -q -b 'makeAvLaserHits.C(0)'
  root.exe -q -b 'makeAvLaserHits.C(1)'
  root.exe -q -b 'makeAvLaserHits.C(2)'
  root.exe -q -b 'makeAvLaserHits.C(3)'
  root.exe -q -b 'makeAvLaserHits.C(4)'
  root.exe -q -b 'makeAvLaserHits.C(5)'
*/

class StLaserAvClusterMaker;
void makeAvLaserHits( const Char_t *rootFile) {
  // "st_laser_adc_14046081_raw.F.Fit.g1.LandauIFreQ.AllN.root"
  // "st_laser_adc_14072106_raw.RF.Fit.g1.LandauIFreQ.AllN.root"
  gROOT->LoadMacro("bfc.C");
  //  TString Chain("MakeEvent,LaserAvCl,Evout,TpcHitMover,CorrX,LaserIT,analysis,noHistos,noRunco,NoDefault");
  TString Chain("MakeEvent,LaserAvCl,Evout,TpcHitMover,CorrX,LaserIT,analysis,noHistos,noRunco,,KFVertex,Lana,Analysis,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco,NoTags,StiCA");
  TString RootFile(rootFile);
  TString TagFile(RootFile);
  TagFile.ReplaceAll(".root",".tags.root");
  chain = bfc(-1,Chain.Data(),0,RootFile.Data(),TagFile.Data());
  StLaserAvClusterMaker *avL = (StLaserAvClusterMaker *) chain->Maker("LaserAvEvent");
  avL->SetInputFile(rootFile);
  StMaker::lsMakers(chain);
  chain->Init();
  St_trgTimeOffsetC::instance()->SetLaser(kTRUE);
#if 1
  for (Int_t iev = 0; iev < 1000; iev++) {
    avL->SetToken(iev);
    if (chain->MakeEvent()) {
      if (iev > 0)  break;
      continue;
    }
    StAnalysisMaker::PrintTpcHits();
    if (chain->GetTFile()) {
      chain->GetTFile()->cd();
      StAnalysisMaker::PrintTpcHits(0,0,1);
    }
  }
#endif
}
void makeAvLaserHits(Int_t kase = 1) {
#ifdef __y2013__
  const Int_t NK = 5;
  const Char_t *laserFiles[5] = {
    "st_laser_adc_14044071_raw.ZF.Fit.g3.LandauIFreQ.SecAll.root",  // ZF
    "st_laser_adc_14046081_raw.F.Fit.g3.LandauIFreQ.SecAll.root",   // FF
    "st_laser_adc_14072106_raw.RF.Fit.g3.LandauIFreQ.SecAll.root",  // RF
    "st_laser_adc_14158028.raw.RF.Fit.g3.LandauIFreQ.SecAllB.root", // RC
    "st_laser_adc_14161023.raw.ZF.Fit.g3.LandauIFreQ.SecAllB.root"};// ZC
#else
  const Int_t NK = 6;
  const Char_t *laserFiles[6] = {
    "15040047.ClnoW.Fit.g3.LangauIFreQ.All.root", // FF  20k
    "15040049.ClnoW.Fit.g3.LangauIFreQ.All.root", // ZF	 20k
    "15050192.ClnoW.Fit.g3.LangauIFreQ.All.root", // ZF	  7k
    "15050194.ClnoW.Fit.g3.LangauIFreQ.All.root", // ZR	 14k
    "15051026.ClnoW.Fit.g3.LangauIFreQ.All.root", // RF	  6k
    "15051088.ClnoW.Fit.g3.LangauIFreQ.All.root"};// RF	 20k
#endif
  if (kase >= 0 && kase < NK) makeAvLaserHits(laserFiles[kase]);
}
