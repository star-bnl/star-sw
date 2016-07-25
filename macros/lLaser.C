void lLaser() {
  gROOT->LoadMacro("bfc.C");
  TString Chain("noinput,tpcDb,magF,TpcHitMover,lana,CorrX,LaserIT,nodefault");
  bfc(-1,Chain,0,0,0);
  
}
