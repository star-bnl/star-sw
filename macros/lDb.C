void lDb(const Char_t *date = 0) {
  gROOT->LoadMacro("bfc.C");
  TString Chain("mysql,tpcDb,detDb,magF,TpcHitMover,CorrX,LaserIT,nodefault");
  if (date) {Chain += ","; Chain += date;}
  bfc(-1,Chain,0,0,0);
}
