void lHft(const Char_t *date = 0) {
  gROOT->LoadMacro("bfc.C");
  TString Chain("tpcDb,istDb,pxlDb,StEvent,detDb,HftMatTree,nodefault");
  if (date) {Chain += ","; Chain += date;}
  bfc(-1,Chain,0,0,0);
}
