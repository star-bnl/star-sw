void pulserReader(Int_t nevents=100, const Char_t *input = "st_pulser_5341024_raw_1090001.daq") {
  gROOT->LoadMacro("bfc.C"); 
  TString Chain("in daq db detDb NoDefault");  
  bfc(-1,Chain.Data(),input,0,0);
  gROOT->LoadMacro("tpcReader.C+");
  chain->Init();
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && !istat) {
   chain->Clear();
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
   if (istat) break;
   istat = tpcReader();
   if (istat) break;
   iev++; goto EventLoop;
 } // Event Loop
  Finish();
}
