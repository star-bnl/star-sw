void hijing( int nevents=100,int rngSeed=0, const char* tag="y2020", Int_t RunG = 1 ) {
   gROOT->LoadMacro("starsim.hijing.C");
   starsim(nevents,rngSeed,tag, RunG);
}
