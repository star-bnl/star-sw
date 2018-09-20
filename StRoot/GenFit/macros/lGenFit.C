//#include "garfield/Include/Plotting.hh"

void lGenFit() {
  std::cout << "Welcome to GenFit++" << std::endl;
  gSystem->Load("libEve");
  gSystem->Load("libGeom");
  gSystem->Load("libGenFit");
#if 0
  const Char_t *subdirs[] = {"core", "eventDisplay", "fields", "finitePlanes", "fitters", "GBL", "GFRave", "measurements", "trackReps", "utilities", 0};
  Int_t i = 0;
  while (subdirs[i]) {
    TString path("-IStRoot/GenFit/");
    path += subdirs[i];
    path += "/include";
    gSystem->AddIncludePath(path);
    i++;
  }
#endif
  gSystem->AddIncludePath("-IStRoot/GenFit -I$STAR/StRoot/GenFit -I$XOPTSTAR/include/eigen3");
}
