void loadPythiaLibs(){

  gSystem->Load("libEG");
  gSystem->Load("libEGPythia6");
  gSystem->Load("$OPTSTAR/alt/lib/libPythia6.so");    
  gSystem->Load("StEStructPoolEventGenerators.so");

};
