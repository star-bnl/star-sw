class StResidualMaker;
StResidualMaker* rm;

void ReadBack(char* FileToRead="ResidualMakerHistos.root"){
  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StAnalysisUtilities");  // needed by V0dstMaker
  gSystem->Load("StMagF");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StResidualMaker");


  cout << "Dynamic loading done" << endl;

  rm = new StResidualMaker;
  // unnecessary now  rm->Init();  // gotta do this to instantiate the Sigma histograms!!! (segfault when trying to fill them otherwise!)
  rm->readHistoFile(FileToRead);  

}

  
