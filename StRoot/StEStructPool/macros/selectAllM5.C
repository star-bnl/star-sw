void selectAllM5(const char* dirName, const char* inFile){

  // -- example for recombining histograms by selection in new root file 
  //
  // root.exe -q -b selectAllM5.C("dirContainingFinal.rootFile","inFile")
  //

  TString dir(dirName);
  dir+="/";
  dir+=inFile;
  dir+=".root";
  gSystem->Load("StEStructPoolSupport.so");

  StEStructHAdd adder;

  TFile * tf=new TFile(dir.Data());

  if(!tf){
    cout<<"error opening file "<<endl;
    return ;
  };

  //--> do all of the following
  const char* oname[]={"pi_pi", "pi_K", "pi_p", "K_K", "K_p", "p_p", "o_o",};

  int nList[14];
  for (int i=0;i<14;i++) {
      nList[i] = i;
  }
  TString fname(dirName);
  fname+="/";
  fname+=inFile;
  fname+="all";
  fname+=".root";
  adder.addCuts(fname.Data(),tf,nList,14);

  for(int k=0;k<7;k++){
    nList[0] = 2*k;
    nList[1] = 2*k + 1;
    TString fname(dirName);
    fname+="/";
    fname+=inFile;
    fname+=oname[k];
    fname+=".root";
    adder.addCuts(fname.Data(),tf,nList,2);
  }

};


