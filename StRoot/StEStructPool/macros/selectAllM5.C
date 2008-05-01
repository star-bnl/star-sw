void selectAllM5(const char* dirName, const char* inFile){

  // -- example for recombining histograms by selection in new root file 
  //
  // root.exe -q -b selectAllM5.C("dirContainingFinal.rootFile","inFile")
  //

  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
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
  const char* oname[]={"pi_o", "pi_pi", "pi_K", "pi_p",
                       "K_o",  "K_K",   "K_p",
                       "p_o",  "p_p",
                       "o_o",  "all"};
  int parentDist[10][2] = {1,0, 1,1, 1,2, 1,3,  2,0, 2,2, 2,3,  3,0, 3,3,  0,0};

  int nList[10];
  for (int k=0;k<10;k++) {
      nList[k] = k;
  }
  int nParentDist = 4;
  int parentSum[4][2] = { 0,0, 1,1, 2,2, 3,3 };
  TString fname(dirName);
  fname+="/";
  fname+=inFile;
  fname+="all";
  fname+=".root";
  int symm=1;
  adder.addCuts(fname.Data(),tf,nList,10,parentSum,nParentDist,symm);
  TFile * tfComb=new TFile(fname.Data(),"UPDATE");
  tfComb->cd();
  adder.combineUS(tfComb);
  tfComb->Close();

  tf->cd();
  nParentDist = 1;
  int parentD[1][2];
  for(int k=0;k<10;k++){
    TString fname(dirName);
    fname+="/";
    fname+=inFile;
    fname+=oname[k];
    fname+=".root";
    nList[0] = k;
    parentD[0][0] = parentDist[k][0];
    parentD[0][1] = parentDist[k][1];
    adder.addCuts(fname.Data(),tf,nList,1,parentD,nParentDist);
  }

  TString fname(dirname);
  fname+="/";
  fname+=fileBase;
  fname+="_pairDensities.root";
  adder.addDensities(fname.Data(),tf);
};


