void selectAllM3(const char* dirName, const char *fileBase ){

  // -- example for recombining histograms by selection in new root file 
  //
  // root.exe -q -b selectASNS.C'("dirContainingFinal.rootFile")
  //

  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  TString inFile(dirName);
  inFile+="/";
  inFile+=fileBase;
  inFile+=".root";
  gSystem->Load("StEStructPoolSupport.so");

  StEStructHAdd adder;

  TFile * tf=new TFile(inFile.Data());

  if(!tf){
    cout<<"error opening file "<<endl;
    return ;
  };
  StEStructCutBin* cb = StEStructCutBin::Instance();
  cb->setMode(3);

  //--> do all of the following
  const char* oname[]={"all","awayside","nearside","soft","softAS","softNS","neck","neckAS","neckNS","hard","hardAS","hardNS","softHard","softHardAS","softHardNS"};

  const int _map[15][16]={
      0,  1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
      0,  1, 4, 5, 8, 9,12,13, 0, 0, 0, 0, 0, 0, 0, 0,
      2,  3, 6, 7,10,11,14,15, 0, 0, 0, 0, 0, 0, 0, 0,
      0,  1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0,  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2,  3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      4,  5, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      4,  5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      6,  7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      8,  9,10,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      8,  9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      10,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      12,13,14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      12,13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  int num[15]={16,8,8,4,2,2,4,2,2,4,2,2,4,2,2};


  int nParentSum[2];
  int parentSum[3][2]  = { 0,0, 1,1, 2,2 };
  for(int k=0;k<15;k++){
    int nin = num[k];
    int * ndata=_map[k];
    for(int i=0;i<16;i++) cout<<ndata[i]<<",";
    cout<<" = "<<nin<<endl;

    TString fname(dirName);
    fname+="/";
    fname+=fileBase;
    fname+=oname[k];
    fname+=".root";
    if (k < 3) {
        nParentSum[0] = 3;
        nParentSum[1] = 3;
    } else if (k < 12) {
        nParentSum[0] = 1;
        nParentSum[1] = 1;
        parentSum[0][0] = k/3 - 1;
        parentSum[0][1] = k/3 - 1;
    } else {
        nParentSum[0] = 1;
        nParentSum[1] = 2;
        parentSum[0][0]  = 2;
        parentSum[0][1]  = 0;
        parentSum[1][1]  = 1;
    }
    adder.addCuts(fname.Data(),tf,ndata,nin,parentSum,nParentSum,1);
  }

  TString fname(dirName);
  fname+="/";
  fname+=fileBase;
  fname+="_pairDensities.root";
  adder.addDensities(fname.Data(),tf);
};


