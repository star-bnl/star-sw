void selectAllM10(const char* dirname, const char *fileBase ){

  // -- example for recombining histograms by selection in new root file 
  //
  // root.exe -q -b selectASNS.C'("dirContainingFinal.rootFile")
  //

  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  TString inFile(dirname);
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

  //--> do all of the following
  const char* oname[]={"all","marg1","marg2","marg3","marg4","marg5","marg6","marg7","marg8","marg9","ytlow"};

  const int _map[11][45]={ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,
	  0, 1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  1, 9, 10, 11, 12, 13, 14, 15, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  2, 10, 17, 18, 19, 20, 21, 22, 23, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  3, 11, 18, 24, 25, 26, 27, 28, 29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  4, 12, 19, 25, 30, 31, 32, 33, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  5, 13, 20, 26, 31, 35, 36, 37, 38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  6, 14, 21, 27, 32, 36, 39, 40, 41, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  7, 15, 22, 28, 33, 37, 40, 42, 43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  8, 16, 23, 29, 34, 38, 41, 43, 44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  0, 1, 2, 3, 4, 5, 9, 10, 11, 12, 13, 17, 18, 19, 20, 18, 19, 20, 24, 25, 26, 30, 31, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  
  int num[11]={45,9,9,9,9,9,9,9,9,9,24};


  int nParentSum[2];
  int parentSum[3][2]  = { 0,0, 1,1, 2,2 };
  for(int k=0;k<10;k++){
    int nin = num[k];
    int * ndata=_map[k];
    for(int i=0;i<45;i++) cout<<ndata[i]<<",";
    cout<<" = "<<nin<<endl;

    TString fname(dirname);
    fname+="/";
    fname+=fileBase;
    fname+=oname[k];
    fname+=".root";
    nParentSum[0] = 3;
    nParentSum[1] = 3;
    adder.addCuts(fname.Data(),tf,ndata,nin,parentSum,nParentSum,1);
  } 

  TString fname(dirname);
  fname+="/";
  fname+=fileBase;
  fname+="_pairDensities.root";
  adder.addDensities(fname.Data(),tf);
};


