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
  const char* oname[]={"all",
                       "pi+_pi+","pi+_pi-","pi-_pi-",
                       "pi+_K+", "pi+_K-", "pi-_K+", "pi-_K-",
                       "pi+_p+", "pi+_p-", "pi-_p+", "pi-_p-",
                       "K+_K+",  "K+_K-",  "K-_K-",
                       "K+_p+",  "K+_p-",  "K-_p+",  "K-_p-",
                       "p+_p+",  "p+_p-",  "p-_p-",
                       "other"};

  int nList[22];
  for (int i=0;i<22;i++) {
      nList[i] = i;
  }
  TString fname(dirName);
  fname+="/";
  fname+=inFile;
  fname+=oname[0];
  fname+=".root";
  adder.addCuts(fname.Data(),tf,nList,14);

  int offSet = 0;
  for(int k=0;k<22;k++){
    for (int i=0;i<2;i++) {
        nList[i] = i + offSet;
    }
    TString fname(dirName);
    fname+="/";
    fname+=inFile;
    fname+=oname[k+1];
    fname+=".root";
    adder.addCuts(fname.Data(),tf,nList,2);
    offSet += 2;
  }

};


