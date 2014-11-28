void selectASNS(const char* dirname){

  // -- example for recombining histograms by selection in new root file 
  //
  // root.exe -q -b selectASNS.C'("dirContainingFinal.rootFile")
  //

  TString dir(dirname);
  dir+="/final.root";
  gSystem->Load("StEStructPoolSupport.so");

  StEStructHAdd adder;

  TFile * tf=new TFile(dir.Data());

  if(!tf){
    cout<<"error opening file "<<endl;
    return ;
  };

  int nas[8]={0,1,4,5,8,9,12,13};
  int numas=8;
  TString fname(dirname); fname+="/awayside";
  fname+=".root";
  adder.addCuts(fname.Data(),tf,nas,numas);


  int nns[8]={2,3,6,7,10,11,14,15};
  int numns=8;
  TString fname2(dirname); fname2+="/nearside";
  fname2+=".root";
  adder.addCuts(fname2.Data(),tf,nns,numns);

};


