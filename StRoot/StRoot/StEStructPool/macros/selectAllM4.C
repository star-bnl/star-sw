void selectAllM4(const char* dirname){

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

  int nas[32];//={0,12,4,5,8,9,12,13};
  int numas=32;
  for(int i=0;i<numas;i++)nas[i]=i;
  TString fname(dirname); fname+="/all";
  fname+=".root";
  cout<<"Will write "<<fname.Data()<<endl;
  adder.addCuts(fname.Data(),tf,nas,numas);

  TString fname_ns(dirname); fname_ns+="/nearside.root";
  cout<<"Will write "<<fname_ns.Data()<<endl;
  adder.addCuts(fname_ns.Data(),tf,nas,16);

  int *ns=&nas[16];
  TString fname_as(dirname); fname_as+="/awayside.root";
  cout<<"Will write "<<fname_as.Data()<<endl;
  adder.addCuts(fname_as.Data(),tf,ns,16);

  int nsoft[2]={0,16};
  TString fname_soft(dirname); fname_soft+="/soft.root";
  cout<<"Will write "<<fname_soft.Data()<<endl;
  adder.addCuts(fname_soft.Data(),tf,nsoft,2);

  int nhard[26]={1,2,3,4,5,6,7,8,9,10,11,12,13,17,18,19,20,21,22,23,24,25,26,27,28,29};
  TString fname_hard(dirname); fname_hard+="/hard.root";
  cout<<"Will write "<<fname_hard.Data()<<endl;
  adder.addCuts(fname_hard.Data(),tf,nhard,26);




};


