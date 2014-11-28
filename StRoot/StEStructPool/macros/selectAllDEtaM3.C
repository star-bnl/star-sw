void selectAllDEtaM3(const char* dirname){

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

  //--> do all of the following
  const char* oname[]={"LDEall","LDEallAS","LDEallNS","SDEall","SDEallAS","SDEallNS"};


  const int _map[6][8]= {  0, 3, 4, 7, 8,11,12,15,
                           0, 4, 8,12, 0, 0, 0, 0,
                           3, 7,11,15, 0, 0, 0, 0,
                           1, 2, 5, 6, 9,10,13,14,
                           1, 5, 9,13, 0, 0, 0, 0,
                           2, 6,10,14, 0, 0, 0, 0};
  
int num[6]={8,4,4,8,4,4};


  for(int k=0;k<6;k++){
    int nin = num[k];
    int * ndata=_map[k];
    for(int i=0;i<8;i++) cout<<ndata[i]<<",";
    cout<<" = "<<nin<<endl;

    TString fname(dirname);
    fname+="/";
    fname+=oname[k];
    fname+=".root";
    adder.addCuts(fname.Data(),tf,ndata,nin);
  } 

  /*
  int nas[16];//={0,12,4,5,8,9,12,13};
  int numas=16;
  for(int i=0;i<numas;i++)nas[i]=i;
  TString fname(dirname); fname+="/all";
  fname+=".root";
  adder.addCuts(fname.Data(),tf,nas,numas);
  */

};


