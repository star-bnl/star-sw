void selectAllM9(const char* dirName, const char *fileBase ){

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
  const char* oname[]={"all","below4","below3","below2","below1","above1","above2","above3","above4","one","onetwo","onethree","two","twothree","three","soft","hard"};

  const int _map[17][21]={
      0,  1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,
      0,  1, 2, 3, 4, 6, 7, 8, 9,11,12,13,15,16,18, 0, 0, 0, 0, 0, 0,
      0,  1, 2, 3, 6, 7, 8,11,12,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0,  1, 2, 6, 7,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0,  1, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2,  3, 4, 5, 8, 9,10,13,14,17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      3,  4, 5, 9,10,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      4,  5,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      5,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2,  3, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2,  3, 4, 8, 9,13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      3,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      3,  4, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      4,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      1,  2, 3, 4, 5, 7, 8, 9,10,12,13,14,16,17,19, 0, 0, 0, 0, 0 0};

  int num[17]={21,15,10,6,3,10,6,3,1,1,3,6,1,3,1,1,15};


  int nParentSum[2];
  int parentSum[6][2]  = { 0,0, 1,1, 2,2,  3,3,   4,4,  5,5 };
  for(int k=0;k<17;k++){
    int nin = num[k];
    int * ndata=_map[k];
    for(int i=0;i<21;i++) cout<<ndata[i]<<",";
    cout<<" = "<<nin<<endl;

    TString fname(dirName);
    fname+="/";
    fname+=fileBase;
    fname+=oname[k];
    fname+=".root";
    switch (k) {
        case 0 : {
            nParentSum[0] = 6;
            nParentSum[1] = 6;
            break;
        } case 1 : {
            nParentSum[0] = 5;
            nParentSum[1] = 5;
            break;
        } case 2 : {
            nParentSum[0] = 4;
            nParentSum[1] = 4;
            break;
        } case 3 : {
            nParentSum[0] = 3;
            nParentSum[1] = 3;
            break;
        } case 4 : {
            nParentSum[0] = 2;
            nParentSum[1] = 2;
            break;
        } case 5 : {
            nParentSum[0] = 4;
            nParentSum[1] = 4;
            parentSum[0][0] = 2;
            parentSum[0][1] = 2;
            parentSum[1][0] = 3;
            parentSum[1][1] = 3;
            parentSum[2][0] = 4;
            parentSum[2][1] = 4;
            parentSum[3][0] = 5;
            parentSum[3][1] = 5;
            break;
        } case 6 : {
            nParentSum[0] = 3;
            nParentSum[1] = 3;
            parentSum[0][0] = 3;
            parentSum[0][1] = 3;
            parentSum[1][0] = 4;
            parentSum[1][1] = 4;
            parentSum[2][0] = 5;
            parentSum[2][1] = 5;
            break;
        } case 7 : {
            nParentSum[0] = 2;
            nParentSum[1] = 2;
            parentSum[0][0] = 4;
            parentSum[0][1] = 4;
            parentSum[1][0] = 5;
            parentSum[1][1] = 5;
            break;
        } case 8 : {
            nParentSum[0] = 1;
            nParentSum[1] = 1;
            parentSum[0][0] = 5;
            parentSum[0][1] = 5;
            break;
        } case 9 : {
            nParentSum[0] = 1;
            nParentSum[1] = 1;
            parentSum[0][0] = 2;
            parentSum[0][1] = 2;
            break;
        } case 10 : {
            nParentSum[0] = 2;
            nParentSum[1] = 2;
            parentSum[0][0] = 2;
            parentSum[0][1] = 2;
            parentSum[1][0] = 3;
            parentSum[1][1] = 3;
            break;
        } case 11 : {
            nParentSum[0] = 3;
            nParentSum[1] = 3;
            parentSum[0][0] = 2;
            parentSum[0][1] = 2;
            parentSum[1][0] = 3;
            parentSum[1][1] = 3;
            parentSum[2][0] = 4;
            parentSum[2][1] = 4;
            break;
        } case 12 : {
            nParentSum[0] = 1;
            nParentSum[1] = 1;
            parentSum[0][0] = 3;
            parentSum[0][1] = 3;
            break;
        } case 13 : {
            nParentSum[0] = 2;
            nParentSum[1] = 2;
            parentSum[0][0] = 3;
            parentSum[0][1] = 3;
            parentSum[1][0] = 4;
            parentSum[1][1] = 4;
            break;
        } case 14 : {
            nParentSum[0] = 1;
            nParentSum[1] = 1;
            parentSum[0][0] = 4;
            parentSum[0][1] = 4;
            break;
        } case 15 : {
            nParentSum[0] = 1;
            nParentSum[1] = 1;
            parentSum[0][0] = 0;
            parentSum[0][1] = 0;
            break;
        } case 16 : {
            nParentSum[0] = 5;
            nParentSum[1] = 5;
            parentSum[0][0] = 1;
            parentSum[0][1] = 1;
            parentSum[1][0] = 2;
            parentSum[1][1] = 2;
            parentSum[2][0] = 3;
            parentSum[2][1] = 3;
            parentSum[3][0] = 4;
            parentSum[3][1] = 4;
            parentSum[4][0] = 5;
            parentSum[4][1] = 5;
            break;
        }
    }
    adder.addCuts(fname.Data(),tf,ndata,nin,parentSum,nParentSum,1);
  }

  TString fname(dirName);
  fname+="/";
  fname+=fileBase;
  fname+="_pairDensities.root";
  adder.addDensities(fname.Data(),tf);
};


