class  SmdGains;
SmdGains *task=0;
TObjArray  *HList;

doSmdGains( int iU=1){
  int sec=5;

  TString iPath="./";
 char *libL[]={
   "../StRoot/StEEmcUtil/EEmcGeom/libEEmcGeom.so", // some hidden dependence
   "../StRoot/StEEmcPool/StEzSmdCal/libEzSmdCal.so",
 };
 
 gStyle->SetPalette(1,0);
 int i;
 for(i=0;i<sizeof(libL)/sizeof(char*);i++) {
   printf("   load '%s' ...\n",libL[i]);
   assert( !gSystem->Load(libL[i]));
 }
 printf("loaded %d libraries\n",i);
 
  HList=new TObjArray;
  task=new SmdGains;

  // task->open("5UV.hist.root"); // Murad's file
  //task->open("../outC3/mip.hist.root"); // Iter-3
  task->open("../outC5/mipA.hist.root"); // Iter-4


  task->set(HList,5,'U'+iU);
  task->init();
  int str1=1,str2=280;
 
  task->doOneStripEne(str1,str2);
  task->doGainCorr(str1,str2);
  task->saveHisto();
  //  return;

  task->plFGC();
  task->plTGraph("pol0",0);
  task->plTGraph("pol0",1);
  


  return;
  sprintf(tt,"smd%02d%c.dat",sec,'U'+iU);
  FILE *fd=fopen(tt,"w"); assert(fd);
  task->saveGains(fd);
  fclose(fd);

  // ....... slopes only:  
  task->fitSlopes(251,259); return;
   

  return;
  

  // task-> doSlopesOnly(760.);

  //.... absolute average MIP position

  task->finish(0);

  return;
 

	  
  // .........   input  event file   .........
  
  TChain *chain = new TChain("ezstar");
  TString fullName=iPath+run+".ez.root";  
  chain->Add(fullName);
  int nEntries = (Int_t)chain->GetEntries();
  printf("Sort %d  of total Events %d\n",mxEve, nEntries);
  int nEve=0;
  
  Int_t nEntries = (Int_t)chain->GetEntries();
  if(nEntries<=0) {
    printf("\n\nSth is wrong, chain is empty nEntries=%d\n\n",nEntries);
    assert(nEntries>0);
  }
  
  EEfeeRawEvent  *eFee=0;
  chain->SetBranchAddress("eemc",&eFee);
  sorter->init();
  
  int nEve=0;
  int t1=time(0);  
  for(nEve=0; nEve<nEntries && nEve<mxEve; nEve++) {
    chain->GetEntry(nEve);
    if(nEve%2000==0)printf("in %d\n",nEve);
    sorter->make();
  }

  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*nEve/(t2-t1);
  printf("sorting done, nEve=%d CPU event rate=%.1f Hz, total time %.1f minute(s) \n",nEve,rate,tMnt);

  

  return;
} 

