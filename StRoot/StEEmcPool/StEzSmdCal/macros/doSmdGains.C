class  SmdGains;
SmdGains *task=0;
TObjArray  *HList;
TFile *fd;

doSmdGains( int iU=0){
  int sectID=6;

  TString iPath="/star/data05/scratch/balewski/outD1/";
  
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
  fd=task->open(iPath+"mip06A.hist.root"); 
  //fd=task->open(iPath+"R5112018.hist.root"); 
  //  return;

  task->set(HList,sectID,'U'+iU);
  task->init();
  int str1=1,str2=288;

  // return;
  //task->fitSlopes(30,38); return;
  //  task->fitSlopes(str1,str2);  task-> doSlopesOnly(760.); 

#if 1
  task->doOneStripEne(str1,str2);
  task->doGainCorr(str1,str2);
  task->saveHisto();

  if(0) {
    task->plFGC();
    task->plTGraph("pol0",0); // individual gain corrections
    task->plTGraph("pol0",1); // avearge MIP energy
  }

#endif
  char tt[100];
  sprintf(tt,"smd%02d%c.dat",sectID,'U'+iU);
  FILE *fd=fopen(tt,"w"); assert(fd);
  task->saveGains(fd);
  fclose(fd);
  return;
  // ....... slopes only:  
  task->fitSlopes(251,259); return;
   

  return;
  

  //

  //.... absolute average MIP position
  
  task->finish(0);

  return;
 
} 

