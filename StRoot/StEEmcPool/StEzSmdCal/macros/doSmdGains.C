class  SmdGains;
SmdGains *task=0;
TObjArray  *HList;
TFile *fd;

doSmdGains( int sectID=5 , int iU=0){
  
  TString iPath="/star/data05/scratch/balewski/outD0/";//BNL
  iPath="/auto/pdsfdv34/starspin/balewski/calib2004/outE2/";//LBL
  
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
  
  char tt[100];
  sprintf(tt,"mipA%02d",sectID);

  fd=task->open(iPath+tt+".hist.root"); 
  //fd=task->open(iPath+"R5112018.hist.root"); 
  //  return;

  task->set(HList,sectID,'U'+iU);
  task->init();
  int str1=1,str2=288;

  //  plotAllTiles(); return;
  //  task->fitSlopesSmd(250,280,1); return;
  //  fitSlopesSmdPlain(); // plot all strips w/ slopes for one plain

#if 0
  task->fitSlopesSmd(str1,str2); 
  //task->fitSlopesSmd(261,290,1);
  task-> doSlopesOnly(760.); 
#endif

#if 1
  task->doGainCorr(str1,str2,15,0);
  task->plTGraph("pol0",1,3); // avearge MIP energy  
  task->saveHisto();

  if(0) {
    task->plFGC();
    task->plTGraph("pol0",0); // individual gain corrections

  }
  return;
#endif
  char tt[100];
  sprintf(tt,"smd%02d%c.dat",sectID,'U'+iU);
  FILE *fd=fopen(tt,"w"); assert(fd);
  fprintf(fd,"# gains for SMD plain %02d%c, inverted slopes\n# stripName, gain[ch/GeV], erGain, anything\n",sectID,'U'+iU);
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

//--------------------------------
//  UTIL
//-------------------------------
 
void  plotAllTiles() {
  // fit raw P,Q,R,T  spectra with expo,  just plot
  char cT[4]={'P','Q','R','T'};
  int iT;
  for(iT=0;iT<4;iT++) {
    task->fitSlopesTile(1,6,cT[iT],3);  
    task->fitSlopesTile(7,6,cT[iT],3);  
  }
}

//-------------------------------
// plot all strips w/ slopes for one plain
void  fitSlopesSmdPlain(){
  int k;
  for(k=0;k<10;k++) {
    int str1=30*k;
    task->fitSlopesSmd(1+str1,30+str1,3);
  }
}

//------------------------------

//  task->doOneStripEne(str1,str2); // fit individaul energy of strips
//  task->doGainCorr(str1,str2);
