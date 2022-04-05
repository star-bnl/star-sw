class  SmdGains;
SmdGains *task=0;
TObjArray  *HList;
TFile *fd;

// doSmdGains( int sectID=6 , int iU=0){
doSmdGains( int sectID , int iU) {
  
  TString iPath="iter5-pp/sect";
  if (sectID<10) iPath+="0"; 
  iPath+=sectID; iPath+="/";  

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

 gStyle->SetPalette(1,0);

 gSystem->Load("StEEmcUtil");
 gSystem->Load("StEzSmdCal");
 printf("loaded  libraries\n");
 
  HList=new TObjArray;
  task=new SmdGains;
  
  char tt[500];
  sprintf(tt,"%ssum-sect%d.hist.root",iPath.Data(),sectID);

  fd=task->open(tt); 
  // fd=task->open(iPath+"smdCal-R6049129.hist.root"); 
  // fd=task->open(iPath+"sum.hist.root"); 
  //  return;

  task->set(HList,sectID,'U'+iU);
  task->init();
 
   int str1=267,str2=270;

#if 0
  //=== iteration 0 =====> SMD gains from slopes
  // just plot all SMD slopes for given sector for one plain
  for(str1=1;str1<288;str1+=30) {
    str2=str1+29;
    if(str2>288) str2=288;
    task->fitSlopesSmd(str1,str2,1); 
    // sww 1/26/2007 change conv fact from 760. -> 850.
    //task-> doSlopesOnly(760./1.4); // for CuCu200-minB
    task-> doSlopesOnly(850./1.4); // for 2006 pp minbias
    //  break;
  }
#endif


#if 1
  //=== iteration 1 =====> SMD gains from MIPs from pairs of strips
  str1=1; str2=288;
  task->doGainCorr(str1,str2,12,1);
  task->plTGraph("pol0",1,1); // avearge MIP energy  
  task->saveHisto();

  if(0) {
    task->plFGC();
    task->plTGraph("pol0",0); // individual gain corrections

  }
 #endif

  /* === iteration 1b =====> run reCalSmd.C to 
     - average gain corrections among plains
     - calculate new gains from the old gains
  */

  char tt[100];
  sprintf(tt,"smd%02d%c.dat",sectID,'U'+iU);
  FILE *fd=fopen(tt,"w"); assert(fd);
  fprintf(fd,"# gains for SMD plain %02d%c, inverted slopes\n# stripName, gain[ch/GeV], erGain, anything\n",sectID,'U'+iU);
  task->saveGains(fd);
  fclose(fd);
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
