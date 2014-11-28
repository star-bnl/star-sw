
int pedStat(char runNum[10], int k=-1, TString path="./"){
  
  TFile *fd;
  TFile *fdraw;
  int jSector=0,jSubSect=0,jEta=0;
  int mskDay=0;
  const int mSec=12;
  const int mSub=5;
  const int mEta=12;
  const int mSmdPlanes=2;
  const int mSmdStrips=288;
  char preL[4]="PQR";
  int rNum=0;
  char day[10];
  char channel[10];
  TString subdir = path; subdir+="StatFiles/";
  char fileName[50];
  char logFile[50];
  
  float minTPedPos=1.0,maxTPedPos=100.0;
  float minMPedPos=1.0,maxMPedPos=400.0;
  float minTSig=.5,maxTSig=2.5;
  float minMSig=.3,maxMSig=1.5;
  float deadentriesT=.998;
  float deadentriesM=.9995; 
  
  assert( !gSystem->Load("StEEmcStatus"));
  oflPedStat *stat=new oflPedStat;

  FILE *fpout,*fplog;
    
  rNum=atoi(runNum+1);
  printf("\n\n%s %d %d\n\n",runNum,k,rNum);
      
  strncpy(day,runNum+3,3);
  day[3]='\0';
  
  TString fullname = path+"day"+day+"/outPed"+runNum+"/"+runNum+"fit.hist.root";
  TString fullnameraw = path+"day"+day+"/outPed"+runNum+"/"+runNum+".hist.root";
  
  fd= new TFile(fullname);
  if(fd==0){
    printf("file %s not found\n",runNum);
    return 1;
  }
  
  fdraw= new TFile(fullnameraw);
  if(fdraw==0){
    printf("raw file %s not found\n",runNum);
    return 1;
  }
      
  strcpy(fileName,runNum);
  strcat(fileName,".errs");
  strcpy(logFile,runNum);
  strcat(logFile,".log");
  
  TString Outname= subdir + fileName;
  fpout=fopen(Outname.Data(),"w");
  if(fpout==0){
    printf("Output file not opened\n");
    return 1;
  }
  TString logname=subdir + logFile;
  fplog=fopen(logname.Data(),"w");
  if(fplog==0){
    printf("Log file not opened\n");
    return 1;
  }
  int t=0;
   
  //initialize files for run
  stat->initRun(k,fd,fdraw,fpout,fplog,rNum);
  
  // loop over all towers
  for(jSector=0; jSector<12; jSector++)
    {  for (jSubSect=0;jSubSect<5;jSubSect++)
      { for(jEta=0; jEta<12; jEta++)
	{ 
	  sprintf(channel,"a%02dT%c%02d",jSector+1,'A'+jSubSect,jEta+1);
	  stat->procDetector(channel,maxTPedPos,minTPedPos,maxTSig,minTSig,deadentriesT,mskDay);
	}
      }
    }
  
  // loop over mapmt strips
  for(int jSector=0;jSector<mSec;jSector++) {
    for(int uv= 0; uv < mSmdPlanes; uv++){
      for(int lst=0;lst < mSmdStrips; lst++) {
	sprintf(channel,"a%02d%c%03d",jSector+1,'U'+uv,lst+1);
	stat->procDetector(channel,maxMPedPos,minMPedPos,maxMSig,minMSig,deadentriesM,mskDay);
      }
    }
    //loop over all pre1, pre2, post shower
    for(int jSubSect=0;jSubSect<mSub;jSubSect++) { 
      for(int jEta=0;jEta< mEta;jEta++) {
	for(int prlst=0;prlst < 3; prlst++){
	  
	  sprintf(channel,"a%02d%c%c%02d",jSector+1,preL[prlst],'A'+jSubSect,jEta+1);
	  stat->procDetector(channel,maxMPedPos,minMPedPos,maxMSig,minMSig,deadentriesM,mskDay);
	}
      }
    }
  }
  
  printf("Closing files\n");
  fclose(fpout);
  fclose(fplog);
  fd->Close("R");
  fdraw->Close("R");
  printf("all done\n");
  
  return 0;
} 
