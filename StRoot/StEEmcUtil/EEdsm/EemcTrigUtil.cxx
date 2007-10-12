#include <assert.h>
#include <stdio.h>
#include <StMessMgr.h>

#include "EemcTrigUtil.h"

//==================================================
//==================================================

void 
EemcTrigUtil::getDsmThresholds(int yymmdd, int hhmmss, int *HTthr, int *TPthr, int *JPthr, int &TPthrSelc, int &HTTPthrSelc, int &BEsumthr, int &EEsumthr, int &JPSIthrSelc, int &BarreSide, int &EtotThr){
  double wallTime= yymmdd+ hhmmss/1e6;
  int HTthrA[3]={6,12,22}; 
  int TPthrA[3]={1,17,31}; 

  int JPthrA[3]={32,49,69};

  int TPthrSelcA=2;
  int HTTPthrSelcA=2;

  int BEsumthrA=71;//need to find out the real value
  int EEsumthrA=21;//need to find out the real value

  int JPSIthrSelcA=0;

  int BarreSideA=3;

  int EtotThrA=109;

  int i;
  for(i=0;i<3;i++){
    HTthr[i]= HTthrA[i];
    TPthr[i]= TPthrA[i];

    JPthr[i]= JPthrA[i];
  }
  
  if(wallTime>=20060409.120000 ){ // R7099070
    HTthr[1]=13;
    TPthr[1]=21;
  }

  TPthrSelc=TPthrSelcA;
  HTTPthrSelc=HTTPthrSelcA;

  BEsumthr=BEsumthrA;
  EEsumthr=EEsumthrA;

  JPSIthrSelc=JPSIthrSelcA;
  
  BarreSide=BarreSideA;

  EtotThr=EtotThrA;

  // temp limits on the avaliable time stampe
  assert(yymmdd>=20060408  ); // R7098001
  //assert(yymmdd<=20060414  ); // R7103013

}

//==================================================
//==================================================

void 
EemcTrigUtil::getFeePed4(char *path,int yyyymmdd, int hhmmss, int mxChan, int *feePed4){
  assert(yyyymmdd>=20060307); // not implemented before 2006
  if(yyyymmdd<=20060620) { // real data ended on June-20, day171
    char *dataSet="03.07.2006/";
    if(yyyymmdd>=20060406)  dataSet="04.06.2006/";
    if(yyyymmdd>=20060428)  dataSet="04.28.2006/";
    printf("getEndcap FeePed4 input set=%s\n",dataSet);
    readPed4(path, dataSet,mxChan, feePed4);
  } else {
    // assume it is MC, 12bit ADC peds are at 0
    int ped4val=5;
    printf("set Endcap FeePed4 to val=%d\n",ped4val);
    genPed4(ped4val,mxChan, feePed4);
  }
}

//==================================================
//==================================================

void 
EemcTrigUtil::genPed4(int ped4val, int mxChan, int *feePed4){
  for (int crate=1; crate<=6; crate++){
    int board;
    for ( board=1; board<=4; board++){
      for (int i=0; i<32; i++){
	feePed4[(crate-1)*mxChan+(board-1)*32+i]=ped4val;
      }
    }
  }
}

//==================================================
//==================================================

void 
EemcTrigUtil::readPed4(char *path, char *dataSet, int mxChan, int *feePed4){

  for (int crate=1; crate<=6; crate++){
    char fname[1000];
    int board;
    for ( board=1; board<=4; board++){
      sprintf(fname,"%s%scrate%dboard%d.ped4", path, dataSet, crate, board);
      FILE *fd=fopen(fname,"r");
      if(fd==0)  LOG_FATAL <<"EemcTrigUtil::failed open"<<fname<<endm;
      assert(fd);
      for (int i=0; i<32; i++){
	int ival;
	int ret=fscanf(fd, "%d", &ival);
	assert(ret>=0);
	//printf("i=%d, ival=%d\n", i, ival);
	feePed4[(crate-1)*mxChan+(board-1)*32+i]=ival;
      }
      fclose(fd);
    }
  }
}

