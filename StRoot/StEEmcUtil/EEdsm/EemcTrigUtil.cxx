#include <assert.h>
#include <stdio.h>
#include <StMessMgr.h>
#include <TString.h>

#include "EemcTrigUtil.h"



struct DsmThreshold
{
  Int_t date_start; 
  Int_t time_start;// timezone = EST/EDT
  Int_t date_finish; 
  Int_t time_finish;
  Int_t HT[3];
  Int_t TP[3];
  Int_t JP[3];
  Int_t HTTPselect;
};

// -----------------------------------------------------------------------------------------------------
//
// pp2006 run period http://drupal.star.bnl.gov/STAR/?q=node/8349
//
//                             date0    time0  dateF    timeF  HT[]      TP[]      JP[]       HTTP-select   // run number
DsmThreshold _pp2006Long1a  = {20060323,141204,20060406,170325,{6,12,18},{1,17,31},{32,49,69},2};           // 7082079
DsmThreshold _pp2006Trans1a = {20060406,170325,20060508,210815,{6,12,22},{1,17,31},{32,49,69},2};           // 7096033
DsmThreshold _pp2006Trans1b = {20060508,210815,20060508,224937,{6,13,22},{1,21,31},{32,49,69},2};           // 7128050
DsmThreshold _pp2006Trans1c = {20060508,224937,20060508,230315,{6,12,22},{1,17,31},{32,49,69},2};           // 7128060
DsmThreshold _pp2006Trans1d = {20060508,230315,20060510,164714,{6,13,22},{1,21,31},{32,49,69},2};           // 7128061
DsmThreshold _pp2006Long2a  = {20060510,164714,20060512,152554,{6,12,22},{1,20,31},{38,49,69},2};           // 7130037
DsmThreshold _pp2006Long2b  = {20060512,152554,20060513,175249,{6,17,22},{1,20,31},{39,50,69},2};           // 7132045
DsmThreshold _pp2006Long2c  = {20060513,175249,20060607,194011,{6,16,22},{1,20,31},{39,52,69},2};           // 7133052

DsmThreshold _thresholds[] = {
  _pp2006Long1a, _pp2006Trans1a, _pp2006Trans1b, _pp2006Trans1c, _pp2006Trans1d, _pp2006Long2a, _pp2006Long2b, _pp2006Long2c 
};
//
// end pp2006






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

  for(Int_t i=0;i<3;i++){
    HTthr[i]= HTthrA[i];
    TPthr[i]= TPthrA[i];
    JPthr[i]= JPthrA[i];
  }
  
  TPthrSelc=TPthrSelcA;
  HTTPthrSelc=HTTPthrSelcA;

  BEsumthr=BEsumthrA;
  EEsumthr=EEsumthrA;
  JPSIthrSelc=JPSIthrSelcA;
  BarreSide=BarreSideA;
  EtotThr=EtotThrA;

  
  // Take the above as defaults, and overide with values
  // defined below

  // pp2006 time dependent thresholds
  Bool_t go = false;
  for ( UInt_t i=0;i<sizeof(_thresholds)/sizeof(DsmThreshold);i++ )
    {
      Double_t wallStart = _thresholds[i].date_start + _thresholds[i].time_start / 1.0E6;
      Double_t wallEnd   = _thresholds[i].date_finish + _thresholds[i].time_finish / 1.0E6;

      // shift from GMT to EDT
      Double_t myWallTime = wallTime - 040000/1.0E6;

      if ( myWallTime >= wallStart && myWallTime < wallEnd ) {
	go = true;
	for(Int_t j=0;j<3;j++)
	  {
	    HTthr[j]= _thresholds[i].HT[j];
	    TPthr[j]= _thresholds[i].TP[j];
	    JPthr[j]= _thresholds[i].JP[j];
	  }
	HTTPthrSelc=_thresholds[i].HTTPselect;
	break;
      }
    }
  assert(go); // or timestamp out of range of lookup table in EemcTrigUtil
  
  LOG_INFO<<Form( "Load DSM thresholds for %8i %06i: HT %02i %02i %02i TP %02i %02i %02i JP %02i %02i %02i",
		  yymmdd,hhmmss,
		  HTthr[0], HTthr[1], HTthr[2],
		  TPthr[0], TPthr[1], TPthr[2], 
		  JPthr[0], JPthr[1], JPthr[2]
		  ) << endm;

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

