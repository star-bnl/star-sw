#include "StMessMgr.h"
#include "TString.h"

#include "EemcTrigUtil.h"

// -----------------------------------------------------------------------------------------------------
//
// pp2006 run period http://drupal.star.bnl.gov/STAR/?q=node/8349
//
//                             date0    time0  dateF    timeF  HT[]      TP[]      JP[]       HTTP-select   // run number
DsmThreshold _pp2006Long1a  = {20060323,181204,20060406,210325,{6,12,18},{1,17,31},{32,49,69},2,2,71,21,0,3,109}; // 7082079
DsmThreshold _pp2006Trans1a = {20060406,210325,20060509, 10815,{6,12,22},{1,17,31},{32,49,69},2,2,71,21,0,3,109}; // 7096033
DsmThreshold _pp2006Trans1b = {20060509, 10815,20060509, 24937,{6,13,22},{1,21,31},{32,49,69},2,2,71,21,0,3,109}; // 7128050
DsmThreshold _pp2006Trans1c = {20060509, 24937,20060509, 30315,{6,12,22},{1,17,31},{36,49,69},2,2,71,21,0,3,109}; // 7128060
DsmThreshold _pp2006Trans1d = {20060509, 30315,20060510,204714,{6,13,22},{1,21,31},{32,49,69},2,2,71,21,0,3,109}; // 7128061
DsmThreshold _pp2006Long2a  = {20060510,204714,20060512,192554,{6,12,22},{1,20,31},{38,49,69},2,2,71,21,0,3,109}; // 7130037
DsmThreshold _pp2006Long2b  = {20060512,192554,20060513,215249,{6,17,22},{1,20,31},{39,50,69},2,2,71,21,0,3,109}; // 7132045
DsmThreshold _pp2006Long2c  = {20060513,215249,20060607,234011,{6,16,22},{1,20,31},{39,52,69},2,2,71,21,0,3,109}; // 7133052

//
// end pp2006

// #### modified by Liaoyuan ####
// pp2009 run period and thresholds 
// http://www.star.bnl.gov/HyperNews-star/get/startrig/3753.html & 
// StRoot/StTriggerUtilities/Bemc/StBemcTriggerDbThresholds.cxx
//                             date0    time0  dateF    timeF  HT[]      TP[]      JP[]       HTTP-select   // run number
DsmThreshold _pp2009_500GeV = {20090217,     0,20090415,     0,{20,40,-1},{-1,-1,-1},{28,35,52},-1,2,71,21,0,3,109};
DsmThreshold _pp2009_200GeV = {20090415,     1,20090515,     0,{17,23,-1},{-1,-1,-1},{20,28,35},-1,2,71,21,0,3,109};

//
// end pp2009

DsmThreshold _thresholds[] = {
  _pp2006Long1a, _pp2006Trans1a, _pp2006Trans1b, _pp2006Trans1c, _pp2006Trans1d, _pp2006Long2a, _pp2006Long2b, _pp2006Long2c, 
  _pp2009_500GeV, _pp2009_200GeV
};

// #### modified end ####

//==================================================
//==================================================

void 
EemcTrigUtil::getDsmThresholds(int yyyymmdd, int hhmmss, DsmThreshold &thresholds) {
    thresholds.date_start = 0;
    thresholds.time_start = 0;
    thresholds.date_finish = 0;
    thresholds.time_finish = 0;
    thresholds.HT[0] = -1;
    thresholds.HT[1] = -1;
    thresholds.HT[2] = -1;
    thresholds.TP[0] = -1;
    thresholds.TP[1] = -1;
    thresholds.TP[2] = -1;
    thresholds.JP[0] = -1;
    thresholds.JP[1] = -1;
    thresholds.JP[2] = -1;
    thresholds.HTTPselect = 2;

    double wallTime= yyyymmdd+ hhmmss/1e6;
    Bool_t go = false;
    for (UInt_t i = 0;(i < sizeof(_thresholds) / sizeof(_thresholds[0])) && !go;i++)
      {
	Double_t wallStart = _thresholds[i].date_start + _thresholds[i].time_start / 1.0E6;
	Double_t wallEnd   = _thresholds[i].date_finish + _thresholds[i].time_finish / 1.0E6;
	if ( wallTime >= wallStart && wallTime < wallEnd ) {
	  go = true;
	  thresholds = _thresholds[i];
	  LOG_INFO<<Form( "Load DSM thresholds for %8i %06i: HT %02i %02i %02i TP %02i %02i %02i JP %02i %02i %02i",
		    yyyymmdd,hhmmss,
		    thresholds.HT[0], thresholds.HT[1], thresholds.HT[2],
		    thresholds.TP[0], thresholds.TP[1], thresholds.TP[2], 
		    thresholds.JP[0], thresholds.JP[1], thresholds.JP[2]
		    ) << endm;
	}
      }
    if (!go) {
      LOG_ERROR << "timestamp out of range of lookup table in EemcTrigUtil" << endm;
    }
}


void 
EemcTrigUtil::getFeePed4(char *path,int yyyyyymmdd, int hhmmss, int mxChan, int *feePed4){
  if (!(yyyyyymmdd>=20060307)) {
    LOG_ERROR << "not implemented before 2006" < endm;
  }
  if(yyyyyymmdd<=20060620) { // real data ended on June-20, day171
    char *dataSet="03.07.2006/";
    if(yyyyyymmdd>=20060406)  dataSet="04.06.2006/";
    if(yyyyyymmdd>=20060428)  dataSet="04.28.2006/";
    LOG_DEBUG << Form("getEndcap FeePed4 input set=%s\n",dataSet) << endm;
    readPed4(path, dataSet,mxChan, feePed4);
  } else {
    // assume it is MC, 12bit ADC peds are at 0
    int ped4val=5;
    LOG_DEBUG << Form("set Endcap FeePed4 to val=%d\n",ped4val) << endm;
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
      if (!fd) {
	LOG_ERROR << Form("Could not open %s for reading",fname) << endm;
      }
      for (int i=0; i<32; i++){
	int ival;
	int ret=fscanf(fd, "%d", &ival);
	if(!(ret>=0)) {
	  LOG_ERROR << "Failed to read 4-bit pedestal" << endm;
	}
	LOG_DEBUG << Form("i=%d, ival=%d\n", i, ival) << endm;
	feePed4[(crate-1)*mxChan+(board-1)*32+i]=ival;
      }
      fclose(fd);
    }
  }
}

