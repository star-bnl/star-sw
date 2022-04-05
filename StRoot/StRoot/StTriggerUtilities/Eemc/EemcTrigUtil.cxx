#include <dirent.h>
#include <pwd.h>
#include <cstdio>
#include <algorithm>

using namespace std;

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

//==================================================
//==================================================

void 
EemcTrigUtil::getFeePed4(const char *path,int yyyyyymmdd, int hhmmss, int mxChan, int *feePed4){
  if (yyyyyymmdd<20060307) {
    LOG_ERROR << "not implemented before 2006" << endm;
  }
  if (yyyyyymmdd<=20060620) { // real data ended on June-20, day171
    const char *dataSet = "03.07.2006";
    if (yyyyyymmdd>=20060406) dataSet = "04.06.2006";
    if (yyyyyymmdd>=20060428) dataSet = "04.28.2006";
    LOG_DEBUG << Form("getEndcap FeePed4 input set=%s\n",dataSet) << endm;
    char dir[FILENAME_MAX];
    sprintf(dir,"%s/%s",path,dataSet);
    readPed4(dir,mxChan,feePed4);
  } else {
    // assume it is MC, 12bit ADC peds are at 0
    int ped4val=5;
    LOG_DEBUG << Form("set Endcap FeePed4 to val=%d\n",ped4val) << endm;
    genPed4(ped4val,mxChan, feePed4);
  }
}

//==================================================
//==================================================

static int filter(const struct dirent* d)
{
  int month, day, year;
  return strlen(d->d_name) == 10 && sscanf(d->d_name,"%d.%d.%d",&month,&day,&year) == 3;
}

static TDatime getDateTime(const char* dirname)
{
  int month, day, year;
  sscanf(dirname,"%d.%d.%d",&month,&day,&year);
  return TDatime(year,month,day,0,0,0);
}

static void scanDirForDates(const char* dir, vector<TDatime>& dates)
{
  struct dirent** namelist;
  int n = scandir(dir,&namelist,filter,0);
  if (n == -1) return;
  while (n--) {
    dates.push_back(getDateTime(namelist[n]->d_name));
    free(namelist[n]);
  }
  free(namelist);
  sort(dates.begin(),dates.end());
}

static TDatime getTimeStampFromDates(const TDatime& date, const vector<TDatime>& dates)
{
  for (vector<TDatime>::const_reverse_iterator i = dates.rbegin(); i != dates.rend(); ++i)
    if (date > *i) return *i;
  return dates.front();
}

void EemcTrigUtil::getFeePed4(const TDatime& date, int mxChan, int *feePed4)
{
  char dir[FILENAME_MAX];
  struct passwd* pw = getpwnam("pibero");
  sprintf(dir,"%s/public/StarTrigSimuSetup/ped",pw->pw_dir);
  vector<TDatime> dates;
  scanDirForDates(dir,dates);
  // Adjust times
  for (size_t i = 0; i < dates.size(); ++i) {
    if (dates[i].GetDate() == 20090506) dates[i].Set("2009-05-06 10:00:00");
    if (dates[i].GetDate() == 20090513) dates[i].Set("2009-05-13 17:00:00");
    if (dates[i].GetDate() == 20090517) dates[i].Set("2009-05-17 23:30:00");
    if (dates[i].GetDate() == 20090520) dates[i].Set("2009-05-20 07:00:00");
    if (dates[i].GetDate() == 20090606) dates[i].Set("2009-06-06 18:00:00");
  }
  TDatime timeStamp = getTimeStampFromDates(date,dates);
  TString timeStampString = Form("%02d.%02d.%4d",timeStamp.GetMonth(),timeStamp.GetDay(),timeStamp.GetYear());
  char pathname[FILENAME_MAX];
  sprintf(pathname,"%s/%s",dir,timeStampString.Data());
  LOG_INFO << "Using ped4 directory " << pathname << endm;
  readPed4(pathname,mxChan,feePed4);

  // Check for additional pedestal files from this time stamp, e.g. 05.06.2009_crate5board2.ped4
  for (int crate = 1; crate <= 6; ++crate) {
    for (int board = 1; board <= 4; ++board) {
      sprintf(pathname,"%s/%s_crate%dboard%d.ped4",dir,timeStampString.Data(),crate,board);
      if (FILE* fp = fopen(pathname,"r")) {
	LOG_INFO << "Using ped4 file " << pathname << endm;
	int ped4val;
	for (int chan = 0; fscanf(fp,"%d",&ped4val)!= EOF; ++chan) {
	  int rdo = (crate-1)*mxChan+(board-1)*32+chan;
	  feePed4[rdo] = ped4val;
	  LOG_DEBUG << Form("crate=%d board=%d chan=%d rdo=%d ped4=%d",crate,board,chan,rdo,ped4val) << endm;
	}
	fclose(fp);
      }
    }
  }
}

//==================================================
//==================================================

static int maskFilter(const struct dirent* d)
{
  int month, day, year;
  return strlen(d->d_name) == 16 && sscanf(d->d_name,"eec.%d-%d-%d.dat",&month,&day,&year) == 3;
}

static TDatime getMaskDateTime(const char* maskfile)
{
  int month, day, year;
  sscanf(maskfile,"eec.%d-%d-%d.dat",&month,&day,&year);
  return TDatime(2000+year,month,day,0,0,0);
}

static void scanMaskDirForDates(const char* dir, vector<TDatime>& dates)
{
  struct dirent** namelist;
  int n = scandir(dir,&namelist,maskFilter,0);
  if (n == -1) return;
  while (n--) {
    dates.push_back(getMaskDateTime(namelist[n]->d_name));
    free(namelist[n]);
  }
  free(namelist);
  sort(dates.begin(),dates.end());
}

void EemcTrigUtil::getFeeOutMask(const char* maskfile, int* highTowerMask, int* patchSumMask)
{
  FILE* fp = fopen(maskfile,"r");
  if (fp) {
    char line[100];
    while (fgets(line,sizeof(line),fp) != NULL) {
      if (*line == '#') continue;
      int jetpatch, triggerpatch;
      char s1[3], s2[3];
      *s1 = *s2 = 0;
      int n = sscanf(line,"%d %d %s %s\n",&jetpatch,&triggerpatch,s1,s2);
      if (n > 2) {
	int triggerpatch2;
	getTriggerPatchFromSteveJetPatchAndTriggerPatch(jetpatch,triggerpatch,triggerpatch2);
	LOG_INFO << Form("JP=%d TP=%d %s %s (%d)",jetpatch,triggerpatch,s1,s2,triggerpatch2) << endm;
	if (strncmp(s1,"HT",2) == 0 || strncmp(s2,"HT",2) == 0) highTowerMask[triggerpatch2] = 0;
	if (strncmp(s1,"TP",2) == 0 || strncmp(s2,"TP",2) == 0)  patchSumMask[triggerpatch2] = 0;
      }
    }
  }
  fclose(fp);
}

void EemcTrigUtil::getDsmAndChannelFromSteveJetPatchAndTriggerPatch(int jetpatch, int triggerpatch, int& dsm, int& chan)
{
  // See Steve Vigdor's EEMC Trigger Patches document
  static const int dsmMap[6][15] = {
    { 7,7,8,7,7,8,7,7,8,7,7,8,7,7,8 },
    { 9,9,8,9,9,8,9,9,8,9,9,8,9,9,8 },
    { 1,1,2,1,1,2,1,1,2,1,1,2,1,1,2 },
    { 3,3,2,3,3,2,3,3,2,3,3,2,3,3,2 },
    { 4,4,5,4,4,5,4,4,5,4,4,5,4,4,5 },
    { 6,6,5,6,6,5,6,6,5,6,6,5,6,6,5 }
  };
  static const int chanMap[6][15] = {
    { 0,1,0,2,3,1,4,5,2,6,7,3,8,9,4 },
    { 0,1,5,2,3,6,4,5,7,6,7,8,8,9,9 },
    { 0,1,0,2,3,1,4,5,2,6,7,3,8,9,4 },
    { 0,1,5,2,3,6,4,5,7,6,7,8,8,9,9 },
    { 0,1,0,2,3,1,4,5,2,6,7,3,8,9,4 },
    { 0,1,5,2,3,6,4,5,7,6,7,8,8,9,9 }
  };
  dsm  =  dsmMap[jetpatch-1][triggerpatch-1];
  chan = chanMap[jetpatch-1][triggerpatch-1];
}

void EemcTrigUtil::getTriggerPatchFromDsmAndChannel(int dsm, int chan, int& triggerpatch)
{
  triggerpatch = (dsm-1)*10+chan;
}

void EemcTrigUtil::getTriggerPatchFromSteveJetPatchAndTriggerPatch(int jetpatch, int triggerpatch, int& triggerpatch2)
{
  int dsm, chan;
  getDsmAndChannelFromSteveJetPatchAndTriggerPatch(jetpatch,triggerpatch,dsm,chan);
  getTriggerPatchFromDsmAndChannel(dsm,chan,triggerpatch2);
}

void EemcTrigUtil::getFeeOutMask(const TDatime& date, int* highTowerMask, int* patchSumMask)
{
  char maskdir[FILENAME_MAX];
  struct passwd* pw = getpwnam("pibero");
  sprintf(maskdir,"%s/public/StarTrigSimuSetup/mask",pw->pw_dir);
  vector<TDatime> dates;
  scanMaskDirForDates(maskdir,dates);
  TDatime timestamp = getTimeStampFromDates(date,dates);
  char maskfile[FILENAME_MAX];
  sprintf(maskfile,"%s/eec.%02d-%02d-%02d.dat",maskdir,timestamp.GetMonth(),timestamp.GetDay(),timestamp.GetYear()%100);
  LOG_INFO << "Using mask file " << maskfile << endm;
  getFeeOutMask(maskfile,highTowerMask,patchSumMask);
}

void EemcTrigUtil::getFeeBoardFromSteveTriggerPatch(int triggerpatch, int& board)
{
  // See Steve Vigdor's EEMC Trigger Patches document
  static const int boardMap[15] = { 1,3,1,1,3,1,2,4,2,2,4,2,3,4,3 };
  board = boardMap[triggerpatch-1];
}

void EemcTrigUtil::getFeeBoardMask(const TDatime& date, int* highTower)
{
  char maskdir[FILENAME_MAX];
  struct passwd* pw = getpwnam("pibero");
  sprintf(maskdir,"%s/public/StarTrigSimuSetup/mask",pw->pw_dir);
  vector<TDatime> dates;
  scanDirForDates(maskdir,dates);
  TDatime timestamp = getTimeStampFromDates(date,dates);
  char timestampString[12];
  sprintf(timestampString,"/%02d.%02d.%4d",timestamp.GetMonth(),timestamp.GetDay(),timestamp.GetYear());
  strcat(maskdir,timestampString);
  LOG_INFO << "Using mask directory " << maskdir << endm;
  for (int jetpatch = 1; jetpatch <= 6; ++jetpatch) {
    int boardmask[5];
    fill(boardmask,boardmask+5,0);
    char maskfile[FILENAME_MAX];
    sprintf(maskfile,"%s/tower-%d-current_beam_config.dat",maskdir,jetpatch);
    LOG_INFO << "Scanning mask file " << maskfile << endm;
    FILE* fp = fopen(maskfile,"r");
    if (fp) {
      int value;
      char name[100];
      while (fscanf(fp,"%x %s\n",&value,name) != EOF) {
	if (strcmp(name,"board1mask") == 0) boardmask[0] = value;
	if (strcmp(name,"board2mask") == 0) boardmask[1] = value;
	if (strcmp(name,"board3mask") == 0) boardmask[2] = value;
	if (strcmp(name,"board4mask") == 0) boardmask[3] = value;
	if (strcmp(name,"board5mask") == 0) boardmask[4] = value;
      }
    }
    fclose(fp);
    for (int triggerpatch = 1; triggerpatch <= 15; ++triggerpatch) {
      int board = 0;
      getFeeBoardFromSteveTriggerPatch(triggerpatch,board);
      if (boardmask[board-1]) {
	int triggerpatch2;
	getTriggerPatchFromSteveJetPatchAndTriggerPatch(jetpatch,triggerpatch,triggerpatch2);
	highTower[triggerpatch2] = 0;
	LOG_INFO << Form("JP=%d TP=%d Brd=%d (%d)",jetpatch,triggerpatch,board,triggerpatch2) << endm;
      }
    }
  }
}

//==================================================
//==================================================

void 
EemcTrigUtil::genPed4(int ped4val, int mxChan, int *feePed4){
  for (int crate=1; crate<=6; crate++){
    for (int board=1; board<=4; board++){
      for (int chan=0; chan<32; chan++){
	int rdo = (crate-1)*mxChan+(board-1)*32+chan;
	feePed4[rdo]=ped4val;
	LOG_DEBUG << Form("crate=%d board=%d chan=%d rdo=%d ped4=%d",crate,board,chan,rdo,ped4val) << endm;
      }
    }
  }
}

//==================================================
//==================================================

void 
EemcTrigUtil::readPed4(const char *path, int mxChan, int *feePed4) {
  for (int crate = 1; crate <= 6; ++crate) {
    for (int board = 1; board <= 4; ++board) {
      char fname[FILENAME_MAX];
      sprintf(fname,"%s/crate%dboard%d.ped4",path,crate,board);
      FILE* fd = fopen(fname,"r");
      if (!fd) {
	LOG_ERROR << "Could not open " << fname << " for reading" << endm;
	continue;
      }
      LOG_DEBUG << "Reading " << fname << endm;
      int chan;
      for (chan = 0; chan < 32; ++chan) {
	int ped4;
	if (fscanf(fd,"%d",&ped4) == EOF) break;
	int rdo = (crate-1)*mxChan+(board-1)*32+chan;
	feePed4[rdo] = ped4;
	LOG_DEBUG << Form("crate=%d board=%d chan=%d rdo=%d ped4=%d",crate,board,chan,rdo,ped4) << endm;
      }
      LOG_DEBUG << "Read " << chan << " channels" << endm;
      fclose(fd);
    }
  }
}
