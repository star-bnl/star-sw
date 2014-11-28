// -*- mode:c++ -*-

#ifndef EEMC_TRIG_UTIL_HH
#define EEMC_TRIG_UTIL_HH

#include "TDatime.h"

struct DsmThreshold
{
  Int_t date_start;
  Int_t time_start;// timezone = GMT
  Int_t date_finish;
  Int_t time_finish;
  Int_t HT[3];
  Int_t TP[3];
  Int_t JP[3];
  Int_t HTTPselect;
  Int_t TPthrSelc;
  Int_t BEsumthr;
  Int_t EEsumthr;
  Int_t JPSIthrSelc;
  Int_t BarreSide;
  Int_t EtotThr;
};

// function class to retrieve information about dsm configuration based on timestamps
class EemcTrigUtil {
 private:
  static void readPed4(const char *path, int mxChan, int *feePed4); 
  static void genPed4(int ped4val, int mxChan, int *feePed4);

 public:
  static void getDsmThresholds(int yyyymmdd, int hhmmss, DsmThreshold &thresholds);
  static void getFeePed4(const char *path,int yyyymmdd, int hhmmss, int mxChan, int *feePed4);
  static void getFeePed4(const TDatime& date, int mxChan, int* feePed4);
  static void getFeeOutMask(const char* maskfile, int* highTowerMask, int* patchSumMask);
  static void getFeeOutMask(const TDatime& date, int* highTowerMask, int* patchSumMask);
  static void getDsmAndChannelFromSteveJetPatchAndTriggerPatch(int jetpatch, int triggerpatch, int& dsm, int& chan);
  static void getTriggerPatchFromDsmAndChannel(int dsm, int chan, int& triggerpatch);
  static void getTriggerPatchFromSteveJetPatchAndTriggerPatch(int jetpatch, int triggerpatch, int& triggerpatch2);
  static void getFeeBoardMask(const TDatime& date, int* highTower);
  static void getFeeBoardFromSteveTriggerPatch(int triggerpatch, int& board);
};

//
// $Id: EemcTrigUtil.h,v 1.5 2011/10/16 21:43:44 pibero Exp $
//
// $Log: EemcTrigUtil.h,v $
// Revision 1.5  2011/10/16 21:43:44  pibero
// Implement EEMC FEE boards HT masks
//
// Revision 1.4  2011/10/16 17:41:59  pibero
// Implement EEMC FEE HT & TP masks
//
// Revision 1.3  2009/11/18 19:12:13  pibero
// Added Endcap FEE pedestals for all years.
// The code will scan the setup directory /afs/rhic.bnl.gov/star/users/pibero/public/StarTrigSimuSetup/ped
// and load pedestals from the DB timestamp.
// The plan for the future is to upload the ped4 into the STAR database and retrieve from there.
//
// Revision 1.2  2009/11/18 15:50:59  pibero
// Address several compiler warnings of the type:
//
// warning: deprecated conversion from string constant 'char*'
//
// Revision 1.1  2009/10/12 18:04:27  pibero
// Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
//
// Revision 1.3  2009/02/21 09:07:24  ogrebeny
// Updates for 2009 DSM thresholds from Pibero and Liaoyuan
//
// Revision 1.2  2007/12/05 23:44:59  jwebb
// Time-dependent DSM thresholds added.
//
//

#endif 
