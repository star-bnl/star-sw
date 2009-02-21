#ifndef EEMC_TRIG_UTIL_HH
#define EEMC_TRIG_UTIL_HH

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
  static void readPed4(char *path, char *dataSet, int mxChan, int *feePed4); 
  static void genPed4(int ped4val, int mxChan, int *feePed4);

 public:

  static void getDsmThresholds(int yyyymmdd, int hhmmss, DsmThreshold &thresholds);

  static void getFeePed4(char *path,int yyyymmdd, int hhmmss, int mxChan, int *feePed4);

};

//
// $Id: EemcTrigUtil.h,v 1.3 2009/02/21 09:07:24 ogrebeny Exp $
//
// $Log: EemcTrigUtil.h,v $
// Revision 1.3  2009/02/21 09:07:24  ogrebeny
// Updates for 2009 DSM thresholds from Pibero and Liaoyuan
//
// Revision 1.2  2007/12/05 23:44:59  jwebb
// Time-dependent DSM thresholds added.
//
//

#endif 
