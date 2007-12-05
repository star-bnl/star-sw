#ifndef EEMC_TRIG_UTIL_HH
#define EEMC_TRIG_UTIL_HH

// function class to retrieve information about dsm configuration based on timestamps
class EemcTrigUtil {

 private:
  static void readPed4(char *path, char *dataSet, int mxChan, int *feePed4); 
  static void genPed4(int ped4val, int mxChan, int *feePed4);

 public:

  static void getDsmThresholds(int yymmdd, int hhmmss, int *HTthr, int *TPthr, int *JPthr, int &TPthrSelc, int &HTTPthrSelc, int &BEsumthr, int &EEsumthr, int &JPSIthrSelc, int &BarreSide, int &EtotThr);
  static void getFeePed4(char *path,int yyyymmdd, int hhmmss, int mxChan, int *feePed4);

};

//
// $Id: EemcTrigUtil.h,v 1.2 2007/12/05 23:44:59 jwebb Exp $
//
// $Log: EemcTrigUtil.h,v $
// Revision 1.2  2007/12/05 23:44:59  jwebb
// Time-dependent DSM thresholds added.
//
//

#endif 
