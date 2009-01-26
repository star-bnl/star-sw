/***************************************************************************
 *
 * $Id: StSvtBadAnodesMaker.h,v 1.2 2009/01/26 15:01:51 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Bad anodes Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtBadAnodesMaker.h,v $
 * Revision 1.2  2009/01/26 15:01:51  fisyak
 * Add missing (in ROOT 5.22) forward declaration
 *
 * Revision 1.1  2004/02/03 20:06:28  munhoz
 * first version of bad anode maker
 *
 *
 **************************************************************************/

#ifndef STSVTBADANODESMAKER_H
#define STSVTBADANODESMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif
class TFile;
class TH2F;
class TString;

class TObjectSet;
class StSvtHybridCollection;
class StSvtHybridData;
class StSvtData;
class StSvtHybridBadAnodes;
class StSvtHybridPed;

class StSvtBadAnodesMaker : public StMaker {
 private:
  StSvtHybridData  *mHybridData; //!
  StSvtData        *mSvtData;    //!
  StSvtHybridCollection *mSvtBadAnodes;    //!
  StSvtHybridBadAnodes  *mHybridBadAnodes; //!
  StSvtHybridCollection *mSvtPed;       //!
  StSvtHybridCollection *mSvtRMSPed;       //!
  StSvtHybridPed   *mHybridPed;         //!   
  StSvtHybridPed   *mHybridRMSPed;         //!   

  TObjectSet       *mBadAnodesSet;   //! 

  int mEvents;
  int mRmsScaleFactor;

  int NULL_ADC;                      //
  int OVERLOADED_ADC;                //
  float BAD_RMS;                       //  in ADC counts
  int BAD_MEAN_PED_MIN;                  //
  int BAD_MEAN_PED_MAX;                  //
  float BAD_MEAN_RMS_MIN;                  //
  float BAD_MEAN_RMS_MAX;                  //

  int NULL_ADC_THRESHOLD;            //
  int OVERLOADED_ADC_THRESHOLD;      //  in number of time bins
  int OCCUP_THRESHOLD;               //
  int RMS_THRESHOLD;                 //

  float FREQ_OVERLOADED_ADC;         //
  float FREQ_NULL_ADC;               //  fraction of events
  float FREQ_OCCUP;                  //

  TH2F**   mBadAnodesHist;   //!
  TH1F**   mBadAnodesBarrel; //!
  TH1F**   mBadAnodesLadder; //!

  TFile*   mFile; //!
  TString* mFileName; //!
  TString* mFileNameTxt; //!

 protected:

 public: 
  StSvtBadAnodesMaker(const char *name="SvtBadAnodes");
  virtual       ~StSvtBadAnodesMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Reset();

  void setSvtData();
  void setPedestal();
  void setRMSPedestal();
  void setSvtBadAnodes();
  void bookHistograms();

  void setBadValues(int x1, int x2, int x3);
  void setThresholds(int x1, int x2, int x3, int x4);
  void setThresholdNull(int);
  void setThresholdOver(int);
  void setThresholdOccup(int);
  void setThresholdRMS(int);
  void setFrequencies(float x1, float x2, float x3);
  void setFrequencyNull(float);
  void setFrequencyOver(float);
  void setFrequencyOccup(float);
  void setNullADC(int);
  void setOverloadedADC(int);
  void setBadRMS(float);
  void setBadMeanPedMin(int);
  void setBadMeanPedMax(int);
  void setBadMeanRMSMin(float);
  void setBadMeanRMSMax(float);
  void setRMSFactor(int factor){mRmsScaleFactor = factor;}

  void setOutputFile(const char* name);
  void writeToFile(const char* fileName = "badAnodes.txt");
  void blwh2rma(int barrel, int ladder, int wafer, int hybrid, 
		int& recBoard, int& mezz, int& mz_hyb);

  ClassDef(StSvtBadAnodesMaker, 1)   //StAF chain virtual base class for Makers
};

inline void StSvtBadAnodesMaker::setNullADC(int x)
{NULL_ADC = x;};
inline void StSvtBadAnodesMaker::setOverloadedADC(int x)
{OVERLOADED_ADC = x;};
inline void StSvtBadAnodesMaker::setBadRMS(float x)
{BAD_RMS = x;};
inline void StSvtBadAnodesMaker::setBadMeanPedMin(int x)
{BAD_MEAN_PED_MIN = x;};
inline void StSvtBadAnodesMaker::setBadMeanPedMax(int x)
{BAD_MEAN_PED_MAX = x;};
inline void StSvtBadAnodesMaker::setBadMeanRMSMin(float x)
{BAD_MEAN_RMS_MIN = x;};
inline void StSvtBadAnodesMaker::setBadMeanRMSMax(float x)
{BAD_MEAN_RMS_MAX = x;};
inline void StSvtBadAnodesMaker::setThresholds(int x1, int x2, int x3, int x4)
{NULL_ADC_THRESHOLD = x1; OVERLOADED_ADC_THRESHOLD = x2; 
 OCCUP_THRESHOLD = x3; RMS_THRESHOLD = x4;};
inline void StSvtBadAnodesMaker::setThresholdNull(int x1)
{NULL_ADC_THRESHOLD = x1;};
inline void StSvtBadAnodesMaker::setThresholdOver(int x2)
{OVERLOADED_ADC_THRESHOLD = x2;};
inline void StSvtBadAnodesMaker::setThresholdOccup(int x3)
{OCCUP_THRESHOLD = x3;};
inline void StSvtBadAnodesMaker::setThresholdRMS(int x4)
{RMS_THRESHOLD = x4;};
inline void StSvtBadAnodesMaker::setFrequencies(float x1, float x2, float x3)
{FREQ_NULL_ADC = x1; FREQ_OVERLOADED_ADC = x2; 
 FREQ_OCCUP = x3;};
inline void StSvtBadAnodesMaker::setFrequencyNull(float x1)
{FREQ_NULL_ADC = x1;};
inline void StSvtBadAnodesMaker::setFrequencyOver(float x2)
{FREQ_OVERLOADED_ADC = x2;};
inline void StSvtBadAnodesMaker::setFrequencyOccup(float x3)
{FREQ_OCCUP = x3;};

#endif


