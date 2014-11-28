/*********************************************************************
 * $Id: StBemcBeamBckgFinderMaker.h,v 1.9 2014/08/06 11:43:05 jeromel Exp $
 * \author Issam Qattan , IUCF, 2006 
 *********************************************************************
 * Description:
 * Pattern recognition of the Barrel Beam Background on an event-by-event basis.
 *********************************************************************
 */

#ifndef STAR_St_StBemcBeamBckgFinderMaker_h
#define STAR_St_StBemcBeamBckgFinderMaker_h

 
#ifndef StMaker_H
#include "StMaker.h"
#endif

class  TH2F;
class  TCanvas;
class  TStyle;
class  StEmcGeom;
class  TObjArray;
class  StEmcDecoder;

class StBemcBeamBckgFinderMaker : public StMaker {
 
 private:   
  // Private methods declaration if any:
 
  StEmcGeom    *mGeomB;
  StEmcDecoder *mMappB;
  TObjArray    *mHList;

  enum{mMaxH=8};
  enum{mMaxevtH=3};
  TH1F *mhisto[mMaxH];
  TH2F *mevtH[mMaxevtH];

  enum{mxSoftId = 4800}; 
  enum{mxEta=40, mxPhi=120};
  enum{xmlocate = 10};

   
  Int_t GetNewStatus(int id, int rdo, int run);
  void FillAdc(float iadc,int ieta,int iphi,int isoft);
  Int_t CheckPatternType3(int &etaBegin, int &phiBegin, int &etaEnd, int &patternLength, float &sumAdc, float &AverageWeightedEta);
  void PlotOneEvent();

  float mdb_btowPed[mxSoftId];   //array of pedestals
  int mdb_btowStat[mxSoftId];    //array of towers status (1 or 0 in values)

  int mdb_btowetaBin[mxSoftId];  //array of towers eta bins
  int mdb_btowphiBin[mxSoftId];  //array of towers phi bins
   
  int mdb_btowRdo[mxSoftId];     //array of towers rdos
  int mdb_btowSoftId[mxSoftId];  //array of towers softIds

  float mAdcArray[mxPhi][mxEta]; //array of adc values passing adc threshold
  int mSoftId[mxPhi][mxEta];     //array of towers softIDs for a given eta and phi

  int mInpEve;     //input events counter
  int mAccEve;     //accepted events counter
  int mTrigId;     //Trigger Id
  int mRunNumber;  //run number
  int mDecision;   //Decision whether a background==1 or not ==0.
  char mLocation[xmlocate];  //location of background (east, central, west) based on eta range.
  int metaBegin;             //value of beginning eta bin in the background pattern
  int metaEnd;               //value of ending eta bin in the background pattern
  int mphiBegin;             //value of beginning phi bin in the background pattern
  int mpatternLength;        //background pattern length 
  float msumAdc;             //background pattern adc sum
  float mAvgEtaAdc;          //average weighted eta = SUM(eta_i*adc_i)/SUM(adc_i) for the background pattern
  int mPattSoftId[mxSoftId]; //list of soft ID's of towers from identified pattern

  int mAdcThreshold;         //value used to set adc threshold
  float mAdcSumThreshold;    //value used to set adc sum threshold
  int mpattern;              //value used to set pattern length needed
  int mMaxYesPlots;          //value of Maximum number of postscript files to produce (file/event) when event is background.
  int mMaxNoPlots;           //value of Maximum number of postscript files to produce (file/event) when event is not background.
  bool mSearchDone;
 
 public: 
  // Public methods if any:

  void SetHList(TObjArray *x){mHList = x;}

  StBemcBeamBckgFinderMaker(const char *name="BemcBckgFinder");
  ~StBemcBeamBckgFinderMaker();
  Int_t Init();
  Int_t Make();
  Int_t InitRun  (int runumber);
  void Clear(const Option_t* = "");
  Int_t Finish();
  void SetTrigger(int x) {mTrigId=x;}
 
  void SetAdcThreshold(int setadc) {mAdcThreshold=setadc;}
  void SetAdcSumThreshold(float setadcsum) {mAdcSumThreshold=setadcsum;}
  void SetPatternLength(int setpattern) {mpattern=setpattern;}
  void SetMaxYesPlots(int setyesplots) {mMaxYesPlots=setyesplots;}
  void SetMaxNoPlots(int setnoplots) {mMaxNoPlots=setnoplots;}
  void GetDecision(int &fDecision,int &eta1, int &phi1, int &eta2, int &patternleng, float &Adcsum);
  const int *GetSoftIdList(){return mPattSoftId;} // 0 is the terminator

  /* Note fDecision =  1  when trigger is of type set and background found.
   *      fDecision =  0  when trigger is of type set and no background found.
   *      fDecision = -1  when trigger is not of type set (not executed).
   */

  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  // Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StBemcBeamBckgFinderMaker.h,v 1.9 2014/08/06 11:43:05 jeromel Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;
  }

  ClassDef(StBemcBeamBckgFinderMaker,0)   //StAF chain virtual base class for Makers
};

#endif

/**********************************************************************
  $Log: StBemcBeamBckgFinderMaker.h,v $
  Revision 1.9  2014/08/06 11:43:05  jeromel
  Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes

  Revision 1.8  2006/06/27 15:41:30  qattan
  *** empty log message ***

  Revision 1.7  2006/06/13 22:01:36  qattan
  *** empty log message ***

  Revision 1.5  2006/06/13 21:42:42  qattan
  *** empty log message ***

  Revision 1.4  2006/06/13 21:26:25  qattan
  *** empty log message ***

  Revision 1.3  2006/05/30 22:38:23  qattan
  check4

  Revision 1.2  2006/05/30 22:21:41  qattan
  *** empty log message ***

  Revision 1.1  2006/05/30 20:08:03  qattan
  start1

 
*/

