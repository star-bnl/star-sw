/*********************************************************************
 * $Id: StBemcBeamBckgFinderMaker.h,v 1.1 2006/05/30 20:08:03 qattan Exp $
 * \author Issam Qattan , IUCF, 2006 
 *********************************************************************
 * Descripion:
 * ????
 *********************************************************************
 */

#ifndef STAR_St_StBemcBeamBckgFinderMaker_h
#define STAR_StBemcBeamBckgFinderMaker_h

 
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
  enum{mMaxevtH=2};
  TH1F *mhisto[mMaxH];
  TH2F *mevtH[mMaxevtH];

  enum{mxSoftId = 4800}; 
  enum{mxEta=40, mxPhi=120};
  enum{xmlocate = 10};

   
  Int_t GetNewStatus(int id, int rdo, int run);
  void FillAdc(float iadc,int ieta,int iphi,int isoft);
  Int_t CheckPatternType3(int &etaBegin, int &phiBegin, int &etaEnd, int &patternLength, float &sumAdc);
  void PlotOneEvent();

  float mdb_btowPed[mxSoftId];
  int mdb_btowStat[mxSoftId];

  int mdb_btowetaBin[mxSoftId];
  int mdb_btowphiBin[mxSoftId];
   
  int mdb_btowRdo[mxSoftId];
  int mdb_btowSoftId[mxSoftId];

  float mAdcArray[mxPhi][mxEta];

  int mInpEve;// input events counter
  int mAccEve;// accepted events counter
  int mTrigId;
  int mRunNumber;
  int mDecision;
  char mLocation[xmlocate];
  int metaBegin;
  int metaEnd;
  int mphiBegin;
  int mpatternLength;
  float msumAdc;

  int mAdcThreshold;      //used to set adc threshold
  float mAdcSumThreshold; //used to set adc sum threshold
  int mpattern;           //used to set pattern length needed
  int mMaxYesPlots;    //Maximum number of postscript files to produce (file/event) when event is background.
  int mMaxNoPlots;     //Maximum number of postscript files to produce (file/event) when event is not background.
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

  /* Note fDecision =  1  when trigger is of type set and background found.
   *      fDecision =  0  when trigger is of type set and no background found.
   *      fDecision = -1  when trigger is not of type set (not executed).
   */

  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  // Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StBemcBeamBckgFinderMaker.h,v 1.1 2006/05/30 20:08:03 qattan Exp $ built "__DATE__" "__TIME__ ;
    return cvs;
  }

  ClassDef(StBemcBeamBckgFinderMaker,0)   //StAF chain virtual base class for Makers
};

#endif

/**********************************************************************
  $Log: StBemcBeamBckgFinderMaker.h,v $
  Revision 1.1  2006/05/30 20:08:03  qattan
  start1

 
*/

