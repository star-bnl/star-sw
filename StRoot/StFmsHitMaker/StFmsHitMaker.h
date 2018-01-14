#ifndef ST_FMS_HIT_MAKER_H
#define ST_FMS_HIT_MAKER_H

/*!
 *                                                                     
 * \class  StFmsHitMaker
 * \author Jingguo Ma
 * \date   2009/12/12
 * \brief  StFmsHitMaker
 *
 *
 * This maker makes Fms Hit, Cluster and photon lists and fill them in to
 * StFmsCollection in StEvent
 *
 *
 */                                                                      

//modified by Yuxi Pan 03/28/2013
class StFmsDbMaker;
class StFmsCollection;
class StMuFmsCollection;

#include "TMatrix.h"
#include "StMaker.h"
using namespace std;

class StFmsHitMaker : public StMaker {
public:
  StFmsHitMaker(const char* name = "StFmsHitMaker");
  ~StFmsHitMaker();

  void   Clear(Option_t* option = "");
  Int_t  Init();
  Int_t  InitRun(Int_t runNumber);    //called by StMaker when switch to a new run#
  Int_t  Make();
  Int_t  Finish();

  //For Time Dep Corr based on LED
  void SetTimeDepCorr(int correct=0)  {mTimeDepCorr=correct;} //Use Time Dep. Corrections                      
  void SetTowerRej(int rej=1)         {mTowerRej=rej;} //Use Tower Rejection mode  //! 

  //Correcting ADC zero suppression done wrong online
  void SetCorrectAdcOffByOne(int v=1) {mCorrectAdcOffByOne=v;}

  //! 0=search StTriggerData from StTriggerDataMaker/StEvent/MuDst and apply new DB (default)                      
  //! 1=Read hits,clusters,points from Mudst, new calibration from DB will NOT be applied!!!!
  void SetReadMuDst(int v=1) {mReadMuDst=v;} //Set to read MuDst 

  TMatrix**	GetEnergyMatrices();
  Bool_t	Legal(Int_t iew,Int_t nstb,Int_t row0,Int_t col0);
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFmsHitMaker.h,v 1.5 2018/01/12 20:05:19 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

private:
  StFmsDbMaker*      mFmsDbMaker;    //! DB maker provides FMS geometry and calibration data
  StFmsCollection*   mFmsCollection; //! FMS data structure for StEvent
  StMuFmsCollection* mMuFmsColl;     //! FMS data structure for StMuEvent  
  Int_t mCorrectAdcOffByOne=1;       //! Make correction for ADC from QT being off by 1
  Int_t readMuDst();  
  Int_t mReadMuDst=0;                //!
  Int_t mTimeDepCorr=0;              //!
  Int_t mTowerRej=0;                 //!
  Int_t mCurrentRunNumber=0;         //!
  ClassDef(StFmsHitMaker,1);
};

#endif
