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

class StFmsDbMaker;
class StFmsCollection;
class StMuFmsCollection;

#include "StMaker.h"

class StFmsHitMaker : public StMaker {
public:
  StFmsHitMaker(const char* name = "StFmsHitMaker");
  ~StFmsHitMaker();

  void   Clear(Option_t* option = "");
  Int_t  Init();
  Int_t  InitRun(Int_t runNumber);
  Int_t  Make();
  Int_t  Finish();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFmsHitMaker.h,v 1.2 2014/08/06 11:43:15 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

private:
  StFmsDbMaker*      mFmsDbMaker;    //! DB maker provides FMS geometry and calibration data
  StFmsCollection*   mFmsCollection; /// FMS data structure for StEvent
  StMuFmsCollection* mMuFmsColl;     //! FMS data structure for StMuEvent

  ClassDef(StFmsHitMaker,1);
};

#endif
