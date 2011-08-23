//
//  $Id
//  $Log
// \author Anselm Vossen (avossen@indiana.edu)
//

#include "DAQ_READER/daq_det.h"
#include "DAQ_FGT/

class StFgtRawMaker : public StRTSBaseMaker
{

 protected:
  StEvent*  mEvent;
  StFgtDB*  mDb;
  fgt_adc_t *mFgtRawData;


 public: 
  StFgtRawMaker(const char* name="FgtRaw");
  virtual ~StFgtRawMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t runnumber);
  virtual Int_t Make();
  virtual Int_t Finish();

}
