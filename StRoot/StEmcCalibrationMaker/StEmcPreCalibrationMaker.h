// $Id: StEmcPreCalibrationMaker.h,v 1.1 2001/09/24 13:30:50 suaide Exp $
// $Log: StEmcPreCalibrationMaker.h,v $
// Revision 1.1  2001/09/24 13:30:50  suaide
// Added Effective pedestal calculation and Pre Calibration Maker to
// generate EMC and L3 StEvent objects from Daq file
//
// Revision 1.12  1999/09/24 22:03:09  perev
// Add InitRun & FinishRun to template maker
//
#ifndef STAR_StEmcPreCalibrationMaker
#define STAR_StEmcPreCalibrationMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#endif

class StEmcPreCalibrationMaker : public StMaker 
{
  private: 
  
  protected:    
    
  public: 
                 StEmcPreCalibrationMaker(const char *name="EmcPreCalibration",int daq=0);
    virtual       ~StEmcPreCalibrationMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
  
  private:
    St_DataSet*         mTheEmcData;//!
    int                 mDaq;  // looking for DAQ data or not?
 
   ClassDef(StEmcPreCalibrationMaker, 1)  
};

#endif
