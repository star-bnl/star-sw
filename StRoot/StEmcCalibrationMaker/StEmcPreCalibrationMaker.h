// $Id: StEmcPreCalibrationMaker.h,v 1.3 2001/10/26 21:00:33 suaide Exp $
// $Log: StEmcPreCalibrationMaker.h,v $
// Revision 1.3  2001/10/26 21:00:33  suaide
// Many modifications to optimize for real data
//
// Revision 1.2  2001/10/17 13:51:31  suaide
// new modifications to work with real data
//
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

#include "TH2.h"
#include "TH2.h"

class StEvent;
class StEmcCollection;

class StEmcPreCalibrationMaker : public StMaker 
{
  private: 
    StEvent         *currevent;//!   
    StEmcCollection *emccol; //!
    TH2F            *etaDistr; //!
    TH2F            *pDistr; //!
    TH2F            *pDistr1; //!
  protected:    
    
  public: 
                 StEmcPreCalibrationMaker(const char *name="EmcPreCalibration",int daq=0);
    virtual       ~StEmcPreCalibrationMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    //        void  Clear(Option_t *option="");
  private:
    St_DataSet*         mTheEmcData;//!
    int                 mDaq;  // looking for DAQ data or not?
 
   ClassDef(StEmcPreCalibrationMaker, 1)  
};

#endif
