// $Id: StEmcPreCalibrationMaker.h,v 1.8 2003/09/10 19:47:11 perev Exp $
// $Log: StEmcPreCalibrationMaker.h,v $
// Revision 1.8  2003/09/10 19:47:11  perev
// ansi corrs
//
// Revision 1.7  2002/12/02 21:30:57  suaide
// New EMC calibration maker
//
// Revision 1.5  2001/12/29 20:33:51  suaide
// added documentation
//
// Revision 1.4  2001/12/28 21:31:09  suaide
// Added documentation
//
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

/*!\class StEmcPreCalibrationMaker
\author Alexandre A. P. Suaide

This class gets EMC hits and L3 tracks from DAQ file and generates a StEvent object
*/

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
                 StEmcPreCalibrationMaker(const char *name="EmcPreCalibration",int daq=0);///< Default constructor 
    virtual       ~StEmcPreCalibrationMaker();///< Default destructor 
    virtual Int_t Init();///< Init method
    virtual Int_t Make();///< Make method. Process each event 
    virtual Int_t Finish();///< Finish method
    //        void  Clear(Option_t *option="");
  private:
    St_DataSet*         mTheEmcData;//!
    int                 mDaq;  // looking for DAQ data or not?
 
   ClassDef(StEmcPreCalibrationMaker,0)  
};

#endif
