#ifndef __controlADCtoE_st__
#define __controlADCtoE_st__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
struct controlADCtoE_st {
    short DeductPedestal[8];    /* switch for deducting pedestal */
    short Calibration[8];       /* switch for calibration */    
    float CutOff[8];            /* cutoff value. See CutOffType for more information*/
    short CutOffType[8];        /* cutoff type (0 = none, 1 = pedestal RMS, 2 = energy) */
    short OnlyCalibrated[8];    /* save only calibrated hits */
    short CheckStatus[8];       /* save only if status is ok */
    short CheckCrate[8];        /* save only if crate is ok */
    short messLimit;            /* limit for warning message */
};
class St_controlADCtoE : public TTable {
 public:
  ClassDefTable(St_controlADCtoE,controlADCtoE_st)
  ClassDef(St_controlADCtoE,1) //C++ container for chain/makers status 
};

#endif /* __controlADCtoE_st__ */
