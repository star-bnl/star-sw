#include "StEStructSupport.h"
#include "TH2D.h"
#include "TMinuit.h"

// Want to use TMinuit to find scale factor for \rho_{ref} so that
// \Delta\rho = \rho_{sib} - scale \rho_{ref} is minimized.
// This is useful for YtYt and SYtDYt.
//
// TMinuit requires a static external function so we can't use
// member data. Use an object of this class to hold information
// about whar to fit.
class StEStructMinimizeNegative : public TObject {

public:
    StEStructMinimizeNegative();   
    virtual ~StEStructMinimizeNegative();

    StEStructSupport *mSupport;
    int               mChargeType;
    int               mCorrType;
    double            mLambda;


    ClassDef(StEStructMinimizeNegative,1)
};
