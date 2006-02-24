/*!\class StBemcData
\author Alexandre A. P. Suaide
 
This class does the hit selection and calibration for the
Barrel EMC. It uses MuDST or StEvent structures as input. The
output is a StEmcCollection filled with StEmcRawHits.
*/

#ifndef STAR_StBemcData
#define STAR_StBemcData

#include "TObject.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/defines.h"
#include "TDataSet.h"

class StEvent;
class StEmcCollection;
class StEmcRawData;
class StMuEmcCollection;

class StBemcData : public StBemcRaw
{
protected:
public:
    StBemcData(); ///< StBemcData constructor
    virtual                   ~StBemcData(); ///< StBemcData destructor
    Bool_t                    make(TDataSet*,StEvent*); ///< Make the BEMC detector from DAQ
    Bool_t                    make(StEmcRawData*,StEvent*); ///< Make the BEMC detector from StEmcRaw
    Bool_t                    make(StEmcCollection*,StEvent*); ///< Make the BEMC detector from StEmcCollection
    Bool_t                    make(StMuEmcCollection*,StEvent*); ///< Make the BEMC detector from MuDST

    ClassDef(StBemcData, 2)
};

#endif
