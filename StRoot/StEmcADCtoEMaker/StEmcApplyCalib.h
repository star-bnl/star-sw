/***************************************************************************
 * Author:Subhasis Chattopadhyay 
 ***************************************************************************
 *
 * Description: EMC StEvent Only Input Handling
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEmcApplyCalib
#define STAR_StEmcApplyCalib

#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#endif

#include "TTable.h"  
//#include "tables/St_emcCalSettings_Table.h"
//#include "tables/St_emcCalSummary_Table.h"
#include "tables/St_emcCalibration_Table.h"
//#include "tables/St_emcEqualization_Table.h"
//#include "tables/St_emcMipCalib_Table.h"                                      
class StEMCReader;
class StEmcCollection;
class TDataset;
class StEmcHandleDB;
class StEmcCollection;

class StEmcApplyCalib {
    
public: 
    StEmcApplyCalib(StEmcCollection*, TDataSet*);
    virtual       ~StEmcApplyCalib();
    Int_t  calibrate();
    Int_t  calibrateTower(StEmcHandleDB*,StEmcCollection *);
    Int_t  calibrateSmd(StEmcHandleDB*,StEmcCollection *);
protected:
    
private:
    StEmcCollection *mEmcCollection; //!
    TDataSet        *mCalibDb;       //!
    ClassDef(StEmcApplyCalib, 1)   
};

#endif 
#endif /* __ROOT__ */
