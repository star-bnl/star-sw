/***************************************************************************
 * Author: Subhasis Chattopadhyay 
 * Description: Handling of EMC Barrel Tower
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEmcTowerInput
#define STAR_StEmcTowerInput

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

class StEmcTowerInput {
    
public: 
    StEmcTowerInput(StEmcCollection*, StEMCReader*,TDataSet*);
    virtual       ~StEmcTowerInput();
    Int_t  processInput();
    Int_t  subtractPedestals(StEmcHandleDB*);
    Int_t  applyEqualization(StEmcHandleDB*);
    Int_t  fillEmcHitsCollection();
protected:
    
private:
    StEmcCollection *mEmcCollection; //!
    StEMCReader *mTheEmcReader;         //!
    TDataSet    *mCalibDb;              //!
    Float_t      mTowerADC[120][20][2]; //!
    ClassDef(StEmcTowerInput, 1)   
};

#endif 
#endif /* __ROOT__ */
