/***************************************************************************
 * Author: Subhasis Chattopadhyay 
 * Description: Handling of EMC Barrel Tower
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEmcTowerInput
#define STAR_StEmcTowerInput

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
class StEvent;
class TDataset;
class StEmcHandleDB;

class StEmcTowerInput {
    
private:
    
protected:
    
public: 
    StEmcTowerInput(const StEvent*, const StEMCReader*,const TDataSet*);
    virtual       ~StEmcTowerInput();
    virtual Int_t  ProcessInput();
    Int_t subtract_pedestals(StEmcHandleDB*);
    Int_t Apply_equalization(StEmcHandleDB*);
    Int_t fillevent();
protected:
    
private:
    StEvent* mevent;
    StEMCReader* mTheEmcReader;//!
    TDataSet* m_calibdb;
    Float_t m_TowerADC[120][20][2];
    ClassDef(StEmcTowerInput, 1)   
	};

#endif 
#endif /* __ROOT__ */
