/***************************************************************************
 * Author: Subhasis Chattopadhyay 
 ***************************************************************************
 *
 * Description: EMC Only StEvent Input Handling
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEmcnoDaqInput
#define STAR_StEmcnoDaqInput

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

class StEmcnoDaqInput {
    
private:
    
protected:
    
public: 
    StEmcnoDaqInput(StEvent*,StEMCReader*,TDataSet*);
    virtual       ~StEmcnoDaqInput();
    virtual Int_t  ProcessInput();
protected:
    
private:
    StEvent* mevent;
    StEMCReader* mTheEmcReader;//!
    TDataSet* m_calibdb;
    ClassDef(StEmcnoDaqInput, 1)   
	};

#endif 
#endif /* __ROOT__ */
