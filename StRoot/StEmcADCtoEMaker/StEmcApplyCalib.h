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
class StEvent;
class TDataset;
class StEmcHandleDB;
class StEmcCollection;

class StEmcApplyCalib {
    
private:
    
protected:
    
public: 
    StEmcApplyCalib(const StEvent*, const TDataSet*);
    virtual       ~StEmcApplyCalib();
    virtual Int_t  Calibrate();
    Int_t Calibrate_Tower(StEmcHandleDB*,StEmcCollection *);
    Int_t Calibrate_Smd(StEmcHandleDB*,StEmcCollection *);
protected:
    
private:
    StEvent* mevent;
    TDataSet* m_calibdb;
    ClassDef(StEmcApplyCalib, 1)   
	};

#endif 
#endif /* __ROOT__ */
