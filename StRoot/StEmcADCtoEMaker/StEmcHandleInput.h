/***************************************************************************
 * Author: Subhasis Chattopadhyay 
 ***************************************************************************
 *
 * Description: EMC INput Handing Interface
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEmcHandleInput
#define STAR_StEmcHandleInput

#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#endif

class StEMCReader;
class StEmcCollection;
class TDataSet;

class StEmcHandleInput {
    
public: 
    StEmcHandleInput(StEmcCollection*, StEMCReader*, TDataSet*);
    virtual       ~StEmcHandleInput();
    Int_t  processInput();
    void clear();
protected:
    
private:
    StEmcCollection *mEmcCollection; //!
    StEMCReader *mTheEmcReader;      //!
    TDataSet    *mCalibDb;           //!
    ClassDef(StEmcHandleInput, 1)   
};

#endif 
#endif
