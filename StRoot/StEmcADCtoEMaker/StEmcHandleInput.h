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
class StEvent;
class TDataSet;

class StEmcHandleInput {
    
private:
    
protected:
    
public: 
    StEmcHandleInput(StEvent*, StEMCReader*, TDataSet*);
    virtual       ~StEmcHandleInput();
    virtual Int_t  ProcessInput();
    void clear();
protected:
    
private:
    StEvent* mevent;
    StEMCReader* mTheEmcReader;
    TDataSet* m_calibdb;
    ClassDef(StEmcHandleInput, 1)   
	};

#endif 
#endif
