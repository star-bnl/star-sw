/***************************************************************************
 * Author: Subhasis Chattopadhyay 
 ***************************************************************************
 *
 * Description: EMC SMD Input handling
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEmcSmdInput
#define STAR_StEmcSmdInput

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
class StEmcHandleDB;

class StEmcSmdInput {
    
private:
    
protected:
    
public: 
    StEmcSmdInput(StEvent*, StEMCReader*, TDataSet *);
    virtual       ~StEmcSmdInput();
    virtual Int_t  ProcessInput();
    Int_t subtract_pedestals(StEmcHandleDB*);
    Int_t Apply_amp_equalization(StEmcHandleDB*);
    Int_t Apply_etaCorrection(StEmcHandleDB*);
    Int_t fillevent();
protected:
    
private:
    StEvent* mevent;
    StEMCReader* mTheEmcReader;//!
    TDataSet* m_calibdb;
    Float_t TimeBin;
    Float_t m_SMDEADC[120][50];
    Float_t m_SMDPADC[120][10][15];
    ClassDef(StEmcSmdInput, 1)   
	};

#endif 
#endif /* __ROOT__ */
