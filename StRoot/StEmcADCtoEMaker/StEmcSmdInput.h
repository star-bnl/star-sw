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
class StEmcCollection;
class TDataSet;
class StEmcHandleDB;

class StEmcSmdInput {
        
public: 
    StEmcSmdInput(StEmcCollection*, StEMCReader*, TDataSet *);
    virtual       ~StEmcSmdInput();
    Int_t  processInput();
    Int_t  subtractPedestals(StEmcHandleDB*);
    Int_t  applyAmpEqualization(StEmcHandleDB*);
    Int_t  applyEtaCorrection(StEmcHandleDB*);
    Int_t  fillEmcHitsCollection();
protected:
    
private:
    StEmcCollection *mEmcCollection; //!
    StEMCReader *mTheEmcReader;      //!
    TDataSet    *mCalibDb;           //!
    //    Float_t mTimeBin;
    Float_t mSMDEADC[120][50];
    Float_t mSMDPADC[120][10][15];
    ClassDef(StEmcSmdInput, 1)   
};

#endif 
#endif /* __ROOT__ */
