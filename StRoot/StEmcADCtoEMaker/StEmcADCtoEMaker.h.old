 /* Author: Subhasis Chattopadhyay, WSU 
 ***************************************************************************
 *
 * Description: EMC Calibration Maker
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEreadMaker
#define STAR_StEreadMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#endif
#include "tables/St_controlADCtoE_Table.h"

class StDAQReader;
class StEMCReader;
class StEvent;
class StEmcCollection;

class StEmcADCtoEMaker : public StMaker {
    
private:
    
protected:
    
public: 
    StEmcADCtoEMaker(const char *name="Eread", int daq=0);
    virtual       ~StEmcADCtoEMaker();
    virtual Int_t  Init();
    virtual Int_t  Make();
    virtual Int_t  Finish();
    virtual void   Clear(Option_t *option="");

    static controlADCtoE_st *getControlTable() 
    {return mControlMaker->GetTable();}

    StEmcCollection *getEmcCollection();
    void  clearStEventStaf();
    void  print(); // *MENU* 
    virtual void  Browse(TBrowser* b);

protected:
    
private:
    static St_controlADCtoE *mControlMaker; //!
    static controlADCtoE_st *mControlTable; //!

    StDAQReader     *mTheDataReader; //!
    StEMCReader     *mTheEmcReader;  //!
    TDataSet        *mTheEmcData;    //!
    StEvent         *mEvent;         //!
    StEmcCollection *mEmcCollection; //!
    TDataSet        *mCalibDb;       //!
    Int_t mDaq;                      //! looking for DAQ data or not?

    // the following is a ROOT macro  that is needed in all ROOT code
    ClassDef(StEmcADCtoEMaker, 1)   
};

#endif 
#endif 

