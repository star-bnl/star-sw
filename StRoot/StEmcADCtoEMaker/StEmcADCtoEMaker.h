/***************************************************************************
 *
 * $Id: StEmcADCtoEMaker.h,v 1.1 2001/07/17 00:14:37 perev Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: RICH offline software
 *              StRchMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEreadMaker
#define STAR_StEreadMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

//#define rCH_DEBUG 1
//#define rCH_HISTOGRAM 1

//#include "StRichDisplayActivate.h"

#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#endif

//#ifdef RCH_HISTOGRAM
//#include "TFile.h"
//#include "TH1.h"
//#include "TNtuple.h"
//#endif
class StDAQReader;
class StEMCReader;
class StEvent;

class StEmcADCtoEMaker : public StMaker {
    
private:
    
protected:
    
public: 
    StEmcADCtoEMaker(const char *name="Eread", int daq=0);
    virtual       ~StEmcADCtoEMaker();
    virtual Int_t  Init();
    virtual Int_t  Make();
    //    virtual void   PrintInfo();
    virtual Int_t  Finish();

protected:
    
private:
    StDAQReader*           mTheDataReader;//!
    StEMCReader* mTheEmcReader;//!
    St_DataSet*            mTheEmcData;//!
    StEvent* mevent;
    TDataSet* m_calibdb;
    int mDaq;  // looking for DAQ data or not?

    // the following is a ROOT macro  that is needed in all ROOT code
    ClassDef(StEmcADCtoEMaker, 1)   
	};

#endif 
#endif 

