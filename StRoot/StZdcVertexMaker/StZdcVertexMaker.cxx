/***************************************************************************
 *
 * $Id: StZdcVertexMaker.cxx,v 1.10 2016/12/12 17:18:04 smirnovd Exp $
 *
 * Author:  Johan E. Gonzalez, August 2001
 ***************************************************************************
 *
 * Description: This Maker makes use of parameters obtained after calibrating the ZDC's to
 * calculate the z-vertex as determined by the ZDC's.  The calibration
 * parameters are retrieved from a database in init(), then the z-vertex is
 * calculated in Make() by using these parameters in conjunction with ADC and
 * TDC signals. 
 *
 ***************************************************************************
 *
 * $Log: StZdcVertexMaker.cxx,v $
 * Revision 1.10  2016/12/12 17:18:04  smirnovd
 * Removed outdated ClassImp ROOT macro
 *
 * Revision 1.9  2009/01/26 15:11:05  fisyak
 * Clean up access to Calibrations/trg/ZdcCalPars table
 *
 * Revision 1.8  2007/04/28 17:57:28  perev
 * Redundant StChain.h removed
 *
 * Revision 1.7  2004/09/09 22:45:56  fisyak
 * Add protection for missing StTriggerData
 *
 * Revision 1.6  2004/03/02 15:52:16  lbarnby
 * Completely updated to use StTriggerData from StEvent
 *
 * Revision 1.5  2004/01/14 22:57:29  fisyak
 * Add declaration of InitRun
 *
 * Revision 1.4  2003/09/02 17:59:21  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2001/10/05 13:39:56  jeromel
 * Changes made by Lee Barnby.
 *
 * Revision 1.2  2001/08/31 19:07:36  macross
 * Modified code to retrieve ADC and TDC pulses from TrgDet table
 *
 **************************************************************************/
#include "StZdcVertexMaker.h"

#include <stdlib.h>
#include <math.h>

#include "StEventTypes.h"
#include "StMessMgr.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "ZdcCalPars.h"
#include "tables/St_ZdcCalPars_Table.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "StDaqLib/TRG/trgStructures.h"


//#include "StEventMaker/StEventMaker.h"

static const char rcsid[] = "$Id: StZdcVertexMaker.cxx,v 1.10 2016/12/12 17:18:04 smirnovd Exp $";


//_________________________________________________
StZdcVertexMaker::StZdcVertexMaker(const char *name, const char *title):StMaker(name,title)
{
    mEAP0 = 0;
    mEAP1 = 0;
    mEAP2 = 0;
    mEAP3 = 0;
    mWAP0 = 0;
    mWAP1 = 0;
    mWAP2 = 0;
    mWAP3 = 0;
    mVPAR = 0;
    mOFF =  0; 
}

//_________________________________________________
StZdcVertexMaker::~StZdcVertexMaker()
{
    //  StZdcVertexMaker Destructor

}

//_________________________________________________
void StZdcVertexMaker::Clear(const char*)
{
    // StZdcVertexMaker - Clear,
        
    StMaker::Clear();
}

//_________________________________________________
Int_t StZdcVertexMaker::Finish()
{

    return StMaker::Finish();
}

//_________________________________________________
Int_t StZdcVertexMaker::Init() {return StMaker::Init(); }
//_________________________________________________
Int_t StZdcVertexMaker::InitRun(int runumber) 
{ 

    // Getting Database info
    St_ZdcCalPars * t  = (St_ZdcCalPars *)GetDataBase("Calibrations/trg/ZdcCalPars");
    if (! t) {
      gMessMgr->Error() << "StZdcVertexMaker::Init():  in ZdcVertexMaker did not find ZdcCalPars." << endm;
      return kStErr;
    }
   
    ZdcCalPars_st* s = (ZdcCalPars_st*)t->GetArray();
    
    mEAP0 = s[0].EAP0;
    mEAP1 = s[0].EAP1;
    mEAP2 = s[0].EAP2;
    mEAP3 = s[0].EAP3;
    mWAP0 = s[0].WAP0;
    mWAP1 = s[0].WAP1;
    mWAP2 = s[0].WAP2;
    mWAP3 = s[0].WAP3;
    mVPAR = s[0].VPAR;
    mOFF = s[0].OFF;
    
    return kStOK;
}

//_________________________________________________
Int_t StZdcVertexMaker::Make()
{

    //
    //  Get StEvent
    //
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    if (!event) {
      gMessMgr->Error() << "StZdcVertexMaker::Make() : unable to get StEvent." << endm;
        return kStOK;
    }

    //
    //  Get trigger data
    //
    StTriggerData *td=event->triggerData();
    if (!td) {
      gMessMgr->Error() << "StZdcVertexMaker::Make() : unable to get StTriggerData." << endm;
      return kStOK;
    }

    // If change to getting StTriggerData before StEvent created then
    // everything in Make() function above here should be changed but 
    // lines below can be preserved.

    //
    // Get appropriate data
    //
    float adcE = td->zdcAttenuated(east,0);
    float adcW = td->zdcAttenuated(west,0);
    float tdcE = td->zdcTDC(east,0);
    float tdcW = td->zdcTDC(west,0);

    //
    // Vertex algorithm
    //
    float VertexZ = ((tdcW-(mWAP0+(mWAP1*adcW)+(mWAP2*::pow(adcW,2))+(mWAP3*::pow(adcW,3))))-
                     (tdcE-(mEAP0+(mEAP1*adcE)+(mEAP2*::pow(adcE,2))+(mEAP3*::pow(adcE,3)))))*mVPAR + mOFF;

    //
    //  Store VertexZ 
    //
    td->setZdcVertexZ(VertexZ);

    return kStOK;
}










