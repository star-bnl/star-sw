/***************************************************************************
 *
 * $Id: StZdcVertexMaker.cxx,v 1.4 2003/09/02 17:59:21 perev Exp $
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
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "ZdcCalPars.h"
#include "tables/St_ZdcCalPars_Table.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "StDaqLib/TRG/trgStructures.h"


//#include "StEventMaker/StEventMaker.h"

static const char rcsid[] = "$Id: StZdcVertexMaker.cxx,v 1.4 2003/09/02 17:59:21 perev Exp $";

ClassImp(StZdcVertexMaker)

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
    // Don't delete the canvas, so that it stays if needed
    
    StMaker::Clear();
}

//_________________________________________________
Int_t StZdcVertexMaker::Finish()
{

    return StMaker::Finish();
}

//_________________________________________________
Int_t StZdcVertexMaker::Init()
{ 

    // Getting Database info

    TDataSet* p = GetDataBase("Calibrations/trg");
    if (!p)
    {
        gMessMgr->Error() << "StZdcVertexMaker::Init():  GetDataBase() in ZdcVertexMaker did not find DB ." << endm;
        return kStErr;
    }
    
    St_DataSetIter       dblocal_calibrations(p);
    St_ZdcCalPars * t  = (St_ZdcCalPars *)dblocal_calibrations("ZdcCalPars");
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
    
    return StMaker::Init();
}

//_________________________________________________
Int_t StZdcVertexMaker::Make()
{
/*
    //
    //  Get StEvent
    //
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    if (!event) {
        return kStOK;
    }
*/
    //
    //  Get ZDC trigger
    //

  //TDataSet *triggerDS = GetDataSet("trg");
  //if (!triggerDS)
  // {
  //    gMessMgr->Error() << "StZdcVertexMaker::Make():  GetDataSet() in ZdcVertexMaker did not find Data Set ." << endm;
  //   return kStErr;
  // }

    // St_DataSetIter triggerI(triggerDS);
    St_dst_TrgDet *triggertable = (St_dst_TrgDet *)GetDataSet("TrgDet");         
    dst_TrgDet_st *tt =(dst_TrgDet_st *) triggertable->GetTable();

/*
    StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
    if (!theTriggers) 
    {
        gMessMgr->Warning() << "StZdcVertexMaker::Make(): no trigger data available." << endm;
        return kStOK; //checking for availability of data
    }

    StZdcTriggerDetector &theZdc = theTriggers->zdc();
 
    float adcE = theZdc.adc(15);
    float adcW = theZdc.adc(12);
    float tdcE = theZdc.adc(8);
    float tdcW = theZdc.adc(9);

*/
    float adcE = tt->adcZDC[15];
    float adcW = tt->adcZDC[12];
    float tdcE = tt->adcZDC[8];
    float tdcW = tt->adcZDC[9];
    
    float VertexZ = ((tdcW-(mWAP0+(mWAP1*adcW)+(mWAP2*::pow(adcW,2))+(mWAP3*::pow(adcW,3))))-
                     (tdcE-(mEAP0+(mEAP1*adcE)+(mEAP2*::pow(adcE,2))+(mEAP3*::pow(adcE,3)))))*mVPAR + mOFF;

    //
    //  Store VertexZ 
    //
        //theZdc.setVertexZ(VertexZ);
    
    tt->ZDCvertexZ = VertexZ;

    return kStOK;
}










