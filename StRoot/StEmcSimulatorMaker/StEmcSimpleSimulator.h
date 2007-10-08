#ifndef STAR_StEmcSimpleSimulator
#define STAR_StEmcSimpleSimulator

// $Id: StEmcSimpleSimulator.h,v 1.8 2007/10/08 15:28:37 kocolosk Exp $

#include "TRandom3.h"

#include "StEmcVirtualSimulator.h"
#include "StEvent/StEnumerations.h"

class StBemcTables;
class StEmcGeom;

/*****************************************************************************
 * @class StEmcSimpleSimulator
 * @author A.Pavlinov -> A.Suaide -> A.Kocoloski
 *
 * This class provides simple transition from deposit energy to ADC using 
 * ADC's scale, energy scale and sampling fraction function. It is correct for 
 * case when the main fluctuations come from shower fluctuations. Possible
 * modes of operation include
 * 
 * kTestMode:  ADC = 0 and energy = dE * sampling fraction
 * kSimpleMode:  Simple transition using DB tables and sampling fraction
 *****************************************************************************/
class StEmcSimpleSimulator : public StEmcVirtualSimulator
{
protected:
    StDetectorId mDetectorId;    
    StEmcSimulatorMode mMode;
    
    const StBemcTables* mTables;
    const StEmcGeom* mGeom;
    
    double  mSF[3];
    double  mMaxADC;
    double  mMaxADCSpread;
    
    bool    mEmbeddingMode;
    double  mCalibScale;
    double  mCalibSpread;
    
    TRandom3  mRandom;

    double samplingFraction(double eta);
    
public:
    StEmcSimpleSimulator(StDetectorId det, StEmcSimulatorMode mode);
    virtual ~StEmcSimpleSimulator() { /* nothing */ }
        
    /// DB access for peds, calib coeffs, etc.
    void setTables(const StBemcTables *tables) { mTables = tables; }
    
    /// scale the hit energy using a Gaussian distribution with this mean
    void setCalibScale(float scale) { mCalibScale = scale; }
    
    /// scale the hit energy using a Gaussian distribution with this width
    void setCalibSpread(float spread) { mCalibSpread = spread; }
    
    /// if true, don't add pedestal noise to the hit
    void setEmbeddingMode(bool flag) { mEmbeddingMode = flag; }
    
    /// mean value for maximum possible ADC
    void setMaximumAdc(double adc) { mMaxADC = adc; }
    
    /// max ADC will be calculated by sampling a Gaussian with this width
    void setMaximumAdcSpread(double spread) { mMaxADCSpread = spread; }
    
    /// workhorse function
    virtual StEmcRawHit* makeRawHit(const StMcCalorimeterHit *mcHit);
    
    ClassDef(StEmcSimpleSimulator, 2)
};
#endif

/*****************************************************************************
 *  $Log: StEmcSimpleSimulator.h,v $
 *  Revision 1.8  2007/10/08 15:28:37  kocolosk
 *  setMaximumAdc(Spread) methods allow for better simulation of BSMD ADC response
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2507.html
 *
 *  Revision 1.7  2007/09/11 21:56:07  kocolosk
 *  remove a couple of unused variables
 *
 *  Revision 1.6  2007/09/11 21:49:14  kocolosk
 *  complete overhaul of the BEMC simulator
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2486.html
 *
 *  Revision 1.5  2005/03/21 21:36:39  suaide
 *  fixed problem with chain
 *
 *  Revision 1.4  2004/08/06 13:24:48  suaide
 *  New features added and fixed some bugs in the database
 *
 *  Revision 1.3  2003/09/23 15:19:51  suaide
 *  fixed bugs and modifications for embedding
 *
 *  Revision 1.2  2002/06/04 16:09:36  pavlinov
 *  added option with DB(pedestal ans calibration  coefficients
 *
 *  Revision 1.1  2000/10/23 22:53:14  pavlinov
 *  First working C++ version
 *****************************************************************************/
