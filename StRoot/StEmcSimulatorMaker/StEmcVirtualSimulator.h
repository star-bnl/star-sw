#ifndef STAR_StEmcVirtualSimulator
#define STAR_StEmcVirtualSimulator

// $Id: StEmcVirtualSimulator.h,v 1.7 2007/10/08 15:28:39 kocolosk Exp $

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

class StEmcRawHit;
class StMcCalorimeterHit;
class StBemcTables;

/*****************************************************************************
 * @class StEmcVirtualSimulator
 * @author A.Pavlinov -> A.Suaide -> A.Kocoloski
 *
 * This class provides the interface for EMC simulator.  It is an abstract 
 * class which is inherited by any simulator class (for example 
 * StEmcSimpleSimulator or StEmcPmtSimulator).
 *****************************************************************************/
class StEmcVirtualSimulator
{
public:
    StEmcVirtualSimulator();
    virtual ~StEmcVirtualSimulator();

    virtual void setTables(const StBemcTables *tables)                  = 0;
    virtual void setCalibScale(float scale)                             = 0;
    virtual void setCalibSpread(float spread)                           = 0;
    virtual void setEmbeddingMode(bool flag)                            = 0;
    virtual void setMaximumAdc(double adc)                              = 0;
    virtual void setMaximumAdcSpread(double spread)                     = 0;
    
    virtual StEmcRawHit* makeRawHit(const StMcCalorimeterHit *mcHit)    = 0;
    
    /// possible modes of operation for simulators.  Currently StEmcSimpleSimulator
    /// only implements the first two, while StEmcPmtSimulator implements all 5.
    enum StEmcSimulatorMode {   kTestMode, 
                                kSimpleMode, 
                                kPrimaryOnlyMode, 
                                kPrimarySecondaryFullMode, 
                                kPrimarySecondaryFastMode 
                            };

    ClassDef(StEmcVirtualSimulator, 1)   // Abstract class for Emc simulator
};
#endif

/*****************************************************************************
 *  $Log: StEmcVirtualSimulator.h,v $
 *  Revision 1.7  2007/10/08 15:28:39  kocolosk
 *  setMaximumAdc(Spread) methods allow for better simulation of BSMD ADC response
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2507.html
 *
 *  Revision 1.6  2007/09/11 21:58:10  kocolosk
 *  small cleanup and extra documentation
 *
 *  Revision 1.5  2007/09/11 21:49:14  kocolosk
 *  complete overhaul of the BEMC simulator
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2486.html
 *
 *  Revision 1.4  2005/03/21 21:36:39  suaide
 *  fixed problem with chain
 *
 *  Revision 1.3  2004/08/06 13:24:48  suaide
 *  New features added and fixed some bugs in the database
 *
 *  Revision 1.2  2002/06/04 16:09:37  pavlinov
 *  added option with DB(pedestal ans calibration  coefficients
 *
 *  Revision 1.1  2000/10/23 22:53:15  pavlinov
 *  First working C++ version
 *****************************************************************************/
