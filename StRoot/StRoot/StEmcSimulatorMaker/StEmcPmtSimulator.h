#ifndef STAR_StEmcPmtSimulator
#define STAR_StEmcPmtSimulator

// $Id: StEmcPmtSimulator.h,v 1.7 2007/09/11 21:49:13 kocolosk Exp $

#include "StEmcSimpleSimulator.h"
#include "StPmtSignal.h"
#include "StEvent/StEnumerations.h"

class StEmcRawHit;
class StMcCalorimeterHit;

/*****************************************************************************
 * @class StEmcPmtSimulator.h
 * @author A.Pavlinov -> A.Suaide -> A.Kocoloski
 *
 * This class simulates EMC response with accounting primary
 * and secondary photostatistics. It supports the modes for
 * simple simulator also.
 *
 * kPrimaryOnlyMode: Taking into account only primary photostatistics.
 *
 * kPrimarySecondaryFullMode: Taking into account primary and secondary
 * photostatistics(full simulation);
 *
 * kPrimarySecondaryFastMode: Taking into account primary and secondary
 * photostatistics(fast simulation);
 *****************************************************************************/
class StEmcPmtSimulator : public StEmcSimpleSimulator
{
protected:
    StPmtSignal mPmtSignal;
    StPmtSignal::simulatorVersion mVer;
    
    double mMipPhotoElectrons;
    double mMipEnergyDeposit;

public:
    StEmcPmtSimulator(StDetectorId det, StEmcSimulatorMode mode);
    virtual ~StEmcPmtSimulator() { /* nothing */ };
    
    StEmcRawHit* makeRawHit(const StMcCalorimeterHit *mcHit);
    
    ClassDef(StEmcPmtSimulator, 2)
};
#endif

/*****************************************************************************
 *  $Log: StEmcPmtSimulator.h,v $
 *  Revision 1.7  2007/09/11 21:49:13  kocolosk
 *  complete overhaul of the BEMC simulator
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2486.html
 *
 *  Revision 1.6  2007/01/22 19:13:40  kocolosk
 *  use STAR logger for all output
 *
 *  Revision 1.5  2005/03/21 21:36:39  suaide
 *  fixed problem with chain
 *
 *  Revision 1.4  2004/08/06 13:24:47  suaide
 *  New features added and fixed some bugs in the database
 *
 *  Revision 1.3  2003/09/23 15:19:46  suaide
 *  fixed bugs and modifications for embedding
 *
 *  Revision 1.2  2002/06/04 16:09:35  pavlinov
 *  added option with DB(pedestal ans calibration  coefficients
 *
 *  Revision 1.1  2000/10/23 22:53:14  pavlinov
 *  First working C++ version
 *****************************************************************************/
