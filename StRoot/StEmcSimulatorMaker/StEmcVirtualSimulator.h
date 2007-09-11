#ifndef STAR_StEmcVirtualSimulator
#define STAR_StEmcVirtualSimulator

// $Id: StEmcVirtualSimulator.h,v 1.5 2007/09/11 21:49:14 kocolosk Exp $

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

    /*virtual void     init() = 0;
    virtual void     setPedestal(const UInt_t type, const Float_t pedMean, const Float_t pedRMS) = 0;
    virtual void     setParameters(const Float_t calibCoeff,const UInt_t type, const Float_t pedMean, const Float_t pedRMS, const Float_t gainUnc)=0;
    virtual Double_t getPedestal(const Int_t type, const Double_t pedMean, const Double_t pedRMS) = 0;
    virtual Double_t deductPedestal(const Int_t type, const Int_t adc, const Double_t pedMean) = 0;
    virtual Int_t    getAdc(const Double_t de, const Double_t eta) = 0;
    virtual Float_t  getEnergy() = 0;
    virtual void     print() = 0;*/
    
    virtual void setTables(const StBemcTables *tables)                  = 0;
    virtual void setCalibScale(float scale)                             = 0;
    virtual void setCalibSpread(float spread)                           = 0;
    virtual void setEmbeddingMode(bool flag)                            = 0;
    
    virtual StEmcRawHit* makeRawHit(const StMcCalorimeterHit *mcHit)    = 0;
    
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
