/***************************************************************************
 *
 * $Id: StRTpcElectronics.h,v 1.9 2007/08/04 00:38:03 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Root implementation of Tpc Electronics interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcElectronics.h,v $
 * Revision 1.9  2007/08/04 00:38:03  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.8  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.7  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCELECTRONICS__
#define __STRTPCELECTRONICS__
#include "StTpcElectronicsI.h"
#include "tables/St_tpcElectronics_Table.h"

class StRTpcElectronics : public StTpcElectronicsI {

private:
    St_tpcElectronics* mElec;

public:
    StRTpcElectronics(St_tpcElectronics* In=0){AddData(In);}
    ~StRTpcElectronics(){}
    void AddData(St_tpcElectronics* In){
     mElec = In;
    }
 
    int    numberOfTimeBins()                   const;
    double nominalGain()                        const;
    double samplingFrequency()                  const;
    double tZero()                              const;
    double adcCharge()                          const;
    double adcConversion()                      const;
    double averagePedestal()                    const;
    double shapingTime()                        const;
    double tau()                                const;

ClassDef(StRTpcElectronics,0)

};


#endif















