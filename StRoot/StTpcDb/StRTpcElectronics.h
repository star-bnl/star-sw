#ifndef __STRTPCELECTRONICS__
#define __STRTPCELECTRONICS__
#include "StTpcElectronicsI.h"
#include "StDbLib/Geometry/tpcElectronics.h"

class StRTpcElectronics : public StTpcElectronicsI {

private:
    tpcElectronics* mElec;

public:
    StRTpcElectronics(){}
    ~StRTpcElectronics(){}
    void AddData(tpcElectronics* In){
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















