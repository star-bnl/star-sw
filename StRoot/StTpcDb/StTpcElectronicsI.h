#ifndef __STTPCELECTRONICSI__
#define __STTPCELECTRONICSI__
#include <TObject.h>

class StTpcElectronicsI : public TObject {

  //Abstract base class defining accessors
public:

    virtual int    numberOfTimeBins()                   const = 0;
    virtual double nominalGain()                        const = 0;
    virtual double samplingFrequency()                  const = 0;
    virtual double tZero()                              const = 0;
    virtual double adcCharge()                          const = 0;
    virtual double adcConversion()                      const = 0;
    virtual double averagePedestal()                    const = 0;
    virtual double shapingTime()                        const = 0;
    virtual double tau()                                const = 0;

ClassDef(StTpcElectronicsI,0)

};
#endif















