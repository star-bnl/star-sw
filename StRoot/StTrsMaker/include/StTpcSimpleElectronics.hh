/**********************************************************************
 *
 * $Id: StTpcSimpleElectronics.hh,v 1.4 1999/02/28 20:17:53 lasiuk Exp $
 *
 * Author: brian Nov 3, 1998
 *
 **********************************************************************
 *
 * Description:  Abstract Class interface for Electronics parameters
 *
 **********************************************************************
 *
 * $Log: StTpcSimpleElectronics.hh,v $
 * Revision 1.4  1999/02/28 20:17:53  lasiuk
 * add numberOfTimeBins
 *
 * Revision 1.3  1999/02/24 19:33:17  lasiuk
 * add tzero offset parameter
 *
 * Revision 1.2  1999/01/18 10:21:57  lasiuk
 * add tau
 *
 * Revision 1.1  1998/11/10 17:12:07  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/04 18:52:26  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#ifndef ST_TPC_SIMPLE_ELECTRONICS_HH
#define ST_TPC_SIMPLE_ELECTRONICS_HH

#include "StTpcElectronics.hh"

class StTpcSimpleElectronics : StTpcElectronics {
public:
    ~StTpcSimpleElectronics() {/* nopt */}
    //StTpcSimpleElectronics(const StTpcSimpleElectronics&);
    //StTpcSimpleElectronics& operator=(const StTpcSimpleElectronics&);

    static StTpcElectronics* instance();
    static StTpcElectronics* instance(const char*);

        // Analog Electronics
    double    nominalGain()                    const;
    double    channelGain(int,int,int)         const;
    double    channelGain(StTpcPadCoordinate&) const;
    double    samplingFrequency()              const;
    double    tZero()                          const;
    double    shapingTime()                    const;
    double    tau()                            const;
    
    // Digital Electronics
    double    adcConversion()                  const;
    double    adcConversionCharge()            const;
    int       numberOfTimeBins()               const;
    int       averagePedestal()                const;
    int       pedestal(int,int,int,int)        const;
    int       pedestal(StTpcPadCoordinate&)    const;

    // Diagnostic: print out complete database
    void print(ostream& = cout)                const;
    
private:
    StTpcSimpleElectronics();
    StTpcSimpleElectronics(const char*);
    
private:
    static StTpcElectronics* mInstance;

    double mNominalGain;
    double mSamplingFrequency;
    double mTZero;
    double mShapingTime;
    double mTau;
    
    double mAdcConversion;
    double mAdcConversionCharge;
    int    mNumberOfTimeBins;
    int    mAveragePedestal;
};
inline double StTpcSimpleElectronics::nominalGain() const {return mNominalGain;}
inline double StTpcSimpleElectronics::samplingFrequency() const {return mSamplingFrequency;}
inline double StTpcSimpleElectronics::shapingTime() const {return mShapingTime;}
inline double StTpcSimpleElectronics::tZero() const {return mTZero;}
inline double StTpcSimpleElectronics::tau() const {return mTau;}
inline double StTpcSimpleElectronics::adcConversion() const {return mAdcConversion;}
inline double StTpcSimpleElectronics::adcConversionCharge() const {return mAdcConversionCharge;}
inline int StTpcSimpleElectronics::numberOfTimeBins() const {return mNumberOfTimeBins;}
inline int StTpcSimpleElectronics::averagePedestal() const {return mAveragePedestal;}
#endif
