/**********************************************************************
 *
 * $Id: StTpcROOTElectronics.hh,v 1.1 2000/02/16 21:02:11 pfachini Exp $
 *
 * Author: brian Mar 22, 1999
 *
 **********************************************************************
 *
 * Description:  Abstract Class interface for Electronics parameters
 *
 **********************************************************************
 *
 * $Log: StTpcROOTElectronics.hh,v $
 * Revision 1.1  2000/02/16 21:02:11  pfachini
 * First version StMixer
 *
 * Revision 1.1  1999/03/23 03:38:48  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#ifdef __ROOT__
#ifndef ST_TPC_ROOT_ELECTRONICS_HH
#define ST_TPC_ROOT_ELECTRONICS_HH

#include "StTrsMaker/electronicsDataSet.h"

#include "StTrsMaker/include/StTpcElectronics.hh"

class StTpcROOTElectronics : StTpcElectronics {
public:
    ~StTpcROOTElectronics() {/* nopt */}
    //StTpcROOTElectronics(const StTpcROOTElectronics&);
    //StTpcROOTElectronics& operator=(const StTpcROOTElectronics&);

    static StTpcElectronics* instance();
    static StTpcElectronics* instance(electronicsDataSet*);

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
    StTpcROOTElectronics();
    StTpcROOTElectronics(electronicsDataSet*);
    
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
inline double StTpcROOTElectronics::nominalGain() const {return mNominalGain;}
inline double StTpcROOTElectronics::samplingFrequency() const {return mSamplingFrequency;}
inline double StTpcROOTElectronics::shapingTime() const {return mShapingTime;}
inline double StTpcROOTElectronics::tZero() const {return mTZero;}
inline double StTpcROOTElectronics::tau() const {return mTau;}
inline double StTpcROOTElectronics::adcConversion() const {return mAdcConversion;}
inline double StTpcROOTElectronics::adcConversionCharge() const {return mAdcConversionCharge;}
inline int StTpcROOTElectronics::numberOfTimeBins() const {return mNumberOfTimeBins;}
inline int StTpcROOTElectronics::averagePedestal() const {return mAveragePedestal;}
#endif
#endif
