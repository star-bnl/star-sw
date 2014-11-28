/**********************************************************************
 *
 * $Id: StTpcDbElectronics.hh,v 1.2 2000/02/10 01:21:46 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez & Brian Lasiuk Sept 13, 1999
 *
 **********************************************************************
 *
 * Description: Class interface for Electronics parameters taken from
 *              STAR TPC DB
 *
 **********************************************************************
 *
 * $Log: StTpcDbElectronics.hh,v $
 * Revision 1.2  2000/02/10 01:21:46  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.1  1999/10/11 23:55:09  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 *
 **********************************************************************/
#ifndef ST_TPC_DB_ELECTRONICS_HH
#define ST_TPC_DB_ELECTRONICS_HH



#include "StTpcElectronics.hh"
class StTpcDb;
class StTpcDbElectronics : public StTpcElectronics {
public:
    ~StTpcDbElectronics() {/* nopt */}

    static StTpcElectronics* instance();
    static StTpcElectronics* instance(StTpcDb*);

        // Analog Electronics
    double    nominalGain()                    const;
    double    channelGain(int,int,int)         const;
    double    channelGain(StTpcPadCoordinate&) const;
    double    samplingFrequency()              const;
    double    tZero()                          const;
    double    tZero(int,int,int)               const;
    double    tZero(StTpcPadCoordinate&)       const;
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
    StTpcDbElectronics();
    StTpcDbElectronics(StTpcDb*);
    
private:
    static StTpcElectronics* mInstance;
    StTpcDb* gTpcDbPtr;

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

inline double StTpcDbElectronics::nominalGain() const {return mNominalGain;}
inline double StTpcDbElectronics::samplingFrequency() const {return mSamplingFrequency;}
inline double StTpcDbElectronics::shapingTime() const {return mShapingTime;}
inline double StTpcDbElectronics::tZero() const {return mTZero;}
inline double StTpcDbElectronics::tau() const {return mTau;}
inline double StTpcDbElectronics::adcConversion() const {return mAdcConversion;}
inline double StTpcDbElectronics::adcConversionCharge() const {return mAdcConversionCharge;}
inline int StTpcDbElectronics::numberOfTimeBins() const {return mNumberOfTimeBins;}
inline int StTpcDbElectronics::averagePedestal() const {return mAveragePedestal;}
#endif
