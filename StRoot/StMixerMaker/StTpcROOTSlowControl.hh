/***************************************************************************
 *
 * $Id: StTpcROOTSlowControl.hh,v 1.1 2000/02/16 21:02:11 pfachini Exp $
 *
 * Author: brian 
 ***************************************************************************
 *
 * Description:   Class implemented as Singleton
 *
 ***************************************************************************
 *
 * $Log: StTpcROOTSlowControl.hh,v $
 * Revision 1.1  2000/02/16 21:02:11  pfachini
 * First version StMixer
 *
 * Revision 1.2  1999/04/07 00:45:28  lasiuk
 * addition of gas gain
 *
 * Revision 1.1  1999/03/23 03:38:49  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifdef __ROOT__
#ifndef ST_TPC_ROOT_SLOW_CONTROL_HH
#define ST_TPC_ROOT_SLOW_CONTROL_HH

#include <iostream.h>

#include "StTrsMaker/slowcontrolDataSet.h"

#include "SystemOfUnits.h"
#include "StTrsMaker/include/StTpcSlowControl.hh"

class StTpcROOTSlowControl : public StTpcSlowControl {
public:
    ~StTpcROOTSlowControl();
    
    static StTpcSlowControl* instance();
    static StTpcSlowControl* instance(slowcontrolDataSet*);
    
    double driftVelocity()                const;

    // voltage
    double driftVoltage()                 const;
	
    double innerSectorAnodeVoltage()      const;
    double innerSectorGatingGridVoltage() const;
    double outerSectorAnodeVoltage()      const;
    double outerSectorGatingGridVoltage() const;

    // environment
    double hallTemperature()              const;
    double hallPressure()                 const;

    // Gas Gain
    double innerSectorGasGain()           const;
    double innerSectorGasGainVzero()      const;
    double innerSectorGasGainb()          const;
    
    double outerSectorGasGain()           const;
    double outerSectorGasGainVzero()      const;
    double outerSectorGasGainb()          const;

    // Diagnostic
    void print(ostream& os = cout)        const;
    
private:
    StTpcROOTSlowControl();
    StTpcROOTSlowControl(slowcontrolDataSet*);
    
private:
    static StTpcSlowControl* mInstance;

    double mDriftVelocity;

    // Environment
    double mHallTemperature;
    double mHallPressure;
    
    // Voltages
    double mDriftVoltage;
    
    double mISAnodeVoltage;
    double mISGatingGridVoltage;
    double mOSAnodeVoltage;
    double mOSGatingGridVoltage;

    // Gas Gain
    double mISGasGain;
    double mISGasGainVzero;
    double mISGasGainb;

    double mOSGasGain;
    double mOSGasGainVzero;
    double mOSGasGainb;
};


inline double StTpcROOTSlowControl::driftVelocity() const {return mDriftVelocity;}
inline double StTpcROOTSlowControl::driftVoltage() const {return mDriftVoltage;}
inline double StTpcROOTSlowControl::innerSectorAnodeVoltage() const {return mISAnodeVoltage;}
inline double StTpcROOTSlowControl::innerSectorGatingGridVoltage() const {return mISGatingGridVoltage;}
inline double StTpcROOTSlowControl::outerSectorAnodeVoltage() const {return mOSAnodeVoltage;}
inline double StTpcROOTSlowControl::outerSectorGatingGridVoltage() const {return mOSGatingGridVoltage;}
inline double StTpcROOTSlowControl::hallTemperature() const {return mHallTemperature;}
inline double StTpcROOTSlowControl::hallPressure() const {return mHallPressure;}
inline double StTpcROOTSlowControl::innerSectorGasGain() const {return mISGasGain;}
inline double StTpcROOTSlowControl::innerSectorGasGainVzero() const {return mISGasGainVzero;}
inline double StTpcROOTSlowControl::innerSectorGasGainb() const {return mISGasGainb;}

inline double StTpcROOTSlowControl::outerSectorGasGain() const {return mOSGasGain;}
inline double StTpcROOTSlowControl::outerSectorGasGainVzero() const {return mOSGasGainVzero;}
inline double StTpcROOTSlowControl::outerSectorGasGainb() const {return mOSGasGainb;}
#endif
#endif
