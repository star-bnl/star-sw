/***************************************************************************
 *
 * $Id: StTpcSimpleSlowControl.hh,v 1.2 1999/04/07 00:45:27 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:   Class implemented as Singleton
 *
 ***************************************************************************
 *
 * $Log: StTpcSimpleSlowControl.hh,v $
 * Revision 1.2  1999/04/07 00:45:27  lasiuk
 * addition of gas gain
 *
 * Revision 1.1  1998/11/10 17:12:08  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/05 18:19:33  lasiuk
 * drift voltage
 *
 * Revision 1.2  1998/05/20 19:00:19  ullrich
 * Renamed getInstance() to instance().
 *
 * Revision 1.1  1998/05/20 14:58:26  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TPC_SIMPLE_SLOW_CONTROL_HH
#define ST_TPC_SIMPLE_SLOW_CONTROL_HH

#include <iostream.h>
#include "SystemOfUnits.h"
#include "StTpcSlowControl.hh"

class StTpcSimpleSlowControl : public StTpcSlowControl {
public:
    ~StTpcSimpleSlowControl();
    
    static StTpcSlowControl* instance();
    static StTpcSlowControl* instance(const char* file);
    
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
    StTpcSimpleSlowControl();
    StTpcSimpleSlowControl(const char*);
    
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


inline double StTpcSimpleSlowControl::driftVelocity() const {return mDriftVelocity;}
inline double StTpcSimpleSlowControl::driftVoltage() const {return mDriftVoltage;}
inline double StTpcSimpleSlowControl::innerSectorAnodeVoltage() const {return mISAnodeVoltage;}
inline double StTpcSimpleSlowControl::innerSectorGatingGridVoltage() const {return mISGatingGridVoltage;}
inline double StTpcSimpleSlowControl::outerSectorAnodeVoltage() const {return mOSAnodeVoltage;}
inline double StTpcSimpleSlowControl::outerSectorGatingGridVoltage() const {return mOSGatingGridVoltage;}
inline double StTpcSimpleSlowControl::hallTemperature() const {return mHallTemperature;}
inline double StTpcSimpleSlowControl::hallPressure() const {return mHallPressure;}
inline double StTpcSimpleSlowControl::innerSectorGasGain() const {return mISGasGain;}
inline double StTpcSimpleSlowControl::innerSectorGasGainVzero() const {return mISGasGainVzero;}
inline double StTpcSimpleSlowControl::innerSectorGasGainb() const {return mISGasGainb;}

inline double StTpcSimpleSlowControl::outerSectorGasGain() const {return mOSGasGain;}
inline double StTpcSimpleSlowControl::outerSectorGasGainVzero() const {return mOSGasGainVzero;}
inline double StTpcSimpleSlowControl::outerSectorGasGainb() const {return mOSGasGainb;}
#endif
