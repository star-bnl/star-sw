/***************************************************************************
 *
 * $Id: StTpcDbSlowControl.hh,v 1.7 2009/11/03 14:34:19 fisyak Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez & Brian Lasiuk Sept 13, 1999
 ***************************************************************************
 *
 * Description:   Class to hold Slow Control parameters for TRS taken
 *                from the STAR TPC DB. Class implemented as Singleton
 *
 ***************************************************************************
 *
 * $Log: StTpcDbSlowControl.hh,v $
 * Revision 1.7  2009/11/03 14:34:19  fisyak
 * Remove default in zFromTB
 *
 * Revision 1.6  2008/06/20 15:00:59  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.5  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2000/05/18 17:48:47  long
 * re_pointing drift velocity to calibrated drift velocity
 *
 * Revision 1.3  2000/02/10 01:21:46  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.2  2000/01/10 23:11:29  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.1  1999/10/11 23:55:09  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 *
 **************************************************************************/
#ifndef ST_TPC_DB_SLOW_CONTROL_HH
#define ST_TPC_DB_SLOW_CONTROL_HH

#include <Stiostream.h>


#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StTpcSlowControl.hh"
#include "StTpcDb/StTpcDb.h"

class StTpcDbSlowControl : public StTpcSlowControl {
public:
    ~StTpcDbSlowControl();
    
    static StTpcSlowControl* instance();
    static StTpcSlowControl* instance(StTpcDb*);
    
    double driftVelocity(int sector)      const;

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
    StTpcDbSlowControl();
    StTpcDbSlowControl(StTpcDb*);
    
private:
    static StTpcSlowControl* mInstance;
    StTpcDb* gTpcDbPtr;

//     double mDriftVelocity;

//     // Environment
//     double mHallTemperature;
//     double mHallPressure;
    
//     // Voltages
//     double mDriftVoltage;
    
//     double mISAnodeVoltage;
//     double mISGatingGridVoltage;
//     double mOSAnodeVoltage;
//     double mOSGatingGridVoltage;

//     // Gas Gain
//     double mISGasGain;
//     double mISGasGainVzero;
//     double mISGasGainb;

//     double mOSGasGain;
//     double mOSGasGainVzero;
//     double mOSGasGainb;
};

inline double StTpcDbSlowControl::driftVelocity(int sector) const {return gTpcDbPtr->DriftVelocity(sector)*(centimeter/(second));}
    // Voltages
inline double StTpcDbSlowControl::driftVoltage() const{return gTpcDbPtr->SlowControlSim()->driftVoltage()*volt;}
inline double StTpcDbSlowControl::innerSectorAnodeVoltage() const{return gTpcDbPtr->SlowControlSim()->innerSectorAnodeVoltage()*volt;}
inline double StTpcDbSlowControl::innerSectorGatingGridVoltage() const{return gTpcDbPtr->SlowControlSim()->innerSectorGatingGridV()*volt;}
inline double StTpcDbSlowControl::outerSectorAnodeVoltage() const{return gTpcDbPtr->SlowControlSim()->outerSectorAnodeVoltage()*volt;}
inline double StTpcDbSlowControl::outerSectorGatingGridVoltage() const{return gTpcDbPtr->SlowControlSim()->outerSectorGatingGridV()*volt;}
    // Environment
inline double StTpcDbSlowControl::hallTemperature() const{return gTpcDbPtr->SlowControlSim()->hallTemperature();}
inline double StTpcDbSlowControl::hallPressure() const{return gTpcDbPtr->SlowControlSim()->hallPressure()*atmosphere;}
    // Gas Gain
inline double StTpcDbSlowControl::innerSectorGasGain() const{return gTpcDbPtr->SlowControlSim()->innerSectorGasGain();}
inline double StTpcDbSlowControl::innerSectorGasGainVzero() const{return gTpcDbPtr->SlowControlSim()->innerSectorGasGainVzero()*volt;}
inline double StTpcDbSlowControl::innerSectorGasGainb() const{return gTpcDbPtr->SlowControlSim()->outerSectorGasGainb()/volt;}

inline double StTpcDbSlowControl::outerSectorGasGain() const{return gTpcDbPtr->SlowControlSim()->outerSectorGasGain();}
inline double StTpcDbSlowControl::outerSectorGasGainVzero() const{return gTpcDbPtr->SlowControlSim()->outerSectorGasGainVzero()*volt;}
inline double StTpcDbSlowControl::outerSectorGasGainb() const{return gTpcDbPtr->SlowControlSim()->outerSectorGasGainb()/volt;}

// inline double StTpcDbSlowControl::driftVelocity() const {return mDriftVelocity;}
//     // Voltages
// inline double StTpcDbSlowControl::driftVoltage() const {return mDriftVoltage;}
// inline double StTpcDbSlowControl::innerSectorAnodeVoltage() const {return mISAnodeVoltage;}
// inline double StTpcDbSlowControl::innerSectorGatingGridVoltage() const {return mISGatingGridVoltage;}
// inline double StTpcDbSlowControl::outerSectorAnodeVoltage() const {return mOSAnodeVoltage;}
// inline double StTpcDbSlowControl::outerSectorGatingGridVoltage() const {return mOSGatingGridVoltage;}
//     // Environment
// inline double StTpcDbSlowControl::hallTemperature() const {return mHallTemperature;}
// inline double StTpcDbSlowControl::hallPressure() const {return mHallPressure;}
//     // Gas Gain
// inline double StTpcDbSlowControl::innerSectorGasGain() const {return mISGasGain;}
// inline double StTpcDbSlowControl::innerSectorGasGainVzero() const {return mISGasGainVzero;}
// inline double StTpcDbSlowControl::innerSectorGasGainb() const {return mISGasGainb;}

// inline double StTpcDbSlowControl::outerSectorGasGain() const {return mOSGasGain;}
// inline double StTpcDbSlowControl::outerSectorGasGainVzero() const {return mOSGasGainVzero;}
// inline double StTpcDbSlowControl::outerSectorGasGainb() const {return mOSGasGainb;}
#endif
