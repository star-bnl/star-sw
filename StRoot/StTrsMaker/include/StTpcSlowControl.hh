/***************************************************************************
 *
 * $Id: StTpcSlowControl.hh,v 1.4 2008/06/20 15:01:02 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTpcSlowControl.hh,v $
 * Revision 1.4  2008/06/20 15:01:02  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.3  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  1999/04/07 00:45:27  lasiuk
 * addition of gas gain
 *
 * Revision 1.1  1998/11/10 17:12:08  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.4  1998/11/05 18:19:31  lasiuk
 * drift voltage
 *
 * Revision 1.3  1998/11/04 21:24:56  lasiuk
 * add voltages/ add print/ incorporate units
 *
 * Revision 1.2  1998/05/25 17:03:03  lasiuk
 * remove abstract destructor
 *
 * Revision 1.1  1998/05/20 14:58:26  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TPC_SLOW_CONTROL_HH
#define ST_TPC_SLOW_CONTROL_HH
#include <Stiostream.h>

class StTpcSlowControl {
public:
    virtual ~StTpcSlowControl() {/* nopt */}

    virtual double driftVelocity(int sector)      const = 0;

    // Environment
    virtual double hallTemperature()              const = 0;
    virtual double hallPressure()                 const = 0;
    
    // Voltages
    virtual double driftVoltage()                 const = 0;
    
    virtual double innerSectorAnodeVoltage()      const = 0;
    virtual double innerSectorGatingGridVoltage() const = 0;
    virtual double outerSectorAnodeVoltage()      const = 0;
    virtual double outerSectorGatingGridVoltage() const = 0;

    // Gas Gains
    virtual double innerSectorGasGain()           const = 0;
    virtual double innerSectorGasGainVzero()      const = 0;
    virtual double innerSectorGasGainb()          const = 0;
    
    virtual double outerSectorGasGain()           const = 0;
    virtual double outerSectorGasGainVzero()      const = 0;
    virtual double outerSectorGasGainb()          const = 0;
    
    // Diagnostic
    virtual void print(ostream& os = cout)        const = 0;
};

#endif
