/**********************************************************************
 * $Id: StRichNoiseSimulator.h,v 1.3 2000/02/08 16:28:32 lasiuk Exp $
 *
 * Description:
 *   StRichNoiseSimulator is the function object containing
 *   the algorithm that add noise to a certain signal.
 * 
 *   StRichNoiseSimulator is used like a normal function,
 *   i.e. StRichNoiseSimulator();  
 * 
 *   StRichNoiseSimulator generates an electric noise
 *   depending on an experimental factor from a 
 *   database.
 *
 **********************************************************************
 * $Log: StRichNoiseSimulator.h,v $
 * Revision 1.3  2000/02/08 16:28:32  lasiuk
 * change to class.  Use dbs and random number generators
 * from data members
 *
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:03  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 8/24/1999 created the class, Alexandre Nevski.
 *     - 8/24/1999 initial implementation, C & A.
 *
 **********************************************************************/
#ifndef ST_RICH_NOISE_SIMULATOR_H
#define ST_RICH_NOISE_SIMULATOR_H

#include <functional>
#ifndef  ST_NO_NAMESPACES
using std::unary_function;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRichRrsMacros.h"
#include "StRichPhysicsDb.h"
#include "StRichOtherAlgorithms.h"

class StRichNoiseSimulator : public unary_function<double,double> {
public:
    StRichNoiseSimulator();
    ~StRichNoiseSimulator();

    //StRichNoiseSimulator(const StRichNoiseSimulator&) {/* use default */}
    //StRichNoiseSimulator& operator=(const StRichNoiseSimulator&) {/* use default */}

    double operator()(void) const;
private:
    StRichPhysicsDb*  mPhysicsDb;
    Randoms           mRandom;

    double            mElectricNoise;
};

#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif
