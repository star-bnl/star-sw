/*******************************************************************
 * $Id: StRichGasGain.h,v 1.3 2000/02/08 16:24:10 lasiuk Exp $
 *
 * Description:
 *   StRichGasGain is a function object containing the charge
 *   amplification algotithm. 
 *   
 *   StRichGasGain is used like a normal function,
 *   i.e. StRichGasGain( GHit,wire position );  
 * 
 *   The algorithm used is the following:
 *    StRichGasGain computes an amplification factor of an
 *    avalanche of electrons on a wire under the effect
 *    of an electrostatic field. This factor depends
 *    on a parameter, which describes the number
 *    of generations in an avalanche (cf. "Particle
 *    Detection with Drift Chambers" by W.Blum, L.Rolandi),
 *    as well as on the characteristics of the gas in the 
 *    chamber.
 *    Each avalanche may also produce feedback photons,
 *    which frequency depends on the gas' characteristics. 
 *    The three other parameters are the efficiency with
 *    which feedback photons are created in an avalanche
 *    and the efficiency with which they kick out an electron
 *    from CsI. Finally, geomtrical considerations have to be
 *    taken in account. 
 *
 *******************************************************************
 * $Log: StRichGasGain.h,v $
 * Revision 1.3  2000/02/08 16:24:10  lasiuk
 * use of dbs
 *
 * Revision 1.3  2000/02/08 16:24:10  lasiuk
 * use of dbs
 *
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:01  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 7/21/1999 created the class, Alexandre Nevski.
 *     - 8/17/1999 added RnPolia,     Alexandre Nevski.
 *     - 8/25/1999 added feedbackPhoton Caroline Peter.
 *
 *******************************************************************/
#ifndef ST_RICH_GAS_GAIN_H
#define ST_RICH_GAS_GAIN_H

#include <functional>
#ifndef ST_NO_NAMESPACES
using std::binary_function;
#endif


#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRichRrsMacros.h"
#include "StRichGHit.h"
#include "StRichGeometryDb.h"
#include "StRichPhysicsDb.h"
#include "StRichOtherAlgorithms.h"

class StRichGasGain : public binary_function<StRichGHit,double,double> {
public:
    StRichGasGain();
    ~StRichGasGain();
    
    //StRichGasGain(const StRichGasGain&) { /* use default */ }
    //StRichGasGain& operator=(const StRichGasGain&) {/* use default */}

    double operator()(StRichGHit&, double );
    
private:	
    void feedbackPhoton( const StRichGHit&, double ) const;

private:
    StRichGeometryDb* mGeometryDb;
    StRichPhysicsDb*  mPhysicsDb;
    Randoms           mRandom;

    double            mAnodePadPlaneSeparation;
    double            mPhotonFeedback;
    double            mPhotoConversion;
};

#ifndef ST_NO_NAMESPACES
//}
#endif
    
#endif // ST_RICH_GAS_GAIN
