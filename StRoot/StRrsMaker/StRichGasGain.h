/*******************************************************************
 * $Id: StRichGasGain.h,v 1.4 2000/03/17 14:54:36 lasiuk Exp $
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
 * Revision 1.4  2000/03/17 14:54:36  lasiuk
 * Large scale revisions after ROOT dependent memory leak
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
 *******************************************************************/
#ifndef ST_RICH_GAS_GAIN_H
#define ST_RICH_GAS_GAIN_H

#include <list>
#ifndef ST_NO_NAMESPACES
using std::list;
#endif

#include "StRichRrsMacros.h"
#include "StRichGHit.h"
#include "StRichMiniHit.h"
#include "StRichOtherAlgorithms.h"

class StRichGasGain {
public:
    StRichGasGain();
    ~StRichGasGain();
    
    //StRichGasGain(const StRichGasGain&) { /* use default */ }
    //StRichGasGain& operator=(const StRichGasGain&) {/* use default */}

    double avalanche(StRichMiniHit*, double wirePos, list<StRichMiniHit*>&);

private:	
    void feedbackPhoton(StRichMiniHit* hit, double q, list<StRichMiniHit*>&);

private:
    Randoms           mRandom;

    double            mAnodePadPlaneSeparation;
    double            mPhotonFeedback;
    double            mPhotoConversion;
    double            mGasGainAmplification;
    double            mPolia;
};

#endif // ST_RICH_GAS_GAIN
