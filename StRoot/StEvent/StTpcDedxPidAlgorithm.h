/***************************************************************************
 *
 * $Id: StTpcDedxPidAlgorithm.h,v 2.6 2000/03/02 12:43:51 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPidAlgorithm.h,v $
 * Revision 2.6  2000/03/02 12:43:51  ullrich
 * Method can be passed as argument to constructor. Default
 * method is truncated mean.
 *
 * Revision 2.5  1999/12/21 15:09:14  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.4  1999/12/02 16:35:40  ullrich
 * Added method to return the stored dE/dx traits
 *
 * Revision 2.3  1999/11/29 15:33:34  ullrich
 * Changed Macro for SUN CC
 *
 * Revision 2.2  1999/10/28 22:27:04  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:55  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcDedxPidAlgorithm_hh
#define StTpcDedxPidAlgorithm_hh
#include <vector>
#include "StFunctional.h"
#include "StEnumerations.h"
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

class StDedxPidTraits;

class StTpcDedxPidAlgorithm : public StPidAlgorithm {
public:
    StTpcDedxPidAlgorithm(StDedxMethod = kTruncatedMeanId);
    
    StParticleDefinition*  operator() (const StTrack&, const StSPtrVecTrackPidTraits&);

    const  StDedxPidTraits* traits() const;
    double numberOfSigma(const StParticleDefinition*) const;
    double meanPidFunction(const StParticleDefinition*) const;
    double sigmaPidFunction(const StParticleDefinition*) const;

private:
    const StDedxPidTraits*        mTraits;       //!
    const StTrack*                mTrack;        //!
    StDedxMethod                  mDedxMethod;   //!
#if defined(ST_NO_TEMPLATE_DEF_ARGS)
    vector<StParticleDefinition*,
           allocator<StParticleDefinition*> > mParticles; //!
#else
    vector<StParticleDefinition*> mParticles;    //!
#endif
};
#endif


