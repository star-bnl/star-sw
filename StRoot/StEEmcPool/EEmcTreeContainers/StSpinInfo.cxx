/*!
 *
 * \class StSpinInfo_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Storage class of the basic spin information needed for
 * spin-dependent analyses.
 *
 */

#include "StSpinInfo.h"

/// Construtor---set everything to zero
StSpinInfo_t::StSpinInfo_t(){
   clear();
};

void StSpinInfo_t::clear(){
   // reset to nominal values
   mValidDB = 0;
   mSpin4 = 0;
   mBunchCrossing7bit = 0;
   mBunchCrossing48bit = 0;
   mBunchCrossingStar = 0;
   mPolarizationType = StSpinInfo_t::UNDEFINED_POLARIZATION;
   mDsmVertex = 0;
};

ClassImp( StSpinInfo_t );

/*
 * $Id: StSpinInfo.cxx,v 1.1 2012/11/26 19:03:06 sgliske Exp $
 * $Log: StSpinInfo.cxx,v $
 * Revision 1.1  2012/11/26 19:03:06  sgliske
 * moved from offline/users/sgliske/StRoot/StSpinPool/StSpinInfo to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
