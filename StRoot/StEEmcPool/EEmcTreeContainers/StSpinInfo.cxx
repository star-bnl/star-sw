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
   mBunchCrossing7bit = -1;
   mBunchCrossing48bit = -1;
   mBunchCrossingStar = -1;
   mPolarizationType = StSpinInfo_t::UNDEFINED_POLARIZATION;
   mDsmVertex = 0;
};

ClassImp( StSpinInfo_t );

/*
 * $Id: StSpinInfo.cxx,v 1.2 2013/02/21 19:14:47 sgliske Exp $
 * $Log: StSpinInfo.cxx,v $
 * Revision 1.2  2013/02/21 19:14:47  sgliske
 * changed initialization values for bXing to -1 instead of 0, since 0 is a valid value
 *
 * Revision 1.1  2012/11/26 19:03:06  sgliske
 * moved from offline/users/sgliske/StRoot/StSpinPool/StSpinInfo to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
