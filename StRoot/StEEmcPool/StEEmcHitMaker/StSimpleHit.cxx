/*!
 *
 * \class StSimpleHit_t
 * \author Stephen Gliske <sgliske@anl.gov>
 * $Id: 
 * $Log:
 *
 * Root stuff for the class.  See the header file.
 */

#include "StSimpleHit.h"

std::ostream &operator<<( std::ostream &out, const StSimpleHit_t &hit ){
   out << "hit " << hit.mID << " at " << hit.mX << ' ' << hit.mY << ' ' << hit.mZ << ", E = " << hit.mE;
   return out;
};

ClassImp(StSimpleHit_t);

/*
 * $Id: StSimpleHit.cxx,v 1.1 2012/11/26 19:05:56 sgliske Exp $ 
 * $Log: StSimpleHit.cxx,v $
 * Revision 1.1  2012/11/26 19:05:56  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
