/***************************************************************************
 *
 * $Id: StLorentzVectorD.hh,v 1.6 2005/07/06 18:49:56 fisyak Exp $
 * $Log: StLorentzVectorD.hh,v $
 * Revision 1.6  2005/07/06 18:49:56  fisyak
 * Replace StHelixD, StLorentzVectorD,StLorentzVectorF,StMatrixD,StMatrixF,StPhysicalHelixD,StThreeVectorD,StThreeVectorF by templated version
 *

****************************************************************************/
#ifndef ST_LORENTZ_VECTOR_D_HH
#define ST_LORENTZ_VECTOR_D_HH
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVector.hh"
typedef StLorentzVector<double> StLorentzVectorD;
#endif
