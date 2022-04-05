/***************************************************************************
 *
 * $Id: StLorentzVectorF.hh,v 1.6 2005/07/06 18:49:56 fisyak Exp $
 * $Log: StLorentzVectorF.hh,v $
 * Revision 1.6  2005/07/06 18:49:56  fisyak
 * Replace StHelixD, StLorentzVectorD,StLorentzVectorF,StMatrixD,StMatrixF,StPhysicalHelixD,StThreeVectorD,StThreeVectorF by templated version
 *

****************************************************************************/
#ifndef ST_LORENTZ_VECTOR_F_HH
#define ST_LORENTZ_VECTOR_F_HH
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVector.hh"
typedef StLorentzVector<float> StLorentzVectorF;
#endif
