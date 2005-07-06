/***************************************************************************
 *
 * $Id: StMatrixD.hh,v 1.9 2005/07/06 18:49:56 fisyak Exp $
 * $Log: StMatrixD.hh,v $
 * Revision 1.9  2005/07/06 18:49:56  fisyak
 * Replace StHelixD, StLorentzVectorD,StLorentzVectorF,StMatrixD,StMatrixF,StPhysicalHelixD,StThreeVectorD,StThreeVectorF by templated version
 *

****************************************************************************/
#ifndef ST_MATRIX_D_HH
#define ST_MATRIX_D_HH
#include "Stiostream.h"
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"
#include "StMatrix.hh"
typedef  StMatrix<double> StMatrixD;
#endif
