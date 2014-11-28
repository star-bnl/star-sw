/***************************************************************************
 *
 * $Id: StMatrixF.hh,v 1.10 2005/07/06 18:49:56 fisyak Exp $
 * $Log: StMatrixF.hh,v $
 * Revision 1.10  2005/07/06 18:49:56  fisyak
 * Replace StHelixD, StLorentzVectorD,StLorentzVectorF,StMatrixD,StMatrixF,StPhysicalHelixD,StThreeVectorD,StThreeVectorF by templated version
 *

****************************************************************************/
#ifndef ST_MATRIX_F_HH
#define ST_MATRIX_F_HH
#include "Stiostream.h"
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"
#include "StMatrix.hh"
typedef  StMatrix<float> StMatrixF;
#endif
