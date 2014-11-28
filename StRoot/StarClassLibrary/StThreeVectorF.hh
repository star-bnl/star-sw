/***************************************************************************
 *
 * $Id: StThreeVectorF.hh,v 1.14 2005/07/06 18:49:57 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:  
 *
 * Remarks:   This is a 'handmade' specialisation of StThreeVector<T>
 *            for T=float. This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StThreeVectorF.hh,v $
 * Revision 1.14  2005/07/06 18:49:57  fisyak
 * Replace StHelixD, StLorentzVectorD,StLorentzVectorF,StMatrixD,StMatrixF,StPhysicalHelixD,StThreeVectorD,StThreeVectorF by templated version
 *
 ****************************************************************************/
#ifndef ST_THREE_VECTOR_F_HH
#define ST_THREE_VECTOR_F_HH
#include "StThreeVector.hh"
typedef  StThreeVector<float> StThreeVectorF;
#endif
