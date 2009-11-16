/***************************************************************************
 *
 * $Id: StRTpcHitErrors.cxx,v 1.2 2007/08/04 00:38:04 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Hit Errors interface  
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#include "StRTpcHitErrors.h"

ClassImp(StRTpcHitErrors)


float  StRTpcHitErrors::Sig2IntrinsicOuterX() const { return (*mHitErrors)[0].sig2_intrinsic_outer_x;}
float  StRTpcHitErrors::Sig2DriftOuterX() const { return (*mHitErrors)[0].sig2_drift_outer_x;}
float  StRTpcHitErrors::Sig2TanOuterX() const { return (*mHitErrors)[0].sig2_tan_outer_x;}

float  StRTpcHitErrors::Sig2IntrinsicOuterZ() const { return (*mHitErrors)[0].sig2_intrinsic_outer_z;}
float  StRTpcHitErrors::Sig2DriftOuterZ() const { return (*mHitErrors)[0].sig2_drift_outer_z;}
float  StRTpcHitErrors::Sig2TanOuterZ() const { return (*mHitErrors)[0].sig2_tan_outer_z;}


float  StRTpcHitErrors::Sig2IntrinsicInnerX() const { return (*mHitErrors)[0].sig2_intrinsic_inner_x;}
float  StRTpcHitErrors::Sig2DriftInnerX() const { return (*mHitErrors)[0].sig2_drift_inner_x;}
float  StRTpcHitErrors::Sig2TanInnerX() const { return (*mHitErrors)[0].sig2_tan_inner_x;}

float  StRTpcHitErrors::Sig2IntrinsicInnerZ() const { return (*mHitErrors)[0].sig2_intrinsic_inner_z;}
float  StRTpcHitErrors::Sig2DriftInnerZ() const { return (*mHitErrors)[0].sig2_drift_inner_z;}
float  StRTpcHitErrors::Sig2TanInnerZ() const { return (*mHitErrors)[0].sig2_tan_inner_z;}








