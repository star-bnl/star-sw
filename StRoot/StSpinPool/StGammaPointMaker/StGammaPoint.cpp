/*
 *  StGammaPoint.cpp
 *  EMCGammaCalib
 *
 *  Created by Alan Hoffman on 10/19/07.
 *
 */

#include "StGammaPoint.h"

//------------------------------------------------------
StGammaPoint::StGammaPoint(char* name)
{
	
	mEta = 0.;
	mPhi = 0.;
	mTheta = 0.;
	mEtaClusterEnergy = 0.;
	mPhiClusterEnergy = 0.;
	mEnergy = 0.;
	mWidthEta = 0.;
	mWidthPhi = 0.;
}

//------------------------------------------------------
StGammaPoint::StGammaPoint()
{
	
	mEta = 0.;
	mPhi = 0.;
	mTheta = 0;
	mEtaClusterEnergy = 0.;
	mPhiClusterEnergy = 0.;
	mEnergy = 0.;
	mWidthEta = 0.;
	mWidthPhi = 0.;
}

//------------------------------------------------------
float StGammaPoint::GetAsym() const {
	float asym = TMath::Abs((mEtaClusterEnergy - mPhiClusterEnergy))/(mEtaClusterEnergy + mPhiClusterEnergy);
	return asym;
}

//------------------------------------------------------
void StGammaPoint::Reset() {
	mEta = 0.;
	mPhi = 0.;
	mEtaClusterEnergy = 0.;
	mPhiClusterEnergy = 0.;
	mEnergy = 0.;
	mWidthEta = 0.;
	mWidthPhi = 0.;
}

