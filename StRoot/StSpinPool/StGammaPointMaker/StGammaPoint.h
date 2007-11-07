/*
 *  StGammaPoint.h
 *  EMCGammaCalib
 *
 *  Created by Alan Hoffman on 10/19/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef StGammaPoint_HH
#define StGammaPoint_HH

#include <iostream>
#include <vector>
#include "StGammaMaker/StGammaCandidate.h"
#include "StGammaMaker/StGammaEvent.h"
#include "StGammaMaker/StGammaStrip.h"
#include "StGammaStripCluster.h"
#include "TVector3.h"
using namespace std;

class StGammaPoint 
{
	
private:
	float mEta;
	float mPhi;
	float mTheta;
	float mEtaClusterEnergy;
	float mPhiClusterEnergy;
	float mEnergy;
	float mWidthEta;
	float mWidthPhi;
	TVector3 mPosition;
	
public:
		
	StGammaPoint(char* name);
	StGammaPoint();
	~StGammaPoint() {}
	
	//Getters
	float GetEta() {return mEta;}
	float GetPhi() {return mPhi;}
	float GetEtaClusterEnergy() {return mEtaClusterEnergy;}
	float GetPhiClusterEnergy() {return mPhiClusterEnergy;}
	float GetEnergy() {return mEnergy;}
	float GetWidthEta() {return mWidthEta;}
	float GetWidthPhi() {return mWidthPhi;}
	float GetTheta() {return mTheta;}
	float GetAsym() const;
	TVector3 GetPosition() {return mPosition;}
	
	//Setters
	void SetEta(float eta) {mEta = eta;}
	void SetPhi(float phi) {mPhi = phi;}
	void SetTheta(float theta) {mTheta = theta;}
	void SetEtaClusterEnergy(float ECenergy) {mEtaClusterEnergy = ECenergy;}
	void SetPhiClusterEnergy(float PCenergy) {mPhiClusterEnergy = PCenergy;}
	void SetWidthEta(float widtheta) {mWidthEta = widtheta;}
	void SetWidthPhi(float widthphi) {mWidthPhi = widthphi;}
	void SetEnergy(float energy) {mEnergy = energy;}
	void SetPosition(TVector3 position) {mPosition = position;}
	
	//others
	void Reset();
	
};

#endif
