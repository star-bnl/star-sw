/*
 *  StGammaStripCluster.cpp
 *
 *  Created by Alan Hoffman on 10/19/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "StGammaStripCluster.h"

//------------------------------------------------------
StGammaStripCluster::StGammaStripCluster(char* name)
{
	
	mPosition = 0.;
	mDetector = 0.;
	mEnergy = 0.;
	mStripcounter = 0;
	debug = 0;
	if (mStrips.size() > 0) mStrips.clear();
}

//------------------------------------------------------
StGammaStripCluster::StGammaStripCluster()
{
	
	mPosition = 0.;
	mEnergy = 0.;
	mStripcounter = 0;
	debug = 0;
	if (mStrips.size() > 0) mStrips.clear();
}


//------------------------------------------------------
void StGammaStripCluster::AddStrip(StGammaStrip* GammaStrip) {
	mStrips.push_back(GammaStrip);
	mEnergy+=GammaStrip->energy;
}

//-------------------------------------------------------
void StGammaStripCluster::ClearStripVector() {
	if (mStrips.size() > 0) {
		mStrips.clear();
	}
}

//--------------------------------------------------------
void StGammaStripCluster::CalcTheta(int det_plane) {
	
	if (mStrips.size() == 0) return;
	
	double positionsum = 0;
	double energysum = 0;
	double temp_theta = 0;
	
	if (det_plane == 0) {
		vector<StGammaStrip*>::iterator centroid_it;
		if (mStrips.size() == 1) {
			centroid_it = mStrips.begin();
			temp_theta = (**centroid_it).position;
			if (debug) cout<<"centroid_it position is: "<<temp_theta<<" and energy: "<<(**centroid_it).energy<<endl;
			energysum = (**centroid_it).energy;
			positionsum+=((**centroid_it).energy)*(temp_theta);
		}
		else {
			for (centroid_it = mStrips.begin();centroid_it != mStrips.end();centroid_it++) {
				temp_theta = (**centroid_it).position;
				if (debug) cout<<"centroid_it position is: "<<temp_theta<<" and energy: "<<(**centroid_it).energy<<endl;
				energysum+=(**centroid_it).energy;
				positionsum+=((**centroid_it).energy)*(temp_theta);
			}
		}
		mTheta = positionsum/energysum;
	}
	
	else if (det_plane == 1) {
		mTheta = 0;
	}
	
	else cout<<"ERROR: wrong entry for detector plane!"<<endl;
	
}

//--------------------------------------------------------
void StGammaStripCluster::CalcPosition(int det_plane) {
	
	if (mStrips.size() == 0) return;
	
	double positionsum = 0;
	double energysum = 0;
	double temp_pos = 0;
	
	if (det_plane == 0) {
		vector<StGammaStrip*>::iterator centroid_it;
		if (mStrips.size() == 1) {
			centroid_it = mStrips.begin();
			temp_pos = -1*TMath::Log(TMath::Tan(.5*(**centroid_it).position));
			if (debug) cout<<"centroid_it position is: "<<temp_pos<<" and energy: "<<(**centroid_it).energy<<endl;
			energysum = (**centroid_it).energy;
			positionsum+=((**centroid_it).energy)*(temp_pos);
		}
		else {
			for (centroid_it = mStrips.begin();centroid_it != mStrips.end();centroid_it++) {
				temp_pos = -1*TMath::Log(TMath::Tan(.5*(**centroid_it).position));
				if (debug) cout<<"centroid_it position is: "<<temp_pos<<" and energy: "<<(**centroid_it).energy<<endl;
				energysum+=(**centroid_it).energy;
				positionsum+=((**centroid_it).energy)*(temp_pos);
			}
		}
		mPosition = positionsum/energysum;
	}
	
	else if (det_plane == 1) {
		vector<StGammaStrip*>::iterator centroid_it;
		if (mStrips.size() == 1) {
			centroid_it = mStrips.begin();
			temp_pos = (**centroid_it).position;
			if (debug) cout<<"centroid_it position is: "<<temp_pos<<" and energy: "<<(**centroid_it).energy<<endl;
			energysum+=(**centroid_it).energy;
			positionsum+=((**centroid_it).energy)*(temp_pos);
		}
		else {
			for (centroid_it = mStrips.begin();centroid_it != mStrips.end();centroid_it++) {
				temp_pos = (**centroid_it).position;
				if (debug) cout<<"centroid_it position is: "<<temp_pos<<" and energy: "<<(**centroid_it).energy<<endl;
				energysum+=(**centroid_it).energy;
				positionsum+=((**centroid_it).energy)*(temp_pos);
			}
		}
		mPosition = positionsum/energysum;
	}
	
	else cout<<"ERROR: wrong entry for detector plane!"<<endl;
	
}

//--------------------------------------------------------
void StGammaStripCluster::CalcWidth(int det_plane) {
	
	if (mStrips.size() == 0) return;
	
	double currentposition = 0;
	double maxposition = 0;
	double minposition = 0;
	
	if (det_plane == 0) {
		vector<StGammaStrip*>::iterator current_it;
		if (mStrips.size() == 1) {
			mWidth = 0;
		}
		else {
			for (current_it = mStrips.begin();current_it != mStrips.end();current_it++) {
				currentposition = -1*TMath::Log(TMath::Tan(.5*(**current_it).position));
				if (debug) cout<<"current position is: "<<currentposition<<" and energy: "<<(**current_it).energy<<endl;
				if (TMath::Abs(currentposition) > maxposition) maxposition = currentposition;
				if (TMath::Abs(currentposition) < minposition) minposition = currentposition;
			}
			mWidth = TMath::Abs(maxposition - minposition);
		}
	}
	
	else if (det_plane == 1) {
		vector<StGammaStrip*>::iterator current_it;
		if (mStrips.size() == 1) {
			mWidth = 0;
		}
		else {
			for (current_it = mStrips.begin();current_it != mStrips.end();current_it++) {
				currentposition = (**current_it).position;
				if (debug) cout<<"current position is: "<<currentposition<<" and energy: "<<(**current_it).energy<<endl;
				if (TMath::Abs(currentposition) > maxposition) maxposition = currentposition;
				if (TMath::Abs(currentposition) < minposition) minposition = currentposition;
			}
			mWidth = TMath::Abs(maxposition - minposition);
		}
	}
	
	else cout<<"ERROR: wrong entry for detector plane!"<<endl;
	
}


//-------------------------------------------------------
int StGammaStripCluster::GetNStrips() {
	int nstrips = mStrips.size();
	return nstrips;
}