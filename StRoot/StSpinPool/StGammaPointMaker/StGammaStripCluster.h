/*
 *  StGammaStripCluster.h
 *  EMCGammaCalib
 *
 *  Created by Alan Hoffman on 10/19/07.
 *
 */

#ifndef StGammaStripCluster_HH
#define StGammaStripCluster_HH

#include <iostream>
#include <vector>
#include "StGammaMaker/StGammaCandidate.h"
#include "StGammaMaker/StGammaEvent.h"
#include "StGammaMaker/StGammaStrip.h"
using namespace std;

class StGammaStripCluster 
{
	
private:
	int mDetector; //0 = SMDe  1 = SMDp
	float mPosition;
	float mWidth;
	float mEnergy;
	float mTheta;
	vector<StGammaStrip*> mStrips;
	int mStripcounter;
	int debug;
	
public:
		
	StGammaStripCluster(char* name);
	StGammaStripCluster();
	~StGammaStripCluster() {}
	
	//Getters
	float GetPosition() {return mPosition;}
	float GetEnergy() {return mEnergy;}
	int GetDetector() {return mDetector;}
	float GetWidth() {return mWidth;}
	float GetTheta() {return mTheta;}
	int GetNStrips();
	
	//Setters
	
	//Others
	void AddStrip(StGammaStrip*);
	void ClearStripVector();
	void CalcPosition(int det_plane);  //0 = smde, 1 = smdp
	void CalcWidth(int det_plane); //0 = smde, 1 = smdp
	void CalcTheta(int det_plane);
	
};

#endif
