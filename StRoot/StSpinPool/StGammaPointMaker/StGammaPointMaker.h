/*
 *  StGammaPointMaker.h
 *
 *  Created by Alan Hoffman on 10/22/07.
 *
 */


#ifndef StGammaPointMaker_HH
#define StGammaPointMaker_HH

#include <iostream>
#include <vector>
#include <set>
#include "StGammaMaker/StGammaCandidate.h"
#include "StGammaMaker/StGammaEvent.h"
#include "StGammaStripCluster.h"
#include "StGammaPoint.h"
#include "StGammaPizero.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TSystem.h"
#include "TVector3.h"
using namespace std;

bool PointSort(const StGammaPoint *point1,const StGammaPoint *point2);

class StGammaPointMaker 
{
	
private:
	double mStripSeed;
	double mStripAdd;
	vector<StGammaStripCluster*> EtaClusters;
	vector<StGammaStripCluster*> PhiClusters;
	vector<StGammaPoint*> PointVector;
	vector<StGammaPizero*> PizeroVector;
	set<int> UsedStrips;
	double mEnergyTot;
	TVector3 mEventVertex;
	//TH1F masshist;
	int debug;
	
public:
		
	StGammaPointMaker(char* name);
	~StGammaPointMaker() {}
	void Reset();
	
	int GetNPoints() {return PointVector.size();}
	
	void SetClusterConditions(double seed,double add);
	void SetEnergyTotal(double energytot) {mEnergyTot = energytot;}
	void SetVertex(TVector3 vertex) {mEventVertex = vertex;}
	void FindCluster(vector<StGammaStrip*> &stripVector,int det_plane);  //0 = smde, 1 = smdp
	void DrawClusters();
	void DrawStrips(vector<StGammaStrip*> &etaStrips,vector<StGammaStrip*> &phiStrips);
	vector<StGammaStripCluster*> GetEtaClusters() {return EtaClusters;}
	vector<StGammaStripCluster*> GetPhiClusters() {return PhiClusters;}
	vector<StGammaPoint*> GetPoints() {return PointVector;}
	vector<StGammaPizero*> GetPizeros() {return PizeroVector;}
	
	
	
	void MakePoints(int algo);
	//algos change the amount of assumed points (max or min right now) but can be used for development.
	
	void MakePizeros();
	
};

#endif
