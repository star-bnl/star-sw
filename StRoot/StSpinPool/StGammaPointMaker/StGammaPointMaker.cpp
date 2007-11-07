/*
 *  StGammaPointMaker.cpp
 *
 *  Created by Alan Hoffman on 10/22/07.
 *
 */

#include "StGammaPointMaker.h"

StGammaPointMaker::StGammaPointMaker(char* name)
{
	cout<<"making new pointmaker"<<endl;
	EtaClusters.clear();
	PhiClusters.clear();
	PointVector.clear();
	mEnergyTot = 0;
	mStripSeed = .4;
	mStripAdd = .01;
	debug = 0;
}


//------------------------------------------------------
void StGammaPointMaker::SetClusterConditions(double seed,double add) {
	cout<<"Setting pointmaking cuts"<<endl;
	mStripSeed = seed;
	mStripAdd = add;
}

//------------------------------------------------------
void StGammaPointMaker::Reset() {
	EtaClusters.clear();
	PhiClusters.clear();
	PointVector.clear();
	PizeroVector.clear();
	UsedStrips.clear();
	mEnergyTot = 0;
	mStripSeed = .4;
	mStripAdd = .01;
	debug = 0;
}

//------------------------------------------------------
void StGammaPointMaker::DrawClusters() {
	
	TH1F temp_eta = TH1F("eta","eta",400,-1.,1.);
	temp_eta.Clear();
	//temp_eta.GetXaxis()->SetRangeUser(0.,.2);
	TH1F temp_phi = TH1F("phi","phi",1000,-3.2,3.2);
	temp_phi.Clear();
	//temp_phi.GetXaxis()->SetRangeUser(-1.5,-1.2);
	
	vector<StGammaStripCluster*>::iterator temp_it;
	for (temp_it = EtaClusters.begin();temp_it != EtaClusters.end();temp_it++) temp_eta.Fill((**temp_it).GetPosition(),(**temp_it).GetEnergy());
	for (temp_it = PhiClusters.begin();temp_it != PhiClusters.end();temp_it++) temp_phi.Fill((**temp_it).GetPosition(),(**temp_it).GetEnergy());
	
	TCanvas *c = new TCanvas("c","c",800,650);
	c->cd(1);
	temp_eta.Draw();
	TCanvas *c1 = new TCanvas("c1","c1",800,650); 
	c1->cd(2);
	temp_phi.Draw();
	
	c->SaveAs("./plots/etaclusters.eps");
	gSystem->Exec("pstopdf ./plots/etaclusters.eps -o ./plots/etaclusters.pdf");
	
	c1->SaveAs("./plots/phiclusters.eps");
	gSystem->Exec("pstopdf ./plots/phiclusters.eps -o ./plots/phiclusters.pdf");
	
	delete c;
	delete c1;
}

//------------------------------------------------------
void StGammaPointMaker::DrawStrips(vector<StGammaStrip*> &etaStrips,vector<StGammaStrip*> &phiStrips) {
	
	TH1F temp_eta = TH1F("eta","eta",400,-1.,1.);
	temp_eta.Clear();
	//temp_eta.GetXaxis()->SetRangeUser(0.,.2);
	TH1F temp_phi = TH1F("phi","phi",1000,-3.2,3.2);
	temp_phi.Clear();
	//temp_phi.GetXaxis()->SetRangeUser(-1.5,-1.2);
	float temp_pos;
	
	vector<StGammaStrip*>::iterator temp_it;
	for (temp_it = etaStrips.begin();temp_it != etaStrips.end();temp_it++) {
		temp_pos = -1*TMath::Log(TMath::Tan(.5*(**temp_it).position));
		temp_eta.Fill(temp_pos,(**temp_it).energy);
	}
	for (temp_it = phiStrips.begin();temp_it != phiStrips.end();temp_it++) temp_phi.Fill((**temp_it).position,(**temp_it).energy);
	
	TCanvas *c = new TCanvas("c","c",800,650);
	c->cd(1);
	temp_eta.Draw();
	TCanvas *c1 = new TCanvas("c1","c1",800,650); 
	c1->cd(2);
	temp_phi.Draw();
	
	c->SaveAs("./plots/etastrips.eps");
	gSystem->Exec("pstopdf ./plots/etastrips.eps -o ./plots/etastrips.pdf");
	
	c1->SaveAs("./plots/phistrips.eps");
	gSystem->Exec("pstopdf ./plots/phistrips.eps -o ./plots/phistrips.pdf");
	
	delete c;
	delete c1;
}


//------------------------------------------------------
// inputs a vector of gamma strips, finds biggest cluster and adds it to cluster vector, returns
// strip vector with clustered strips removed
void StGammaPointMaker::FindCluster(vector<StGammaStrip*> &stripVector,int det_plane) {
	
	///first things first, if the vector is empty break
	if (stripVector.size() == 0) {
		if (debug) cout<<"ERROR: StripVector is Empty!"<<endl;
		return;
	}
	
	StGammaStripCluster *cluster = new StGammaStripCluster();
	cluster->ClearStripVector();
	float currentEnergy;
	float maxEnergy;
	float currentPosition;
	
	//find max strip
	vector<StGammaStrip*>::iterator current_strip;
	vector<StGammaStrip*>::iterator max_strip = stripVector.begin();
	for (current_strip = stripVector.begin();current_strip != stripVector.end();current_strip++) {
		if (UsedStrips.count((**current_strip).index)) continue;
		if ((**current_strip).energy > (**max_strip).energy) max_strip = current_strip;
	}
	
	//Start with the max strip
	if (debug) cout<<"adding max strip"<<endl;
	if (debug) cout<<"max strip energy is: "<<(**max_strip).energy<<endl;
	if (UsedStrips.count((**max_strip).index) == 0) {
		if ((**max_strip).energy > mStripSeed) {
			cluster->AddStrip(*max_strip);
			maxEnergy = (**max_strip).energy;
			currentEnergy = maxEnergy;
			currentPosition = (**max_strip).position;
			UsedStrips.insert((**max_strip).index);
		}
		else return;
	}
	
	//look at the strips "before"...
	for (current_strip = max_strip-1;current_strip != stripVector.begin()-1;current_strip--) {
		if (debug) cout<<"checking strip before"<<endl;
		if (debug) cout<<"current energy is: "<<(**current_strip).energy<<endl;
		if (UsedStrips.count((**current_strip).index)) break;
		if (((**current_strip).energy < mStripAdd) || ((**current_strip).energy >= currentEnergy)) {
			if ((**current_strip).position != currentPosition) break;
		}
		cluster->AddStrip(*current_strip);
		currentEnergy = (**current_strip).energy;
		currentPosition = (**current_strip).position;
		UsedStrips.insert((**current_strip).index);
	}
	
	currentEnergy = maxEnergy;
	
	//look at the strips "after" (i.e plus on the vector) to see if they should be added to main cluster or start thier own or neither.
	for (current_strip = max_strip+1;current_strip != stripVector.end();current_strip++) {
		if (debug) cout<<"checking strip after"<<endl;
		if (debug) cout<<"current energy is: "<<(**current_strip).energy<<endl;
		if (UsedStrips.count((**current_strip).index)) break;
		if (((**current_strip).energy < mStripAdd) || ((**current_strip).energy >= currentEnergy)) {
			if ((**current_strip).position != currentPosition) break;
		}
		cluster->AddStrip(*current_strip);
		currentEnergy = (**current_strip).energy;
		currentPosition = (**current_strip).position;
		UsedStrips.insert((**current_strip).index);
	}
	
	if (debug) cout<<"done adding strips"<<endl;
	cluster->CalcPosition(det_plane);
	cluster->CalcWidth(det_plane);
	cluster->CalcTheta(det_plane);
	if (debug) cout<<"energy weighted position is: "<<cluster->GetPosition()<<endl;
	if ((det_plane == 0) && (cluster->GetNStrips() > 0)) EtaClusters.push_back(cluster);
	if ((det_plane == 1) && (cluster->GetNStrips() > 0)) PhiClusters.push_back(cluster);
	cluster->ClearStripVector();
	if (debug) cout<<"cluster energy is: "<<cluster->GetEnergy()<<endl;
	
	return;
}

//------------------------------------------------------
//makes points out of the inherent vectors owned by the point maker.
//"good" points are added to the pointvector.
void StGammaPointMaker::MakePoints(int algo) {
	
	if ((EtaClusters.size() == 0) || (PhiClusters.size() == 0)) return;
	
	vector<StGammaPoint*> pointlist;
	int maxpoints = 0;
	int etacounter = 0;
	int phicounter = 0;
	double stripenergytot = 0;
	double pointenergy = 0;
	TVector3 absolutepos;
	
	StGammaPoint *point = new StGammaPoint();
	
	vector<StGammaStripCluster*>::iterator eta_iter;
	vector<StGammaStripCluster*>::iterator phi_iter;
	vector<StGammaPoint*>::iterator point_iter;
	
	for (eta_iter = EtaClusters.begin();eta_iter != EtaClusters.end();eta_iter++) {
		etacounter++;
		for (phi_iter = PhiClusters.begin();phi_iter != PhiClusters.end();phi_iter++) {
			point->Reset();
			point->SetEta((**eta_iter).GetPosition());
			if (debug) cout<<"temp point eta position: "<<point->GetEta()<<endl;
			point->SetPhi((**phi_iter).GetPosition());
			if (debug) cout<<"temp point phi position: "<<point->GetPhi()<<endl;
			point->SetTheta((**eta_iter).GetTheta());
			if (debug) cout<<"temp point theta position: "<<point->GetTheta()<<endl;
			point->SetEtaClusterEnergy((**eta_iter).GetEnergy());
			point->SetPhiClusterEnergy((**phi_iter).GetEnergy());
			pointlist.push_back(point);
			point = new StGammaPoint();
		}
	}
	
	for (point_iter = pointlist.begin();point_iter != pointlist.end();point_iter++) {
		double asym = (**point_iter).GetAsym();
		if (debug) cout<<"point asymmetry is: "<<asym<<endl;
	}
	
	//sorting list...
	sort(pointlist.begin(),pointlist.end(),PointSort);
	
	//algo 0 assumes the minimum number of points; algo 1 assumes the maximum; algo 2 assumes maximum possible of 2
	if (algo == 0) {
		maxpoints = EtaClusters.size();
		if (PhiClusters.size() < EtaClusters.size()) maxpoints = PhiClusters.size();
		}
	else if (algo == 1) {
		maxpoints = EtaClusters.size();
		if (PhiClusters.size() > EtaClusters.size()) maxpoints = PhiClusters.size();
	}
	else if (algo == 2) {
		if ((PhiClusters.size() == 1) && (EtaClusters.size() == 1)) maxpoints = 1;
		else maxpoints = 2;
	}
	else if (debug) cout<<"ERROR: wrong algo specified"<<endl;
	
	for (point_iter = pointlist.begin();point_iter != pointlist.begin()+maxpoints;point_iter++) {
		PointVector.push_back(*point_iter);
	}
	
	//get the total energy in the strip clusters.
	for (point_iter = PointVector.begin();point_iter != PointVector.end();point_iter++) {
		stripenergytot+=(**point_iter).GetEtaClusterEnergy();
		stripenergytot+=(**point_iter).GetPhiClusterEnergy();
	}	
	
	//for each good point, add things like energy and position
	if (debug) cout<<"After taking only best points"<<endl;
	if (debug) cout<<"total cluster energy is: "<<mEnergyTot<<endl;
	for (point_iter = PointVector.begin();point_iter != PointVector.end();point_iter++) {
		double asym = (**point_iter).GetAsym();
		pointenergy = (mEnergyTot/stripenergytot)*((**point_iter).GetEtaClusterEnergy() + (**point_iter).GetPhiClusterEnergy());
		(**point_iter).SetEnergy(pointenergy);
		if (debug) cout<<"point energy is... "<<(**point_iter).GetEnergy()<<endl;
		if (debug) cout<<"point asymmetry is: "<<(**point_iter).GetAsym()<<endl;
		absolutepos.SetPtEtaPhi(231.23,(**point_iter).GetEta(),(**point_iter).GetPhi());
		(**point_iter).SetPosition(absolutepos - mEventVertex);
	}
	
	return;
}

//------------------------------------------------------
//makes pionss out of the inherent vectors owned by the point maker.
void StGammaPointMaker::MakePizeros() {
	
	vector<StGammaPoint*> pvec1 = PointVector;
	//vector<StGammaPoint*> pvec2 = PointVector;
	vector<StGammaPoint*>::iterator point_iter1;
	vector<StGammaPoint*>::iterator point_iter2;
	
	StGammaPizero *cand = new StGammaPizero();
	
	//loop over all the points and calculate masses
	for (point_iter1 = pvec1.begin();point_iter1 != pvec1.end();point_iter1++) {
		for (point_iter2 = point_iter1;point_iter2 != pvec1.end();point_iter2++) {
			float costheta = (**point_iter1).GetPosition().Unit().Dot((**point_iter2).GetPosition().Unit());
			if (debug) cout<<"the cos of the angle is: "<<costheta<<endl;
			
			if (costheta == 1) continue;
			
			//fill pion candidate
			float minv2 = (2*(**point_iter1).GetEnergy()*(**point_iter2).GetEnergy()*(1-costheta));
			cand->SetMass(TMath::Sqrt(minv2));
			cand->SetCosAngle(costheta);
			cand->SetAsymmetry(((**point_iter1).GetEnergy() - (**point_iter2).GetEnergy())/((**point_iter1).GetEnergy() + (**point_iter2).GetEnergy()));
			cand->SetDistance(TMath::Sqrt(((**point_iter1).GetEta() - (**point_iter2).GetEta())*((**point_iter1).GetEta() - (**point_iter2).GetEta()) + 
										  ((**point_iter1).GetPhi() - (**point_iter2).GetPhi())*((**point_iter1).GetPhi() - (**point_iter2).GetPhi())));
			cand->SetEnergy1((**point_iter1).GetEnergy());
			cand->SetEnergy2((**point_iter2).GetEnergy());
			cand->SetEta(((**point_iter1).GetEta()*(**point_iter1).GetEnergy() + (**point_iter2).GetEta()*(**point_iter2).GetEnergy())/((**point_iter1).GetEnergy() + (**point_iter2).GetEnergy()));
			cand->SetEta1((**point_iter1).GetEta());
			cand->SetEta2((**point_iter2).GetEta());
			cand->SetPhi(((**point_iter1).GetPhi()*(**point_iter1).GetEnergy() + (**point_iter2).GetPhi()*(**point_iter2).GetEnergy())/((**point_iter1).GetEnergy() + (**point_iter2).GetEnergy()));
			cand->SetPhi1((**point_iter1).GetPhi());
			cand->SetPhi2((**point_iter2).GetPhi());
			cand->SetPt((**point_iter1).GetEnergy()*TMath::Sin((**point_iter1).GetTheta()) + (**point_iter2).GetEnergy()*TMath::Sin((**point_iter2).GetTheta()));
			if (debug) cout<<"Pion mass is: "<<cand->Mass()<<endl;
			if (debug) cout<<"poin pt is: "<<cand->Pt()<<endl;
			PizeroVector.push_back(cand);
			cand = new StGammaPizero();
		}
	}
	
	return;
}

//------------------------------------------------------
bool PointSort(const StGammaPoint *point1,const StGammaPoint *point2) {
	if ((*point1).GetAsym() > (*point2).GetAsym()) return false;
	return true;
}
