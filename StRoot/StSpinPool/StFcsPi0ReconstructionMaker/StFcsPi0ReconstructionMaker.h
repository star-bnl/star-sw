// class StFcsPi0ReconstructionMaker
// author Xilin Liang

#ifndef STAR_StFcsPi0ReconstructionMaker_HH
#define STAR_StFcsPi0ReconstructionMaker_HH

#include "StMaker.h"
#include "StEnumerations.h"
#include "TCanvas.h"
#include "stdio.h"
#include "TFile.h"
#include "TH2F.h"
#include "TGraph.h"
#include "StLorentzVectorD.hh"
#include "StThreeVectorD.hh"

class TApplication;
class StFcsPi0ReconstructionMaker;
class StFcsDbMaker;
class StFcsCollection;

class StFcsPi0ReconstructionMaker : public StMaker
{

public:
	StFcsPi0ReconstructionMaker(const Char_t* name="FcsPi0F");
	~StFcsPi0ReconstructionMaker();
	Int_t Init();
	Int_t Make();
	Int_t Finish();
	void st(Int_t run_number){filename=Form("StFcsPi0invariantmass%i.root",run_number);};

private:

	StFcsDbMaker* mFcsDbMaker=0;
        StFcsCollection* mFcsColl=0;
	TH1F* h0=0; //h0:# of entries
	TH1F* h1_inv_mass_cluster=0; //h1_inv_mass_cluster:invariant mass for cluster finder analysis
	TH1F* h1_Zgg_cluster=0; //h1_Zgg_cluster:energy asymmetry for cluster finder analysis, define as : (abs(E1 - E2) / (E1 + E2)
	TH1F* h1_opening_angle_cluster=0; //h1_opening_angle_cluster:opening angle
	TH1F* h1_each_cluster_energy_nocut=0; //h1_each_cluster_energy_nocut:each cluster energy without cut for cluster finder
	TH1F* h1_each_cluster_energy_aftercut=0; //h1_each_cluster_energy_aftercut:each cluster energy after cut
	TH1F* h1_two_cluster_energy_nocut=0; //h1_two_cluster_energy_nocut:E1 +E2 without cut
	TH1F* h1_two_cluster_energy_aftercut=0; //h1_two_cluster_energy_aftercut:E1 + E2 after all cuts
	TH1F* h1_dgg_cluster=0; //h1_dgg_cluster: distance bewteen two clusters at the Ecal.
	TH2F* h2_EcalMult_vs_TOFMult=0; //h2_EcalMult_vs_TOFMult: 2D hist: Ecal Multiplicity us TOF Multiplicity
	TH2F* h2_EcalMult_vs_TOFMult_E_1=0; //h2_EcalMult_vs_TOFMult_E_1: 2D hist: Ecal Multiplicity us TOF Multiplicity after counting tower energy > 1 GeV
	TH2F* h2_EcalClustMult_vs_TOFMult=0; //h2_EcalClustMult_vs_TOFMult: 2D hist: Ecal Cluster Multiplicity us TOF Multiplicity after counting cluster energy > 1 GeV
	TH2F* h2_EcalClustMult_vs_TOFMult_E_1=0; //h2_EcalClustMult_vs_TOFMult_E_1: 2D hist: Ecal Cluster Multiplicity us TOF Multiplicity(E>1)
	TH2F* h2_dgg_cluster_vs_E1pE2_cluster=0; //h2_dgg_cluster_vs_E1pE2_cluster: 2D hist: dgg vs E1 + E2
	TH2F* h2_mass_cluster_vs_Zgg_cluster=0; //h2_mass_cluster_vs_Zgg_cluster: 2D hist:  invariant mass vs Zgg
	TH2F* h2_cluster_position=0; //h2_cluster_position: 2D hist: cluster position y vs x
	TH2F* h2_mass_cluster_vs_dgg_cluster=0; //2D hist: invariant mass vs dgg for cluster finder

	TH1F *h1list_mass_cluster_by_tower[64]; //h1list_mass_cluster_by_tower: invariant mass sorted by highest energy tower[64]
	TH1F *h1list_Etower[64]; //h1list_Etower: energy spectrum for tower (no cut)

        TH1F* h1_inv_mass_point=0; //h1_inv_mass_point:invariant mass
        TH1F* h1_Zgg_point=0; //h1_Zgg_point:Zgg
        TH1F* h1_opening_angle_point=0; //h1_opening_angle_point:opening angle
        TH1F* h1_each_point_energy_nocut=0; //h1_each_point_energy_nocut:each point energy without cut
        TH1F* h1_each_point_energy_aftercut=0; //h1_each_point_energy_aftercut:each point energy after cuts
        TH1F* h1_two_point_energy_nocut=0; //h1_two_point_energy_nocut: E1 + E2 without cut
        TH1F* h1_two_point_energy_aftercut=0; //h1_two_point_energy_aftercut: E1 + E2 after cut 
        TH1F* h1_dgg_point=0; //h1_dgg_point: distance bewteen two points at Ecal.
	TH1F* h1_x_rPosition_point=0;//h1_x_rPosition_point: point position(x) in cell
	TH1F* h1_y_rPosition_point=0;//h1_y_rPosition_point: point position(y) in cell

        TH2F* h2_EcalPointMult_vs_TOFMult=0; //h2_EcalPointMult_vs_TOFMult: 2D hist: Ecal Point Multiplicity us TOF Multiplicity
        TH2F* h2_EcalPointMult_vs_TOFMult_E_1=0; //h2_EcalPointMult_vs_TOFMult_E_1: 2D hist: Ecal Point Multiplicity us TOF Multiplicity(E>1)

        TH2F* h2_dgg_point_vs_E1pE2_point=0; //h2_dgg_point_vs_E1pE2_point: 2D hist: dgg vs E1+E2
        TH2F* h2_mass_point_vs_Zgg_point=0; //h2_mass_point_vs_Zgg_point: 2D hist: pi0 invariant mass vs Zgg
        TH2F* h2_point_position=0; //h2_point_position: 2D hist: point position y vs x
        TH2F* h2_mass_point_vs_dgg_point=0; //2D hist: invariant mass vs dgg
        TH1F *h1list_mass_point_by_tower[64]; //h1list_mass_point_by_tower: invariant mass sorted by highest energy tower (point maker)

	int mDebug=0;
        int mFilter=0;
	int mNAccepted=0;
        double mMaxEvents=1000000;
	int bins=80;//bin for invariant mass plot
	string filename;
	float m_low=-0.001;
	float m_up=0.501;//upper limit of invariant mass plot
	float EcalMult_cut=25;//upper limit of Ecal Multiplicity cut
	float TofMult_cut=60;//upper limit of Tof Multiplicity cut

	ClassDef(StFcsPi0ReconstructionMaker,0);
};
#endif


