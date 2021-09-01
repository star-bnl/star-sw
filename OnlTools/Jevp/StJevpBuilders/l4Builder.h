#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
//#include <DAQ_READER/daq_dta.h>
#include <TStyle.h>
#include "TVector3.h"
#include <fstream>
#include <iostream>
#include <iomanip>
#include <TH1I.h>
#include <TH1D.h>
#include <TH2F.h>
#include <TH3F.h>
#include <TFile.h> 
#include <TProfile.h>
#include "TStopwatch.h"
#include <math.h>
#include "RTS/include/HLT/HLTFormats.h"

enum {
	e,
	Pi,
	K,
	P,
	D,
	T,
	He3,
	He4
};

class l4Builder : public JevpBuilder {
	public:
		/**
		 * Plots, Functions, Histograms.
		 * @param 
		 * @return 
		 * @exception 
		 * @see 
		 * @author 
		 */  
                static const int nHltPlots = 61;
                JevpPlot *HltPlots[nHltPlots];

		static const int nBeamPlots = 3;
		JevpPlot *BeamPlots[nBeamPlots];

		static const int nBesGoodPlots = 10;
		JevpPlot *BesGoodPlots[nBesGoodPlots];

		static const int nHLTGood2Plots = 6;
	    	JevpPlot *HLTGood2Plots[nHLTGood2Plots];

		JevpPlot *BesMonitorPlots[4];
		JevpPlot *FixedTargetPlots[12];
		JevpPlot *FixedTargetMonitorPlots[6];
		JevpPlot *HeavyFragmentPlots[1];
		JevpPlot *DiElectronPlots[10];
		JevpPlot *DiElectron2TwrPlots[10];
		JevpPlot *DiPionPlots[2];
		JevpPlot *DiMuonPlots[14];
		JevpPlot *UPCDiElectronPlots[6];   
		JevpPlot *HltPlots_UPC[30];
		PlotHisto *ph;

		l4Builder(JevpServer *parent=NULL) : JevpBuilder(parent) {
			plotsetname = (char *)"l4";

		}

		void initialize(int argc, char *argv[]);   
		void startrun(daqReader *rdr);
		void stoprun(daqReader *rdr); 
		void event(daqReader *rdr);
		static void main(int argc, char *argv[]);

		int eventCounter;
		char Currentrun[256];
		char CurrentNtuple[256];
		char Currentdir[256];
		char Destindir_dat[256];
		char Destindir[256];
		char dEdxTheoDir[256];
		char dEdxMeanFiles[8][256];

		TStopwatch timer;

	private:

		void inputDedx();
		double getDedx(double p, const int name);
		void defineHltPlots();
		void defineBeamPlots();
		void defineBesGoodPlots();
		void defineHLTGood2Plots();
		void defineBesMonitorPlots();
		void defineFixedTargetPlots();
		void defineFixedTargetMonitorPlots();
		void defineHeavyFragmentPlots();
		void defineDiElectronPlots();
		void defineDiPionPlots();
		void defineDiMuonPlots();
		void defineHltPlots_UPC();
		void defineUPCDiElectronPlots();
		void defineDiElectron2TwrPlots();
		void setAllPlots();
		void writeHistogram();

		TLegendEntry *entry;
		TF1 *fTheoDedx_e_pos;
		TF1 *fTheoDedx_e_neg; 
		TF1 *fTheoDedx_Pi_pos;
		TF1 *fTheoDedx_Pi_neg;
		TF1 *fTheoDedx_K_pos; 
		TF1 *fTheoDedx_K_neg; 
		TF1 *fTheoDedx_P_pos;
		TF1 *fTheoDedx_P_neg;
		TF1 *fTheoDedx_D_pos;
		TF1 *fTheoDedx_D_neg;
		TF1 *fTheoDedx_T_pos;
		TF1 *fTheoDedx_T_neg;
		TF1 *fTheoDedx_He3_pos;
		TF1 *fTheoDedx_He3_neg;
		TF1 *fTheoDedx_He4_pos;
		TF1 *fTheoDedx_He4_neg;

		int index;
		int primaryTracks ;
		int primaryTracks_UPC ;
		int runnumber;
		int iBin;


		bool TriggerFilled;
		bool EventFilled;
		bool GlobalTracksFilled;
		bool PrimaryTracksFilled;
		bool EMCFilled;
		bool TOFFilled; 
		bool BESGoodFilled;
		bool HLTGood2Filled;
		bool BESMonitorFilled;
		bool FixedTargetFilled;
		bool FixedTargetMonitorFilled; 
		bool UPCFilled; 
		bool DiMuonFilled;
		bool UPCDiElectronFilled;
		bool DiElectronFilled;
		bool HeavyFragmentFilled;
		bool MTDQuarkoniumFilled;
		bool DiElectron2TwrFilled;

		int switch_BesGood;
		int switch_HLTGood2;
		int switch_BesMonitor;
		int switch_FixedTarget;
		int switch_FixedTargetMonitor;
		int switch_HeavyFragment;  
		int switch_jpsi;
		int switch_upc;
		double innerGainPara;
		double outerGainPara;
		double BeamX;
		double BeamY;
		double pi;
		double twopi;
		double A;

		TH1I *hEvtsAccpt;

		// track
		TH1I *hnhits;
		TH1I *hnDedx; 
		TH1D *hDcaXy;
		TH1D *hDcaZ ;
		TH1D *hDcaXy_TofMatch;
		TH1D *hDcaZ_TofMatch ;
		TH1D *hDcaXy_EMCMatch;
		TH1D *hDcaZ_EMCMatch ;

		TH1D *hLn_dEdx;
		TH1D *hGlob_Pt;
		TH1D *hGlob_Phi;
		TH1D *hGlob_Eta;
		TH2F *hGlob_dEdx;
		TH1D *hPrim_Pt;
		TH1D *hPrim_Phi;
		TH1D *hPrim_Eta;
		TH2F *hPrim_dEdx;	
		TH1I *hnhits_UPC;
		TH1I *hnDedx_UPC;
		TH1D *hDcaXy_UPC;
		TH1D *hDcaZ_UPC;
		TH1D *hLn_dEdx_UPC;
		TH1D *hGlob_Pt_UPC;
		TH1D *hGlob_Phi_UPC;
		TH1D *hGlob_Eta_UPC;
		TH2F *hGlob_dEdx_UPC;
		TH1D *hPrim_Pt_UPC;
		TH1D *hPrim_Phi_UPC;
		TH1D *hPrim_Eta_UPC;
		TH2F *hPrim_dEdx_UPC;

		// event
		TH1D *hVertexX; 
		TH1D *hVertexY;
		TH1D *hVertexZ;
		TH2D *hVertexXY;
		TH1D *hVertexR;
		TH1D *hLm_VertexX;
		TH1D *hLm_VertexY;
		TH1D *hLm_VertexZ;
		TH1I *hglobalMult;
		TH1I *hprimaryMult;
		
		JLatex* hltSummaryLine1;
		JLatex* hltSummaryLine2;
		/* TH1D *hFixed_VertexZ; */
		/* TH2D* hFixed_VertexXY; */

		/*   TH1I *hLmPrimaryMult; */

		TH1D *hVertexX_UPC; 
		TH1D *hVertexY_UPC;
		TH1D *hVertexZ_UPC;
		TH1D *hLm_VertexX_UPC;
		TH1D *hLm_VertexY_UPC;
		TH1D *hLm_VertexZ_UPC;
		TH1I *hglobalMult_UPC;
		TH1I *hprimaryMult_UPC;

		// EMC
		TH1D *hMatchPhi_Diff;
		TH1D *hTowerEnergy ;
		TH1I *hTowerDaqId; 
		TH1I *hTowerSoftId;
		TH1D *hzEdge;
		TH2F *hTowerEtaPhi;

		TH1D *hMatchPhi_Diff_UPC;
		TH1D *hTowerEnergy_UPC;
		TH1I *hTowerDaqId_UPC; 
		TH1I *hTowerSoftId_UPC;
		TH1D *hzEdge_UPC;
		TH2F *hTowerEtaPhi_UPC;

		// di-e
		TH1D *hDiElectronInvMassTpxEmc;
		TH1D *hDiElectronInvMassTpxEmcBG;
		TH1D *hDiElectronInvMassFullRange;
		TH1D *hDiElectronInvMassFullRangeBG;
		TH1D *hDiElectronInvMassCut;
		TH1D *hDiElectronInvMassCutBG;
		TH2F *hdEdx_P1; 
		TH1D *hDaughter1P_TowerEnergy;
		TH1D *hDaughter1TpxEmcInverseBeta;
		TH2F *hdEdx_P2;
		TH1D *hDaughter2P_TowerEnergy;
		TH1D *hDaughter2TpxEmcInverseBeta; 
		TH1D *hDiLeptonRapidity;

		TH1D *hDiElectronInvMassTpxEmc_Twr;
		TH1D *hDiElectronInvMassTpxEmcBG_Twr;
		TH1D *hDiElectronInvMassFullRange_Twr;
		TH1D *hDiElectronInvMassFullRangeBG_Twr;
		TH1D *hDiElectronInvMassCut_Twr;
		TH1D *hDiElectronInvMassCutBG_Twr;
		TH2F *hdEdx_P1_Twr;
		TH1D *hDaughter1P_TowerEnergy_Twr;
		TH1D *hDaughter1TpxEmcInverseBeta_Twr;
		TH2F *hdEdx_P2_Twr;
		TH1D *hDaughter2P_TowerEnergy_Twr;
		TH1D *hDaughter2TpxEmcInverseBeta_Twr;
		TH1D *hDiLeptonRapidity_Twr;

		TH1D *hDiElectronInvMassFullRange_UPC;
		TH1D *hDiElectronInvMassFullRangeBG_UPC;
		TH2F *hdEdx_P1_UPC; 
		TH1D *hDaughter1P_TowerEnergy_UPC;
		TH2F *hdEdx_P2_UPC;
		TH1D *hDaughter2P_TowerEnergy_UPC;
		TH1D *hDiLeptonRapidity_UPC;

		// di-muon
		TH1F *hMTDDiMuonJpsiMassUS;
		TH1F *hMTDDiMuonJpsiMassLS;

		TH1F *hMTDDiMuonUpsilonMassUS;
		TH1F *hMTDDiMuonUpsilonMassLS;
		TH1F *hInvMassUS;
		TH1F *hInvMassLS;
		TH2F *hMtdHitMap;        
		TH2F *hMtdMatchHitMap;    
		TH2F *hMtdDeltaZvsModule; 
		TH1F *hMtdDeltaZ;         
		TH2F *hMtdDeltaYvsModule; 
		TH1F *hMtdDeltaY;
		//MTD Quarkonium QA plots
		TH1F *hMTDQmInvMassUS;
		TH1F *hMTDQmInvMassLS;

		TH1F *hMTDQmJpsiMass_ptcut0_US;
		TH1F *hMTDQmJpsiMass_ptcut0_LS;
		TH1F *hMTDQmJpsiMass_ptcut2_US;
		TH1F *hMTDQmJpsiMass_ptcut2_LS;
		TH1F *hMTDQmJpsiMass_ptcut4_US; 
		TH1F *hMTDQmJpsiMass_ptcut4_LS;

		double US8;
		double LS8;
		TLatex *tlx8_us;
		TLatex *tlx8_ls;

		double US9;
		double LS9;
		TLatex *tlx9_us;
		TLatex *tlx9_ls;

		double US10;
		double LS10;
		TLatex *tlx10_us;
		TLatex *tlx10_ls;

		double US11;
		double LS11;
		TLatex *tlx11_us;
		TLatex *tlx11_ls;

		double US12;
		double LS12;
		TLatex *tlx12_us;
		TLatex *tlx12_ls;
		TLatex *tlxmass12;

		double US13;
		double LS13;
		TLatex *tlx13_us;
		TLatex *tlx13_ls;
		TLatex *tlxmass13;



		TH1F *hMTDQmUpsilonMassUS;
		TH1F *hMTDQmUpsilonMassLS;

		// di-pion
		TH1D *hDiPionInvMassFullRange;
		TH1D *hDiPionInvMassFullRangeBG;
		TH1D *hDiPionDeltphi;

		// ToF
		TH1D *hLocalZ;
		TH1D *hLocalY;
		/*   TH2F *hTofprimaryMult; */
		TH2F *hInverseBeta;
		TH2F *hMatchId_fiberId;
		//***********a parallel copy of hMatchId_fiberId""**********
		TH2F *hMatchId_fiberId_copy;
		TH2F *hMatchId_fiberId_copy2;
		//********************************
		TH2F *hTrayID_TrgTime;
		TH1D *hchannelID;
		TH2F *hVzvpd_lmVz ;
		TH1D *hLmVzDiff;
		TH1D *hVzvpd;
		TH1D *hVzDiff;

                TH2D *hVertexRZ;
                TH2D *hVertexXZ; 
		TProfile *hVertexXZ_pfx; JLatex* hVertexXZ_pfx_fit_res;
                TH2D *hVertexYZ; 
                TProfile *hVertexYZ_pfx; JLatex* hVertexYZ_pfx_fit_res;
                TH1D *hBunchId;
                TH1D *hBbceTAC;
                TH1D *hBbcwTAC;
                TH1D *hVpdeTAC;
                TH1D *hVpdwTAC;
                TH1D *hEpdeTAC;
                TH1D *hEpdwTAC;


                // ETOF
                TH2D *hEtofHitsXY;
                TH2D *hEtofInvBeta;
                TH2D *hEtofLocalYMrpc;
                TProfile *pEtofNhitsPerEvent;
                TH1D *hEtofDeltaT;
                double T0;
    
                /*   TH3D *hMatchannel3D ; */

		TH2F *hVzvpd_Vz_UPC ;
		TH1D *hVzDiff_UPC;

		// heavy fragments
		TH2F *hdEdx;
		TH2F *hdEdx_HeavyFragment;
		TH2F *hHFM_dEdx;
		TH2F *hdEdx_UPC;
		TH2F *hHFM_dEdx_UPC;

		// different event tpye
		TH1I *hUpc ;
		TH1I *hMtd ;
		TH1I *hNpeHt_25_NoZdc;
		TH1I *hVpdMb; 
		TH1I *hCentral;
		TH1I *hNpeHt_25;
		TH1I *hNpe;
		TH1I *hAtomcule ;

		// run-by-run display
		TH1D *hBeamX;
		TH1D *hBeamY; 
		TH1D *hInnerGain;
		TH1D *hOuterGain;
		TH1D *hMeanDcaXy;

		// BesGood
		TH2D *hBesGoodVertexXY;
		TH2D *hBesGoodVrVsVz;
		TH1D *hBesGoodVr;
		TH1D *hBesGoodVertexZ;
		TH1I *hBesGoodprimaryMult;
                TH1D *hBesGoodBunchId;
		/* TProfile *pBesGoodVxT; */
		/* TProfile *pBesGoodVyT; */
		TH2D *hBesGoodVxT;
		TH1D *hBesGoodVxT_2;
		TH2D *hBesGoodVyT;
		TH1D *hBesGoodVyT_2;

		//HLTGood2
		TH2D *hHLTGood2VertexXY;
		TH1D *hHLTGood2Vr;
		TH1D *hHLTGood2VertexZ;
		TH1I *hHLTGood2primaryMult;
		/* TProfile *pHLTGood2VzT; */
		TH2D *hHLTGood2VzT;
		TH1D *hHLTGood2VzT_2;

		//BesMonitor
		TH2D *hBesMonitorVertexXY;
		TH1D *hBesMonitorVr;
		TH1D *hBesMonitorVz;
		TH2D *hBesMonitorVertexRZ;

		//Fixed Target
		TH2D *hFixedTargetVertexXY;
		TH2D *hFixedTargetVertexYZ;
		TH1D *hFixedTargetVr;
		TH1D *hFixedTarget_VertexZ;
		TH1D *hFixedTarget_Prim_Eta;
		TH1D *hFixedTarget_Glob_Eta;
		TH1D *hFixedTargetBbceTAC;
                TH1D *hFixedTargetBbcwTAC;
                TH1D *hFixedTargetVpdeTAC;
                TH1D *hFixedTargetVpdwTAC;
                TH1D *hFixedTargetEpdeTAC;
                TH1D *hFixedTargetEpdwTAC;

		TH2D *hFixedTargetMonitorVertexXY;
		TH2D *hFixedTargetMonitorVertexYZ;
		TH1D *hFixedTargetMonitorVr;
		TH1D *hFixedTargetMonitor_VertexZ;
		TH1D *hFixedTargetMonitor_Prim_Eta;
		TH1D *hFixedTargetMonitor_Glob_Eta;
		
                unsigned int first_evt_time = 0;
                
                ClassDef(l4Builder, 1);
};
