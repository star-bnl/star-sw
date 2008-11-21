#ifdef DEFINE_HISTOGRAMS

DEFINE_HISTOGRAM_1D(EtaCoord,              pointParameters.etaCoord, "EtaCoord;#eta");
DEFINE_HISTOGRAM_1D(PhiCoord,              pointParameters.phiCoord, "PhiCoord;#phi");
DEFINE_HISTOGRAM_2D(EtaPhiCoord,           pointParameters.etaCoord, pointParameters.phiCoord, "EtaCoord-PhiCoord;#eta;#phi");
DEFINE_HISTOGRAM_1D(Eta,                   pointParameters.eta, "Eta;#eta");
DEFINE_HISTOGRAM_1D(Phi,                   pointParameters.phi, "Phi;#phi");
DEFINE_HISTOGRAM_2D(EtaPhi,                pointParameters.eta, pointParameters.phi, "Eta-Phi;#eta;#phi");
DEFINE_HISTOGRAM_2D(EtaPhiCoordCircle,     ((pointParameters.etaCoord * 0.75) + 0.25) * cos(pointParameters.phiCoord), ((pointParameters.etaCoord * 0.75) + 0.25) * sin(pointParameters.phiCoord), "Inside is #eta=0, outside is #eta=1;x;y");
DEFINE_HISTOGRAM_1D(Pt,                    pointParameters.pTRec, "Point p_{T};p_{T}");
DEFINE_HISTOGRAM_3D(PtEtaPhiCoord,         pointParameters.pTRec, pointParameters.etaCoord, pointParameters.phiCoord, "Point p_{T} vs. #eta and #phi;p_{T};#eta;#phi");
DEFINE_HISTOGRAM_1D(EnergyTotal,           point.energy, "Total energy in point;Energy, GeV");
DEFINE_HISTOGRAM_2D(EnergySmdeSmdp,        point.clusterBSMDE.energy, point.clusterBSMDP.energy, "SMDE vs. SMDP energy in point;SMDE energy, GeV;SMDP energy, GeV");
DEFINE_HISTOGRAM_2D(EnergyTotalSmde,       point.energy, point.clusterBSMDE.energy, "Total vs. SMDE energy in point;Total energy, GeV;SMDE energy, GeV");
DEFINE_HISTOGRAM_2D(EnergyTotalSmdp,       point.energy, point.clusterBSMDP.energy, "Total vs. SMDP energy in point;Total energy, GeV;SMDP energy, GeV");
DEFINE_HISTOGRAM_2D(EnergyTowerSmde,       point.clusterBTOW.energy, point.clusterBSMDE.energy, "Tower vs. SMDE energy in point;Tower energy, GeV;SMDE energy, GeV");
DEFINE_HISTOGRAM_2D(EnergyTowerSmdp,       point.clusterBTOW.energy, point.clusterBSMDP.energy, "Tower vs. SMDP energy in point;Tower energy, GeV;SMDP energy, GeV");
DEFINE_HISTOGRAM_2D(EnergyTotalTower,      point.energy, point.clusterBTOW.energy, "Total vs. tower energy in point;Total energy, GeV;Tower energy, GeV");
DEFINE_HISTOGRAM_1D(EnergyAsymmetrySmdep,  pointParameters.smdAsymetry, "SMDE and SMDP energy asymmetry in point;SMD energy asymmetry");
DEFINE_HISTOGRAM_2D(DistTrackMC,           point.trackMCDeta, point.trackMCDphi, "Distance to the MC photon;#delta#eta;#delta#phi");
DEFINE_HISTOGRAM_2D(DistTrack,             point.trackDeta, point.trackDphi, "Distance to the TPC track;#delta#eta;#delta#phi");
DEFINE_HISTOGRAM_1D(DistTrack1D,           pointParameters.distTrack, "Distance to the TPC track;#sqrt{#delta#eta^{2} + #delta#phi^{2}}");
DEFINE_HISTOGRAM_2D(DistTowerCenter,       point.towerCenterDeta, point.towerCenterDphi, "Distance to the tower center;#delta#eta;#delta#phi");
DEFINE_HISTOGRAM_2D(DistTowerCenterEnergy, pointParameters.towerCenterDist, point.energy, "Distance to the tower center - energy;Distance to the tower center;Point energy");
DEFINE_HISTOGRAM_3D(SmdeCalib,             point.clusterBSMDE.highestEnergyHit.id, point.clusterBSMDE.highestEnergyHit.adc, point.clusterBTOW.energy, "SMDE calibration;SMDE SoftId;SMDE Strip ADC;Tower cluster energy, GeV");
DEFINE_HISTOGRAM_3D(SmdpCalib,             point.clusterBSMDP.highestEnergyHit.id, point.clusterBSMDP.highestEnergyHit.adc, point.clusterBTOW.energy, "SMDP calibration;SMDP SoftId;SMDP Strip ADC;Tower cluster energy, GeV");
DEFINE_HISTOGRAM_2D(DistJet,               pointParameters.jetDeltaEta, pointParameters.jetDeltaPhi, "Distance to the jet center;#delta#eta;#delta#phi");
DEFINE_HISTOGRAM_1D(DistJet1D,             pointParameters.distJet, "Distance to the jet;#sqrt{#delta#eta^{2} + #delta#phi^{2}}");
DEFINE_HISTOGRAM_2D(DayPhiCoord,           eventParameters.day, pointParameters.phiCoord, "#phi coord. vs. day;Day;#phi coord");
DEFINE_HISTOGRAM_2D(SigmaSmdeSmdp,         point.clusterBSMDE.sigmaEta, point.clusterBSMDP.sigmaPhi, "SMDE vs. SMDP sigma in point;SMDE sigma;SMDP sigma");
DEFINE_HISTOGRAM_2D(EnergyTotalSmdeSigma,  point.energy, point.clusterBSMDE.sigmaEta, "Sigma eta vs. total energy in point;Total energy, GeV;SMDE sigma");
DEFINE_HISTOGRAM_2D(EnergyTotalSmdpSigma,  point.energy, point.clusterBSMDP.sigmaPhi, "Sigma phi vs. total energy in point;Total energy, GeV;SMDP sigma");
DEFINE_HISTOGRAM_2D(EnergyTotalSmdeSize,   point.energy, point.clusterBSMDE.size, "Size eta vs. total energy in point;Total energy, GeV;SMDE size");
DEFINE_HISTOGRAM_2D(EnergyTotalSmdpSize,   point.energy, point.clusterBSMDP.size, "Size phi vs. total energy in point;Total energy, GeV;SMDP size");
DEFINE_HISTOGRAM_1D(PtWithSmd,             PASSED(passedPointCuts, POINT_TYPE_SMDE_CUT | POINT_TYPE_SMDP_CUT) ? pointParameters.pTRec : -1, "Point p_{T} with SMD;p_{T}");
DEFINE_HISTOGRAM_1D(PtWithSmd1,            (PASSED(passedPointCuts, POINT_TYPE_SMDE_CUT) || PASSED(passedPointCuts, POINT_TYPE_SMDP_CUT)) ? pointParameters.pTRec : -1, "Point p_{T} with at least one SMD;p_{T}");
DEFINE_HISTOGRAM_1D(PtWithSmdSize,         PASSED(passedPointCuts, POINT_SMDE_SIZE_CUT | POINT_SMDP_SIZE_CUT) ? pointParameters.pTRec : -1, "Point p_{T} with SMD size cut;p_{T}");
DEFINE_HISTOGRAM_1D(PtWithSmdSize1,        (PASSED(passedPointCuts, POINT_SMDE_SIZE_CUT) || PASSED(passedPointCuts, POINT_SMDP_SIZE_CUT)) ? pointParameters.pTRec : -1, "Point p_{T} with at least one SMD size cut;p_{T}");

#else

#ifndef StPi0Analysis_TPointDataProcessor_H
#define StPi0Analysis_TPointDataProcessor_H

#include <TH1F.h>
#include <TH2F.h>
#include <TH3F.h>

#include <list>
using std::list;

#include "StPi0AnalysisVersion.h"
#include "TDataProcessor.h"
#include "TClusterDataProcessor.h"
#include "TBinStatistics.h"
#include "TInvariantMassDistribution.h"

class TPointDataProcessor : public TDataProcessor {
	public:
		typedef TPointDataProcessor this_type;
		typedef TDataProcessor inherited;
		typedef TBinStatistics bin_statistics_type;
		typedef list<bin_statistics_type> list_type;
		typedef TH1F hist_type;
		typedef TH2F hist2_type;
		typedef TH3F hist3_type;
		typedef TInvariantMassDistribution distribution_type;
		typedef list<distribution_type> list_distr_type;

		TPointDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		TPointDataProcessor(const this_type &processor);
		virtual ~TPointDataProcessor();

		this_type &operator=(const this_type &processor);

		virtual void Print(Option_t* option) const;

		virtual Bool_t add(const inherited &processor);
 
		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

		list_distr_type multiplicityPrimaryDistributions;
		list_distr_type multiplicityPointsDistributions;
                list_distr_type pointTrackDistDistributions;
	
		TClusterDataProcessor clusterTower;
		TClusterDataProcessor clusterSMDE;
		TClusterDataProcessor clusterSMDP;
		TClusterDataProcessor clusters;

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DEF(hist_type,  NAME, TITLE)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DEF(hist2_type, NAME, TITLE)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DEF(hist3_type, NAME, TITLE)
#include "TPointDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D

		ClassDef(TPointDataProcessor, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_HISTOGRAMS
