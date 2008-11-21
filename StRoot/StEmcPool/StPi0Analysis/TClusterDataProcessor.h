#ifdef DEFINE_HISTOGRAMS

DEFINE_HISTOGRAM_1D(Detector,              cluster.detector, "Detector");
DEFINE_HISTOGRAM_1D(Size,                  cluster.size, "Cluster size;Size");
DEFINE_HISTOGRAM_1D(Energy,                cluster.energy, "Cluster energy;E, GeV");
DEFINE_HISTOGRAM_1D(EtaCoord,              cluster.etaCoord, "EtaCoord;#eta");
DEFINE_HISTOGRAM_1D(PhiCoord,              cluster.phiCoord, "PhiCoord;#phi");
DEFINE_HISTOGRAM_1D(SigmaEta,              cluster.sigmaEta, "Eta RMS;#eta RMS");
DEFINE_HISTOGRAM_1D(SigmaEtaNarrow,        Int_t(TMath::Abs(cluster.etaCoord)/0.49) ? -1.0 : cluster.sigmaEta, "Eta RMS, narrow strips;#eta RMS");
DEFINE_HISTOGRAM_1D(SigmaEtaWide,          Int_t(TMath::Abs(cluster.etaCoord)/0.49) ? cluster.sigmaEta : -1.0, "Eta RMS, wide strips;#eta RMS");
DEFINE_HISTOGRAM_1D(SigmaPhi,              cluster.sigmaPhi, "Phi RMS;#phi RMS");
DEFINE_HISTOGRAM_2D(EtaPhiCoord,           cluster.etaCoord, cluster.phiCoord, "Eta-Phi Coord;#eta;#phi");
DEFINE_HISTOGRAM_1D(DeadStripClose,        ((cluster.badClose | 0x0F) ? 1 : 0), "Dead strip close");
DEFINE_HISTOGRAM_2D(HighestHitToTotalEnergy, cluster.energy, ((cluster.energy != 0) ? (cluster.highestEnergyHit.energy / cluster.energy) : -1), "Highest hit / total energy;Cluster energy, GeV;Highest hit / total energy");

#else

#ifndef StPi0Analysis_TClusterDataProcessor_H
#define StPi0Analysis_TClusterDataProcessor_H

#include <TH1F.h>
#include <TH2F.h>
#include <TH3F.h>

#include <list>
using std::list;

#include "StPi0AnalysisVersion.h"
#include "TDataProcessor.h"
#include "THitDataProcessor.h"
#include "TBinStatistics.h"
#include "TInvariantMassDistribution.h"

class TClusterDataProcessor : public TDataProcessor {
	public:
		typedef TClusterDataProcessor this_type;
		typedef TDataProcessor inherited;
		typedef TBinStatistics bin_statistics_type;
		typedef list<bin_statistics_type> list_type;
		typedef TH1F hist_type;
		typedef TH2F hist2_type;
		typedef TH3F hist3_type;
		typedef TInvariantMassDistribution distribution_type;
		typedef list<distribution_type> list_distr_type;

		TClusterDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		TClusterDataProcessor(const this_type &processor);
		virtual ~TClusterDataProcessor();

		this_type &operator=(const this_type &processor);

		virtual void Print(Option_t* option) const;

		virtual Bool_t add(const inherited &processor);
 
		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

		THitDataProcessor highestEnergyHit;

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DEF(hist_type,  NAME, TITLE)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DEF(hist2_type, NAME, TITLE)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DEF(hist3_type, NAME, TITLE)
#include "TClusterDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D

		ClassDef(TClusterDataProcessor, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_HISTOGRAMS
