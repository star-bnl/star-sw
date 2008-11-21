#ifdef DEFINE_HISTOGRAMS

DEFINE_HISTOGRAM_1D(Pt,                        pionParameters.pT, "#pi^{0} p_{T};#pi^{0} p_{T}")
DEFINE_HISTOGRAM_1D(MReco,                     pionParameters.m, "Reconstructed M_{inv};Reconstructed M_{#gamma #gamma}, GeV")
DEFINE_HISTOGRAM_1D(Eta,                       pionParameters.eta, "Eta;#eta")
DEFINE_HISTOGRAM_1D(Phi,                       pionParameters.phi, "Phi;#phi")
DEFINE_HISTOGRAM_2D(EtaPhi,                    pionParameters.eta, pionParameters.phi, "Eta-Phi;#eta;#phi")
DEFINE_HISTOGRAM_1D(EtaCoord,                  pionParameters.etaCoord, "EtaCoord;#eta")
DEFINE_HISTOGRAM_1D(PhiCoord,                  pionParameters.phiCoord, "PhiCoord;#phi")
DEFINE_HISTOGRAM_2D(EtaPhiCoord,               pionParameters.etaCoord, pionParameters.phiCoord, "EtaCoord-PhiCoord;#eta;#phi")
DEFINE_HISTOGRAM_1D(Z,                         pion.parent.summary.z, "Z_{#pi^{0}};Z_{#pi^{0}}")
DEFINE_HISTOGRAM_1D(ZDiff,                     eventParameters.zUse - pion.parent.summary.z, "Z_{vert} - Z_{#pi^{0}};Z_{vert} - Z_{#pi^{0}}")
DEFINE_HISTOGRAM_1D(Asymmetry,                 pionParameters.asymetry, "Asymmetry;asymmetry")
//DEFINE_HISTOGRAM_1D(GammaEnergy,               mcPion->gamma1.energy, "Simulated #gamma energy;Energy, GeV")
//DEFINE_HISTOGRAM_1D(GammaStopRadius,           mcPion->gamma1.stopRadius, "Simulated #gamma conversion radius;Conversion radius")
//DEFINE_HISTOGRAM_2D(GammaAssociationDelta,     mcPion->gamma1.associatedPointDeta, mcPion->gamma1.associatedPointDphi, "Simulated #gamma association with points;#eta_{#gamma} - #eta_{point};#phi_{#gamma} - #phi_{point}")
//DEFINE_HISTOGRAM_2D(GammaAssociationEnergy,    mcPion->gamma1.energy, mcPion->gamma1.associatedPoint.energyTotal, "Simulated #gamma energy vs. associated point energy;E_{gamma};E_{point}")
DEFINE_HISTOGRAM_1D(OpenAngleSimu,             pionParameters.openangle, "Angle simulated;open angle")
DEFINE_HISTOGRAM_1D(OpenAngleReco,             pionParameters.openangleReco, "Angle reconstructed;open angle")
DEFINE_HISTOGRAM_1D(OpenAngleResolution,       recoSimuRatio, "Angle reconstruction resolution;reconstructed / simulated open angle")
DEFINE_HISTOGRAM_2D(OpenAngleResolutionEnergy, pionParameters.energy, recoSimuRatio, "Opening angle resolution;Simulated #pi^{0} energy, GeV;Reconstructed / Simulated opening angle")
DEFINE_HISTOGRAM_2D(OpenAngleResolutionPt,     pionParameters.pT, recoSimuRatio, "Angle reconstruction resolution vs. simulated p_{T};simulated p_{T};reconstructed / simulated open angle")
DEFINE_HISTOGRAM_2D(DayRun,                    eventParameters.day, eventParameters.runDay, "Statistics per run;Day;Run");
DEFINE_HISTOGRAM_1D(StatDay,                   eventParameters.day, "Statistics per day;Day");
DEFINE_HISTOGRAM_2D(PtPartonicPt,              pionParameters.pT, event.simulatedParticle.pT, "#pi^{0} vs. PYTHIA partonic p_{T};#pi^{0} p_{T};PYTHIA partonic p_{T}")
DEFINE_HISTOGRAM_2D(EnergyOpenAngleSimu,       pionParameters.energy, pionParameters.openangle, "Angle vs. energy simulated;E, GeV/c^2;Opening angle, rad")

#else

#ifndef StPi0Analysis_TSimuDataProcessor_H
#define StPi0Analysis_TSimuDataProcessor_H

#include <TH1F.h>
#include <TH2F.h>

#include <list>
using std::list;

#include "StPi0AnalysisVersion.h"
#include "StPi0AnalysisUtil.h"
#include "TDataProcessor.h"
#include "TBinStatistics.h"
#include "TMCGammaDataProcessor.h"

class TSimuDataProcessor : public TDataProcessor {
	public:
		typedef TSimuDataProcessor this_type;
		typedef TDataProcessor inherited;
		typedef TBinStatistics bin_statistics_type;
		typedef list<bin_statistics_type> list_type;
		typedef TMCGammaDataProcessor gamma_processor_type;
		typedef TH1F hist_type;
		typedef TH2F hist2_type;

		TSimuDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		TSimuDataProcessor(const this_type &processor);
		virtual ~TSimuDataProcessor();

		this_type &operator=(const this_type &processor);

		virtual void Print(Option_t* option) const;

		virtual Bool_t add(const inherited &processor);

		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

		list_type binStatistics;
		
		 
		gamma_processor_type gamma1;
		gamma_processor_type gamma2;
		gamma_processor_type gammas;
		/*
		void setCuts(const inherited::cuts_type &newcuts) {
		    mCuts = newcuts;
		    gamma1.setCuts(newcuts);
		    gamma2.setCuts(newcuts);
		    gammas.setCuts(newcuts);
		}
		*/

                event_list_type badEvents;

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DEF(hist_type,  NAME, TITLE)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DEF(hist2_type, NAME, TITLE)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DEF(hist3_type, NAME, TITLE)
#include "TSimuDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D

		ClassDef(TSimuDataProcessor, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_HISTOGRAMS
