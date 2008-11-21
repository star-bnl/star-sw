#ifdef DEFINE_HISTOGRAMS

DEFINE_HISTOGRAM_1D(Detector,              hit.detector, "Detector");
DEFINE_HISTOGRAM_1D(Id,                    hit.id, "ID;ID");
DEFINE_HISTOGRAM_1D(Adc,                   hit.adc, "ADC;ADC");
DEFINE_HISTOGRAM_1D(Adc2,                  hit.adc, "ADC2;ADC");
DEFINE_HISTOGRAM_1D(Adc3,                  hit.adc, "ADC3;ADC");
DEFINE_HISTOGRAM_1D(AdcPed,                hit.adc - hitParameters.ped, "ADC-PED;ADC-PED");
DEFINE_HISTOGRAM_1D(EtaCoord,              hitParameters.etaCoord, "EtaCoord;#eta");
DEFINE_HISTOGRAM_1D(PhiCoord,              hitParameters.phiCoord, "PhiCoord;#phi");
DEFINE_HISTOGRAM_2D(EtaPhiCoord,           hitParameters.etaCoord, hitParameters.phiCoord, "Eta-Phi Coord;#eta;#phi");
DEFINE_HISTOGRAM_1D(Energy,                hit.energy, "Hit energy;E, GeV");
DEFINE_HISTOGRAM_1D(Et,                    hitParameters.eT, "Hit E_{T};E_{T}, GeV");

#else

#ifndef StPi0Analysis_THitDataProcessor_H
#define StPi0Analysis_THitDataProcessor_H

#include <TH1F.h>
#include <TH2F.h>
#include <TH3F.h>

#include <list>
using std::list;

#include "StPi0AnalysisVersion.h"
#include "TDataProcessor.h"
#include "TBinStatistics.h"
#include "TInvariantMassDistribution.h"

class THitDataProcessor : public TDataProcessor {
	public:
		typedef THitDataProcessor this_type;
		typedef TDataProcessor inherited;
		typedef TBinStatistics bin_statistics_type;
		typedef list<bin_statistics_type> list_type;
		typedef TH1F hist_type;
		typedef TH2F hist2_type;
		typedef TH3F hist3_type;
		typedef TInvariantMassDistribution distribution_type;
		typedef list<distribution_type> list_distr_type;

		THitDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		THitDataProcessor(const this_type &processor);
		virtual ~THitDataProcessor();

		this_type &operator=(const this_type &processor);

		virtual void Print(Option_t* option) const;

		virtual Bool_t add(const inherited &processor);
 
		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DEF(hist_type,  NAME, TITLE)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DEF(hist2_type, NAME, TITLE)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DEF(hist3_type, NAME, TITLE)
#include "THitDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D

		ClassDef(THitDataProcessor, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_HISTOGRAMS
