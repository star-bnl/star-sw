#ifdef DEFINE_HISTOGRAMS

DEFINE_HISTOGRAM_2D(SMDThresholds,               -1, -1, "SMD thresholds;Threshold ##;Threshold");
DEFINE_HISTOGRAM_2D(SMDStripsAboveThresholdsEta, -1, -1, "SMD-#eta strips above thresholds;SMD-#eta SoftID;Threshold");
DEFINE_HISTOGRAM_2D(SMDStripsAboveThresholdsPhi, -1, -1, "SMD-#phi strips above thresholds;SMD-#phi SoftID;Threshold");

#else

#ifndef StPi0Analysis_TSMDThresholdDataProcessor_H
#define StPi0Analysis_TSMDThresholdDataProcessor_H

#include <TH1F.h>
#include <TH2F.h>
#include <TH3F.h>

#include <list>
using std::list;

#include "StPi0AnalysisVersion.h"
#include "TDataProcessor.h"

class TSMDThresholdDataProcessor : public TDataProcessor {
	public:
		typedef TSMDThresholdDataProcessor this_type;
		typedef TDataProcessor inherited;
		typedef TH1F hist_type;
		typedef TH2F hist2_type;
		typedef TH3F hist3_type;

		TSMDThresholdDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		TSMDThresholdDataProcessor(const this_type &processor);
		virtual ~TSMDThresholdDataProcessor();

		this_type &operator=(const this_type &processor);

		virtual void Print(Option_t* option) const;

		virtual Bool_t add(const inherited &processor);
 
		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DEF(hist_type,  NAME, TITLE)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DEF(hist2_type, NAME, TITLE)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DEF(hist3_type, NAME, TITLE)
#include "TSMDThresholdDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D

		ClassDef(TSMDThresholdDataProcessor, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_HISTOGRAMS
