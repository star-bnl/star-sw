#ifdef DEFINE_HISTOGRAMS

DEFINE_HISTOGRAM_1D(Eta,                       gammaParameters.eta, "Eta;#eta")
DEFINE_HISTOGRAM_1D(Phi,                       gammaParameters.phi, "Phi;#phi")
DEFINE_HISTOGRAM_2D(EtaPhi,                    gammaParameters.eta, gammaParameters.phi, "Eta-Phi;#eta;#phi")
DEFINE_HISTOGRAM_1D(EtaCoord,                  gammaParameters.etaCoord, "EtaCoord;#eta")
DEFINE_HISTOGRAM_1D(PhiCoord,                  gammaParameters.phiCoord, "PhiCoord;#phi")
DEFINE_HISTOGRAM_2D(EtaPhiCoord,               gammaParameters.etaCoord, gammaParameters.phiCoord, "EtaCoord-PhiCoord;#eta;#phi")
DEFINE_HISTOGRAM_1D(Pt,                        gammaParameters.pT, "Simulated #gamma p_{T};p_{T}, GeV/c")
DEFINE_HISTOGRAM_1D(StopRadius,                gamma.stopRadius, "Simulated #gamma conversion radius;Conversion radius")
DEFINE_HISTOGRAM_2D(AssociationDelta,          gammaParameters.associatedPointDeta, gammaParameters.associatedPointDphi, "Simulated #gamma association with points;#eta_{#gamma} - #eta_{point};#phi_{#gamma} - #phi_{point}")
DEFINE_HISTOGRAM_2D(AssociationEnergy,         gammaParameters.energy, gamma.associatedPoint.energy, "Simulated #gamma energy vs. associated point energy;E_{gamma};E_{point}")

#else

#ifndef StPi0Analysis_TMCGammaDataProcessor_H
#define StPi0Analysis_TMCGammaDataProcessor_H

#include <TH1F.h>
#include <TH2F.h>

#include <list>
using std::list;

#include "StPi0AnalysisVersion.h"
#include "TDataProcessor.h"
#include "TPointDataProcessor.h"

class TMCGammaDataProcessor : public TDataProcessor {
	public:
		typedef TMCGammaDataProcessor this_type;
		typedef TDataProcessor inherited;
		typedef TH1F hist_type;
		typedef TH2F hist2_type;

		TMCGammaDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		TMCGammaDataProcessor(const this_type &processor);
		virtual ~TMCGammaDataProcessor();

		this_type &operator=(const this_type &processor);

		virtual void Print(Option_t* option) const;

		virtual Bool_t add(const inherited &processor);

		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

		TPointDataProcessor associatedPoint;
		
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DEF(hist_type,  NAME, TITLE)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DEF(hist2_type, NAME, TITLE)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DEF(hist3_type, NAME, TITLE)
#include "TMCGammaDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D

		ClassDef(TMCGammaDataProcessor, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_HISTOGRAMS
