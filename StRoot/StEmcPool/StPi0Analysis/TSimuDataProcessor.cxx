#include "TSimuDataProcessor.h"

#include "TAxis.h"
//#include "TVector3.h"

//#include <iostream>
//using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(TSimuDataProcessor);

TSimuDataProcessor::TSimuDataProcessor(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->gamma1.SetNameTitle("gamma1", "Gamma1");
	this->gamma2.SetNameTitle("gamma2", "Gamma2");
	this->gammas.SetNameTitle("gammas", "Gammas");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TSimuDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TSimuDataProcessor::TSimuDataProcessor(const this_type &processor)
	: inherited() {
	this->gamma1.SetNameTitle("gamma1", "Gamma1");
	this->gamma2.SetNameTitle("gamma2", "Gamma2");
	this->gammas.SetNameTitle("gammas", "Gammas");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TSimuDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	this->operator=(processor);
}

TSimuDataProcessor::~TSimuDataProcessor() {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DELETE(NAME)
#include "TSimuDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TSimuDataProcessor::this_type &TSimuDataProcessor::operator=(const this_type &proc) {
	this->inherited::operator=(proc);
	this->binStatistics = proc.binStatistics;
	this->gamma1 = proc.gamma1;
	this->gamma2 = proc.gamma2;
	this->gammas = proc.gammas;
        this->badEvents = proc.badEvents;
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_SET(proc, NAME)
#include "TSimuDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	return *this;
}

void TSimuDataProcessor::Print(Option_t* option) const {
	this->inherited::Print(option);
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	TString newPrefix(prefix);
	newPrefix += tab;
        cout << prefix << "Bad events: " << this->badEvents.size() << endl;
	this->gamma1.Print(newPrefix.Data());
	this->gamma2.Print(newPrefix.Data());
	this->gammas.Print(newPrefix.Data());
	for (list_type::const_iterator iter = this->binStatistics.begin();iter != this->binStatistics.end();++iter) {
		const bin_statistics_type &bin = *iter;
		bin.Print(newPrefix.Data());
	}
}

Bool_t TSimuDataProcessor::add(const inherited &processor) {
	Bool_t result = this->inherited::add(processor);
	if (result) {
		const this_type &proc = dynamic_cast<const this_type &>(processor);
		this->gamma1.add(proc.gamma1);
		this->gamma2.add(proc.gamma2);
		this->gammas.add(proc.gammas);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_ADD(proc, hist_type, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_ADD(proc, hist2_type, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_ADD(proc, hist3_type, NAME)
#include "TSimuDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
                this->badEvents.insert(this->badEvents.end(), proc.badEvents.begin(), proc.badEvents.end());
		for (list_type::const_iterator iter = proc.binStatistics.begin();iter != proc.binStatistics.end();++iter) {
			const bin_statistics_type &bin = *iter;
			Bool_t added = false;
			for (list_type::iterator iterMy = this->binStatistics.begin();iterMy != this->binStatistics.end();++iterMy) {
				bin_statistics_type &binMy = *iterMy;
				Bool_t addedThis = binMy.add(bin);
				added |= addedThis;
			}
			if (!added) {
				this->binStatistics.push_back(bin);
			}
		}
	}
	return result;
}

Bool_t TSimuDataProcessor::process(const void *data, const void *evt, Float_t wRef) {
	Bool_t result = this->inherited::process(data, evt, wRef);
	const TMySimulatedDecayData *pionPtr = 0;
	const TMyEventData *eventPtr = (const TMyEventData *)evt;
	if (!eventPtr) {
		const TMyMCDecayTreeData *mcPionTree = (const TMyMCDecayTreeData *)data;
		if (mcPionTree) {
			eventPtr = &mcPionTree->event;
			pionPtr = &mcPionTree->decay;
		}
	} else {
		pionPtr = (const TMySimulatedDecayData *)data;
	}
	if (pionPtr && eventPtr && result) {
		const cuts_type &cuts = this->getCuts();
    		//const cuts_type::parameters_type &cutParameters = cuts.getParameters();
    		const weight_calculator_type &weightCalculator = this->getWeightCalculator();
		const TMySimulatedDecayData &pion = *pionPtr;
		const TMyEventData &event = *eventPtr;
		result = false;
		TEventParameters eventParameters;
		Int_t passedEventCuts = cuts.passEventCuts(event, eventParameters);
		TGammaParameters gammaParameters1;
		TGammaParameters gammaParameters2;
		Int_t passedGammaCuts1 = cuts.passGammaCuts(event, pion.daughter1, eventParameters, gammaParameters1);
		Int_t passedGammaCuts2 = cuts.passGammaCuts(event, pion.daughter2, eventParameters, gammaParameters2);
		TPionParameters pionParameters;
		Int_t passedPionCuts = cuts.passPionCuts(event, pion, eventParameters, gammaParameters1, gammaParameters2, pionParameters);
		if (passedEventCuts && passedGammaCuts1 && passedGammaCuts2 && passedPionCuts) {
			result = true;
			this->numPassedAllCuts++;

			Float_t w = (wRef < 0) ? weightCalculator.getWeight(event.simulatedParticle.pT) : wRef;

//if (!(((pion.gamma1.energy>6)||(pion.gamma2.energy>6))&&(w > 0.7))) return result;

			Float_t recoSimuRatio = (pionParameters.openangle != 0) ? (pionParameters.openangleReco / pionParameters.openangle) : 0;
			for (list_type::iterator iter = this->binStatistics.begin();iter != this->binStatistics.end();++iter) {
				bin_statistics_type &bin = *iter;
				//bin.fill(event.simulatedParticle.pT, pT, w);
				bin.fill(pion.parent.summary.pT, pT, w);
			}
			this->gamma1.process(&pion.daughter1, &event, w);
			this->gamma2.process(&pion.daughter2, &event, w);
			this->gammas.process(&pion.daughter1, &event, w);
//if ((event.pTMC > 5)&&(w > 0.7)) cout << "pi0 w = " << w;
			this->gammas.process(&pion.daughter2, &event, w);
//if ((event.pTMC > 5)&&(w > 0.7)) cout << endl;
			if ((cuts.getParametersPion().pythiaPi0PtCutoff > 0) && (pion.parent.summary.pT > cuts.getParametersPion().pythiaPi0PtCutoff)) {
			    this->badEvents.push_back(pair<Int_t, Int_t>(event.runId, event.eventId));
			}

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       if (this->hist##NAME) this->hist##NAME->Fill(X, w);
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    if (this->hist##NAME) this->hist##NAME->Fill(X, Y, w);
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) if (this->hist##NAME) this->hist##NAME->Fill(X, Y, Z, w);
#include "TSimuDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
		}
	}
	return result;
}
