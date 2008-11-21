#include "TMCGammaDataProcessor.h"

#include "TAxis.h"
//#include "TVector3.h"

//#include <iostream>
//using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(TMCGammaDataProcessor);

TMCGammaDataProcessor::TMCGammaDataProcessor(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
        this->associatedPoint.SetNameTitle("associatedPoint", "Associated EMC point");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TMCGammaDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TMCGammaDataProcessor::TMCGammaDataProcessor(const this_type &processor)
	: inherited() {
        this->associatedPoint.SetNameTitle("associatedPoint", "Associated EMC point");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TMCGammaDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	this->operator=(processor);
}

TMCGammaDataProcessor::~TMCGammaDataProcessor() {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DELETE(NAME)
#include "TMCGammaDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TMCGammaDataProcessor::this_type &TMCGammaDataProcessor::operator=(const this_type &proc) {
	this->inherited::operator=(proc);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_SET(proc, NAME)
#include "TMCGammaDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	this->associatedPoint = proc.associatedPoint;
	return *this;
}

void TMCGammaDataProcessor::Print(Option_t* option) const {
	this->inherited::Print(option);
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	TString newPrefix(prefix);
	newPrefix += tab;
	this->associatedPoint.Print(newPrefix.Data());
}

Bool_t TMCGammaDataProcessor::add(const inherited &processor) {
	Bool_t result = this->inherited::add(processor);
	if (result) {
		const this_type &proc = dynamic_cast<const this_type &>(processor);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_ADD(proc, hist_type, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_ADD(proc, hist2_type, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_ADD(proc, hist3_type, NAME)
#include "TMCGammaDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	    this->associatedPoint.add(proc.associatedPoint);
	}
	return result;
}

Bool_t TMCGammaDataProcessor::process(const void *data, const void *evt, Float_t wRef) {
//cout << "-"; cout.flush();
	Bool_t result = this->inherited::process(data, evt, wRef);
	const TMySimulatedParticleData *gammaPtr = 0;
	const TMyEventData *eventPtr = (const TMyEventData *)evt;
	if (!eventPtr) {
		const TMyMCParticleTreeData *mcGammaTree = (const TMyMCParticleTreeData *)data;
		if (mcGammaTree) {
			eventPtr = &mcGammaTree->event;
			gammaPtr = &mcGammaTree->particle;
		}
	} else {
		gammaPtr = (const TMySimulatedParticleData *)data;
	}
	if (gammaPtr && eventPtr && result) {
		const cuts_type &cuts = this->getCuts();
    		const weight_calculator_type &weightCalculator = this->getWeightCalculator();
		const TMySimulatedParticleData &gamma = *gammaPtr;
		const TMyEventData &event = *eventPtr;
		result = false;
		TEventParameters eventParameters;
		Int_t passedEventCuts = cuts.passEventCuts(event, eventParameters);
		TGammaParameters gammaParameters;
		Int_t passedGammaCuts = cuts.passGammaCuts(event, gamma, eventParameters, gammaParameters);
//cout << "="; cout.flush();
		if (passedEventCuts && passedGammaCuts) {
			result = true;
			this->numPassedAllCuts++;

			Float_t w = (wRef < 0) ? weightCalculator.getWeight(event.simulatedParticle.pT) : wRef;
//if ((event.pTMC > 5)&&(w > 0.7)) cout << ", gamma w = " << w;
//if ((gamma.energy > 6)&&(w > 0.7)) cout << "gamma w = " << w << ", pTMC = " << event.pTMC << ", gamma E = " << gamma.energy << endl;
/*cout << ":"; *///cout.flush();
			if (gamma.associatedPoint.isValid()) this->associatedPoint.process(&gamma.associatedPoint, &event, w);
//cout << "+"; cout.flush();

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       if (this->hist##NAME) this->hist##NAME->Fill(X, w);
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    if (this->hist##NAME) this->hist##NAME->Fill(X, Y, w);
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) if (this->hist##NAME) this->hist##NAME->Fill(X, Y, Z, w);
#include "TMCGammaDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
//cout << "*"; cout.flush();
		}
	}
	return result;
}
