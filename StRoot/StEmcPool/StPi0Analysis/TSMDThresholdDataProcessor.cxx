#include "TSMDThresholdDataProcessor.h"
#include "StPi0AnalysisUtil.h"

#include "TAxis.h"
//#include "TVector3.h"
#include "TClass.h"

#include <cmath>
//#include <iostream>
using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(TSMDThresholdDataProcessor);

TSMDThresholdDataProcessor::TSMDThresholdDataProcessor(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TSMDThresholdDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TSMDThresholdDataProcessor::TSMDThresholdDataProcessor(const this_type &processor)
	: inherited() {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TSMDThresholdDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	this->operator=(processor);
}

TSMDThresholdDataProcessor::~TSMDThresholdDataProcessor() {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DELETE(NAME)
#include "TSMDThresholdDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TSMDThresholdDataProcessor::this_type &TSMDThresholdDataProcessor::operator=(const this_type &proc) {
	this->inherited::operator=(proc);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_SET(proc, NAME)
#include "TSMDThresholdDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	return *this;
}

void TSMDThresholdDataProcessor::Print(Option_t* option) const {
	this->inherited::Print(option);
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	TString newPrefix(prefix);
	newPrefix += tab;
}

Bool_t TSMDThresholdDataProcessor::add(const inherited &processor) {
	Bool_t result = this->inherited::add(processor);
	if (result) {
		const this_type &proc = dynamic_cast<const this_type &>(processor);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_ADD(proc, hist_type, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_ADD(proc, hist2_type, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_ADD(proc, hist3_type, NAME)
#include "TSMDThresholdDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	}
	return result;
}

Bool_t TSMDThresholdDataProcessor::process(const void *data, const void *evt, Float_t wRef) {
	Bool_t result = this->inherited::process(data, evt, wRef);
	const TMySMDThresholdData *smdEta1Threshold = 0;
	const TMySMDThresholdData *smdPhi1Threshold = 0;
	const TMySMDThresholdData *smdEta2Threshold = 0;
	const TMySMDThresholdData *smdPhi2Threshold = 0;
	const TMySMDThresholdData *smdEta3Threshold = 0;
	const TMySMDThresholdData *smdPhi3Threshold = 0;
	const TMyEventData *eventPtr = (const TMyEventData *)evt;
	if (!eventPtr) {
		const TMySMDThresholdTreeData *smdThresholdTree = (const TMySMDThresholdTreeData *)data;
		if (smdThresholdTree) {
			eventPtr = &smdThresholdTree->event;
			smdEta1Threshold = &smdThresholdTree->smdEta1;
			smdPhi1Threshold = &smdThresholdTree->smdPhi1;
			smdEta2Threshold = &smdThresholdTree->smdEta2;
			smdPhi2Threshold = &smdThresholdTree->smdPhi2;
			smdEta3Threshold = &smdThresholdTree->smdEta3;
			smdPhi3Threshold = &smdThresholdTree->smdPhi3;
		}
	}
	if (eventPtr && result) {
		const cuts_type &cuts = this->getCuts();
		//const cuts_type::parameters_type &cutParameters = cuts.getParameters();
		const weight_calculator_type &weightCalculator = this->getWeightCalculator();
		const TMyEventData &event = *eventPtr;
		result = false;
		TEventParameters eventParameters;
		Int_t passedEventCuts = cuts.passEventCuts(event, eventParameters);
		if (passedEventCuts) {
			result = true;
			this->numPassedAllCuts++;

			Float_t w = (wRef < 0) ? weightCalculator.getWeight(event.simulatedParticle.pT) : wRef;
			
			if (smdEta1Threshold) this->histSMDThresholds->Fill(1, smdEta1Threshold->threshold, w);
			if (smdPhi1Threshold) this->histSMDThresholds->Fill(2, smdPhi1Threshold->threshold, w);
			if (smdEta2Threshold) this->histSMDThresholds->Fill(3, smdEta2Threshold->threshold, w);
			if (smdPhi2Threshold) this->histSMDThresholds->Fill(4, smdPhi2Threshold->threshold, w);
			if (smdEta3Threshold) this->histSMDThresholds->Fill(5, smdEta3Threshold->threshold, w);
			if (smdPhi3Threshold) this->histSMDThresholds->Fill(6, smdPhi3Threshold->threshold, w);
			for (Int_t id = 1;id <= 18000;id++) {
			    if (smdEta1Threshold && smdEta1Threshold->channelsAboveThreshold.TestBitNumber(id)) this->histSMDStripsAboveThresholdsEta->Fill(id, 1, w);
			    if (smdPhi1Threshold && smdPhi1Threshold->channelsAboveThreshold.TestBitNumber(id)) this->histSMDStripsAboveThresholdsPhi->Fill(id, 1, w);
			    if (smdEta2Threshold && smdEta2Threshold->channelsAboveThreshold.TestBitNumber(id)) this->histSMDStripsAboveThresholdsEta->Fill(id, 2, w);
			    if (smdPhi2Threshold && smdPhi2Threshold->channelsAboveThreshold.TestBitNumber(id)) this->histSMDStripsAboveThresholdsPhi->Fill(id, 2, w);
			    if (smdEta3Threshold && smdEta3Threshold->channelsAboveThreshold.TestBitNumber(id)) this->histSMDStripsAboveThresholdsEta->Fill(id, 3, w);
			    if (smdPhi3Threshold && smdPhi3Threshold->channelsAboveThreshold.TestBitNumber(id)) this->histSMDStripsAboveThresholdsPhi->Fill(id, 3, w);
			}

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       if (this->hist##NAME) this->hist##NAME->Fill(X, w);
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    if (this->hist##NAME) this->hist##NAME->Fill(X, Y, w);
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) if (this->hist##NAME) this->hist##NAME->Fill(X, Y, Z, w);
#include "TSMDThresholdDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
		}
	}
	return result;
}

