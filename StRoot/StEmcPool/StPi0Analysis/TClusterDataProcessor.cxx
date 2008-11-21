#include "TClusterDataProcessor.h"
#include "StPi0AnalysisUtil.h"

#include "TAxis.h"
//#include "TVector3.h"
#include "TClass.h"

#include <cmath>
//#include <iostream>
using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(TClusterDataProcessor);

TClusterDataProcessor::TClusterDataProcessor(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->highestEnergyHit.SetNameTitle("highestEnergyHit", "Highest energy hit in the cluster");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TClusterDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TClusterDataProcessor::TClusterDataProcessor(const this_type &processor)
	: inherited() {
	this->highestEnergyHit.SetNameTitle("highestEnergyHit", "Highest energy hit in the cluster");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TClusterDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	this->operator=(processor);
}

TClusterDataProcessor::~TClusterDataProcessor() {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DELETE(NAME)
#include "TClusterDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TClusterDataProcessor::this_type &TClusterDataProcessor::operator=(const this_type &proc) {
	this->inherited::operator=(proc);
	this->highestEnergyHit = proc.highestEnergyHit;
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_SET(proc, NAME)
#include "TClusterDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	return *this;
}

void TClusterDataProcessor::Print(Option_t* option) const {
	this->inherited::Print(option);
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	TString newPrefix(prefix);
	newPrefix += tab;
	this->highestEnergyHit.Print(newPrefix.Data());
}

Bool_t TClusterDataProcessor::add(const inherited &processor) {
	Bool_t result = this->inherited::add(processor);
	if (result) {
		const this_type &proc = dynamic_cast<const this_type &>(processor);
		this->highestEnergyHit.add(proc.highestEnergyHit);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_ADD(proc, hist_type, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_ADD(proc, hist2_type, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_ADD(proc, hist3_type, NAME)
#include "TClusterDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	}
	return result;
}

Bool_t TClusterDataProcessor::process(const void *data, const void *evt, Float_t wRef) {
	Bool_t result = this->inherited::process(data, evt, wRef);
	const TMyClusterData *clusterPtr = 0;
	const TMyEventData *eventPtr = (const TMyEventData *)evt;
	if (!eventPtr) {
		const TMyClusterTreeData *cl = (const TMyClusterTreeData *)data;
		if (cl) {
			eventPtr = &cl->event;
			clusterPtr = &cl->cluster;
		}
	} else {
		clusterPtr = (const TMyClusterData *)data;
	}
	if (clusterPtr && eventPtr && result) {
		const cuts_type &cuts = this->getCuts();
		//const cuts_type::parameters_type &cutParameters = cuts.getParameters();
		const weight_calculator_type &weightCalculator = this->getWeightCalculator();
		const TMyClusterData &cluster = *clusterPtr;
		const TMyEventData &event = *eventPtr;
		result = false;
		TEventParameters eventParameters;
		Int_t passedEventCuts = cuts.passEventCuts(event, eventParameters);
		if (passedEventCuts) {
			result = true;
			this->numPassedAllCuts++;

			Float_t w = (wRef < 0) ? weightCalculator.getWeight(event.simulatedParticle.pT) : wRef;

			this->highestEnergyHit.process(&cluster.highestEnergyHit, &event, w);

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       if (this->hist##NAME) this->hist##NAME->Fill(X, w);
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    if (this->hist##NAME) this->hist##NAME->Fill(X, Y, w);
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) if (this->hist##NAME) this->hist##NAME->Fill(X, Y, Z, w);
#include "TClusterDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
		}
	}
	return result;
}

