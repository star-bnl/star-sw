#include "TEventDataProcessor.h"

#include "TAxis.h"
//#include "TVector3.h"
#include "TClass.h"

//#include <iostream>
//using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(TEventDataProcessor);

TEventDataProcessor::TEventDataProcessor(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->highestAdcHit.SetNameTitle("highestAdcHit", "Highest ADC hit in the event");
	this->highestEtHit.SetNameTitle("highestEtHit", "Highest E_{T} hit in the event");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TEventDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TEventDataProcessor::TEventDataProcessor(const this_type &processor)
	: inherited() {
	this->highestAdcHit.SetNameTitle("highestAdcHit", "Highest ADC hit in the event");
	this->highestEtHit.SetNameTitle("highestEtHit", "Highest E_{T} hit in the event");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TEventDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	this->operator=(processor);
}

TEventDataProcessor::~TEventDataProcessor() {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DELETE(NAME)
#include "TEventDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TEventDataProcessor::this_type &TEventDataProcessor::operator=(const this_type &proc) {
	this->inherited::operator=(proc);
	this->binStatistics = proc.binStatistics;
	this->highestAdcHit = proc.highestAdcHit;
	this->highestEtHit = proc.highestEtHit;
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_SET(proc, NAME)
#include "TEventDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	return *this;
}

void TEventDataProcessor::Print(Option_t* option) const {
	this->inherited::Print(option);
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	TString newPrefix(prefix);
	newPrefix += tab;
	for (list_type::const_iterator iter = this->binStatistics.begin();iter != this->binStatistics.end();++iter) {
		const bin_statistics_type &bin = *iter;
		bin.Print(newPrefix.Data());
	}
	this->highestAdcHit.Print(newPrefix.Data());
	this->highestEtHit.Print(newPrefix.Data());
}

Bool_t TEventDataProcessor::add(const inherited &processor) {
	Bool_t result = this->inherited::add(processor);
	if (result) {
		const this_type &proc = dynamic_cast<const this_type &>(processor);
		this->highestAdcHit.add(proc.highestAdcHit);
		this->highestEtHit.add(proc.highestEtHit);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_ADD(proc, hist_type, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_ADD(proc, hist2_type, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_ADD(proc, hist3_type, NAME)
#include "TEventDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
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

Bool_t TEventDataProcessor::process(const void *data, const void *evt, Float_t wRef) {
	Bool_t result = this->inherited::process(data, evt, wRef);
	const TMyEventData *eventPtr = (const TMyEventData *)evt;
	if (!eventPtr) {
		const TMyEventTreeData *eventTree = (const TMyEventTreeData *)data;
		if (eventTree) eventPtr = &eventTree->event;
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

			for (list_type::iterator iter = this->binStatistics.begin();iter != this->binStatistics.end();++iter) {
				bin_statistics_type &bin = *iter;
				bin.fill(w);
			}
			this->highestAdcHit.process(&event.triggerSimulatedFinal.highestAdcHit, &event, w);
			this->highestEtHit.process(&event.triggerSimulatedFinal.highestEtHit, &event, w);
			if (this->histDayRunAcceptanceBTOW) this->histDayRunAcceptanceBTOW->Fill(eventParameters.day, eventParameters.runDay, w * event.acceptanceBTOW / 4800.0);
#ifdef SAVE_BPRS 
			if (this->histDayRunAcceptanceBPRS) this->histDayRunAcceptanceBPRS->Fill(eventParameters.day, eventParameters.runDay, w * event.acceptanceBPRS / 4800.0);
#endif
			if (this->histDayRunAcceptanceBSMDE) this->histDayRunAcceptanceBSMDE->Fill(eventParameters.day, eventParameters.runDay, w * event.acceptanceBSMDE / 18000.0);
			if (this->histDayRunAcceptanceBSMDP) this->histDayRunAcceptanceBSMDP->Fill(eventParameters.day, eventParameters.runDay, w * event.acceptanceBSMDP / 18000.0);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       if (this->hist##NAME) this->hist##NAME->Fill(X, w);
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    if (this->hist##NAME) this->hist##NAME->Fill(X, Y, w);
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) if (this->hist##NAME) this->hist##NAME->Fill(X, Y, Z, w);
#include "TEventDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
		}
	}
	return result;
}

