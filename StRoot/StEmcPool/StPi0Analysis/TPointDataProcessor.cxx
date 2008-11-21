#include "TPointDataProcessor.h"
#include "StPi0AnalysisUtil.h"

#include "TAxis.h"
//#include "TVector3.h"
#include "TClass.h"

#include <cmath>
//#include <iostream>
using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(TPointDataProcessor);

TPointDataProcessor::TPointDataProcessor(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->clusterTower.SetNameTitle("clusterTower", "Tower cluster");
	this->clusterSMDE.SetNameTitle("clusterSMDE", "SMDE cluster");
	this->clusterSMDP.SetNameTitle("clusterSMDP", "SMDP cluster");
	this->clusters.SetNameTitle("clusters", "All clusters in the point");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TPointDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TPointDataProcessor::TPointDataProcessor(const this_type &processor)
	: inherited() {
	this->clusterTower.SetNameTitle("clusterTower", "Tower cluster");
	this->clusterSMDE.SetNameTitle("clusterSMDE", "SMDE cluster");
	this->clusterSMDP.SetNameTitle("clusterSMDP", "SMDP cluster");
	this->clusters.SetNameTitle("clusters", "All clusters in the point");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TPointDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	this->operator=(processor);
}

TPointDataProcessor::~TPointDataProcessor() {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DELETE(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DELETE(NAME)
#include "TPointDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TPointDataProcessor::this_type &TPointDataProcessor::operator=(const this_type &proc) {
	this->inherited::operator=(proc);
	this->multiplicityPrimaryDistributions = proc.multiplicityPrimaryDistributions;
	this->multiplicityPointsDistributions = proc.multiplicityPointsDistributions;
	this->clusterTower = proc.clusterTower;
	this->clusterSMDE = proc.clusterSMDE;
	this->clusterSMDP = proc.clusterSMDP;
	this->clusters = proc.clusters;
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_SET(proc, NAME)
#include "TPointDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	return *this;
}

void TPointDataProcessor::Print(Option_t* option) const {
	this->inherited::Print(option);
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	TString newPrefix(prefix);
	newPrefix += tab;
	for (list_distr_type::const_iterator iter = this->multiplicityPrimaryDistributions.begin();iter != this->multiplicityPrimaryDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	for (list_distr_type::const_iterator iter = this->multiplicityPointsDistributions.begin();iter != this->multiplicityPointsDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	for (list_distr_type::const_iterator iter = this->pointTrackDistDistributions.begin();iter != this->pointTrackDistDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	this->clusterTower.Print(newPrefix.Data());
	this->clusterSMDE.Print(newPrefix.Data());
	this->clusterSMDP.Print(newPrefix.Data());
	this->clusters.Print(newPrefix.Data());
}

Bool_t TPointDataProcessor::add(const inherited &processor) {
	Bool_t result = this->inherited::add(processor);
	if (result) {
		const this_type &proc = dynamic_cast<const this_type &>(processor);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_ADD(proc, hist_type, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_ADD(proc, hist2_type, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_ADD(proc, hist3_type, NAME)
#include "TPointDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
		for (list_distr_type::const_iterator iter = proc.multiplicityPrimaryDistributions.begin();iter != proc.multiplicityPrimaryDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_distr_type::iterator iterMy = this->multiplicityPrimaryDistributions.begin();iterMy != this->multiplicityPrimaryDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->multiplicityPrimaryDistributions.push_back(distr);
		}
		for (list_distr_type::const_iterator iter = proc.multiplicityPointsDistributions.begin();iter != proc.multiplicityPointsDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_distr_type::iterator iterMy = this->multiplicityPointsDistributions.begin();iterMy != this->multiplicityPointsDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->multiplicityPointsDistributions.push_back(distr);
		}
		for (list_distr_type::const_iterator iter = proc.pointTrackDistDistributions.begin();iter != proc.pointTrackDistDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_distr_type::iterator iterMy = this->pointTrackDistDistributions.begin();iterMy != this->pointTrackDistDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->pointTrackDistDistributions.push_back(distr);
		}
		this->clusterTower.add(proc.clusterTower);
		this->clusterSMDE.add(proc.clusterSMDE);
		this->clusterSMDP.add(proc.clusterSMDP);
		this->clusters.add(proc.clusters);
	}
	return result;
}

Bool_t TPointDataProcessor::process(const void *data, const void *evt, Float_t wRef) {
	Bool_t result = this->inherited::process(data, evt, wRef);
	const TMyPointData *pointPtr = 0;
	const TMyEventData *eventPtr = (const TMyEventData *)evt;
	if (!eventPtr) {
		const TMyPointTreeData *pointTree = (const TMyPointTreeData *)data;
		if (pointTree) {
			eventPtr = &pointTree->event;
			pointPtr = &pointTree->point;
		}
	} else {
		pointPtr = (const TMyPointData *)data;
	}
	if (pointPtr && eventPtr && result) {
		const cuts_type &cuts = this->getCuts();
		//const cuts_type::parameters_type &cutParameters = cuts.getParameters();
		const weight_calculator_type &weightCalculator = this->getWeightCalculator();
		const TMyPointData &point = *pointPtr;
		const TMyEventData &event = *eventPtr;
		result = false;
		TEventParameters eventParameters;
		Int_t passedEventCuts = cuts.passEventCuts(event, eventParameters);
		TPointParameters pointParameters;
		Int_t passedPointCuts = cuts.passPointCuts(event, point, eventParameters, pointParameters);
		if (passedEventCuts && passedPointCuts) {
			result = true;
			this->numPassedAllCuts++;

			Float_t w = (wRef < 0) ? weightCalculator.getWeight(event.simulatedParticle.pT) : wRef;

			for (list_distr_type::iterator iter = this->multiplicityPrimaryDistributions.begin();iter != this->multiplicityPrimaryDistributions.end();++iter) {
				distribution_type &distr = *iter;
				distr.fill(pointParameters, event.nPrimary, w);
			}
			for (list_distr_type::iterator iter = this->multiplicityPointsDistributions.begin();iter != this->multiplicityPointsDistributions.end();++iter) {
				distribution_type &distr = *iter;
				distr.fill(pointParameters, event.nPoints, w);
			}
                        for (list_distr_type::iterator iter = this->pointTrackDistDistributions.begin();iter != this->pointTrackDistDistributions.end();++iter) {
                                distribution_type &distr = *iter;
                                distr.fill(pointParameters, pointParameters.distTrack, w);
                        }
 			this->clusterTower.process(&point.clusterBTOW, &event, w);
			this->clusterSMDE.process(&point.clusterBSMDE, &event, w);
			this->clusterSMDP.process(&point.clusterBSMDP, &event, w);
			this->clusters.process(&point.clusterBTOW, &event, w);
			this->clusters.process(&point.clusterBSMDE, &event, w);
			this->clusters.process(&point.clusterBSMDP, &event, w);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       if (this->hist##NAME) this->hist##NAME->Fill(X, w);
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    if (this->hist##NAME) this->hist##NAME->Fill(X, Y, w);
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) if (this->hist##NAME) this->hist##NAME->Fill(X, Y, Z, w);
#include "TPointDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
		}
	}
	return result;
}

