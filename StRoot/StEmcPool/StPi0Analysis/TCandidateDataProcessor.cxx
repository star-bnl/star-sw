#include "TCandidateDataProcessor.h"

#include <TMath.h>
#include <TAxis.h>
//#include <TVector3.h>

//#include <iostream>
//using namespace std;

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>
#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(TCandidateDataProcessor);

TCandidateDataProcessor::TCandidateDataProcessor(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->point1.SetNameTitle("point1", "First point");
	this->point2.SetNameTitle("point2", "First point");
	this->points.SetNameTitle("points", "Both points");
	this->event1.SetNameTitle("event1", "First event");
	this->event2.SetNameTitle("event2", "Second event");
	this->events.SetNameTitle("events", "Both events");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TCandidateDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TCandidateDataProcessor::TCandidateDataProcessor(const this_type &processor)
	: inherited() {
	this->point1.SetNameTitle("point1", "First point");
	this->point2.SetNameTitle("point2", "First point");
	this->points.SetNameTitle("points", "Both points");
	this->event1.SetNameTitle("event1", "First event");
	this->event2.SetNameTitle("event2", "Second event");
	this->events.SetNameTitle("events", "Both events");
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_INIT(NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_INIT(NAME)
#include "TCandidateDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	this->operator=(processor);
}

TCandidateDataProcessor::~TCandidateDataProcessor() {
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DELETE(NAME);
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DELETE(NAME);
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DELETE(NAME);
#include "TCandidateDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
}

TCandidateDataProcessor::this_type &TCandidateDataProcessor::operator=(const this_type &proc) {
	this->inherited::operator=(proc);

	this->invariantMassDistributions = proc.invariantMassDistributions;
	this->simulatedPtDistributions = proc.simulatedPtDistributions;
	this->pointPtDistributions = proc.pointPtDistributions;
	this->multiplicityPrimaryDistributions = proc.multiplicityPrimaryDistributions;
	this->multiplicityPointsDistributions = proc.multiplicityPointsDistributions;
	this->pointTrackDistDistributions = proc.pointTrackDistDistributions;
	this->pointTrackDist2Distributions = proc.pointTrackDist2Distributions;
	this->point1 = proc.point1;
	this->point2 = proc.point2;
	this->points = proc.points;
	this->event1 = proc.event1;
	this->event2 = proc.event2;
	this->events = proc.events;
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_SET(proc, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_SET(proc, NAME)
#include "TCandidateDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
	return *this;
}

void TCandidateDataProcessor::Print(Option_t* option) const {
	this->inherited::Print(option);

	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	TString newPrefix(prefix);
	newPrefix += tab;
	this->point1.Print(newPrefix.Data());
	this->point2.Print(newPrefix.Data());
	this->points.Print(newPrefix.Data());
	this->event1.Print(newPrefix.Data());
	this->event2.Print(newPrefix.Data());
	this->events.Print(newPrefix.Data());
	for (list_type::const_iterator iter = this->invariantMassDistributions.begin();iter != this->invariantMassDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	for (list_type::const_iterator iter = this->simulatedPtDistributions.begin();iter != this->simulatedPtDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	for (list_type::const_iterator iter = this->pointPtDistributions.begin();iter != this->pointPtDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	for (list_type::const_iterator iter = this->multiplicityPrimaryDistributions.begin();iter != this->multiplicityPrimaryDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	for (list_type::const_iterator iter = this->multiplicityPointsDistributions.begin();iter != this->multiplicityPointsDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	for (list_type::const_iterator iter = this->pointTrackDistDistributions.begin();iter != this->pointTrackDistDistributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
	for (list_type::const_iterator iter = this->pointTrackDist2Distributions.begin();iter != this->pointTrackDist2Distributions.end();++iter) {
		const distribution_type &distr = *iter;
		distr.Print(newPrefix.Data());
	}
}

Bool_t TCandidateDataProcessor::add(const inherited &processor) {
	if (this->debug) cout << "TCandidateDataProcessor::add started" << endl;
	Bool_t result = this->inherited::add(processor);
	if (result) {
		if (this->debug) cout << "TCandidateDataProcessor::inherited::add passed" << endl;
		const this_type &proc = dynamic_cast<const this_type &>(processor);
		this->point1.add(proc.point1);
		this->point2.add(proc.point2);
		this->points.add(proc.points);
		this->event1.add(proc.event1);
		this->event2.add(proc.event2);
		this->events.add(proc.events);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_ADD(proc, hist_type, NAME)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_ADD(proc, hist2_type, NAME)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_ADD(proc, hist3_type, NAME)
#include "TCandidateDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
		for (list_type::const_iterator iter = proc.invariantMassDistributions.begin();iter != proc.invariantMassDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_type::iterator iterMy = this->invariantMassDistributions.begin();iterMy != this->invariantMassDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->invariantMassDistributions.push_back(distr);
		}
		for (list_type::const_iterator iter = proc.simulatedPtDistributions.begin();iter != proc.simulatedPtDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_type::iterator iterMy = this->simulatedPtDistributions.begin();iterMy != this->simulatedPtDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->simulatedPtDistributions.push_back(distr);
		}
		for (list_type::const_iterator iter = proc.pointPtDistributions.begin();iter != proc.pointPtDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_type::iterator iterMy = this->pointPtDistributions.begin();iterMy != this->pointPtDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->pointPtDistributions.push_back(distr);
		}
		for (list_type::const_iterator iter = proc.multiplicityPrimaryDistributions.begin();iter != proc.multiplicityPrimaryDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_type::iterator iterMy = this->multiplicityPrimaryDistributions.begin();iterMy != this->multiplicityPrimaryDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->multiplicityPrimaryDistributions.push_back(distr);
		}
		for (list_type::const_iterator iter = proc.multiplicityPointsDistributions.begin();iter != proc.multiplicityPointsDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_type::iterator iterMy = this->multiplicityPointsDistributions.begin();iterMy != this->multiplicityPointsDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->multiplicityPointsDistributions.push_back(distr);
		}
		for (list_type::const_iterator iter = proc.pointTrackDistDistributions.begin();iter != proc.pointTrackDistDistributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_type::iterator iterMy = this->pointTrackDistDistributions.begin();iterMy != this->pointTrackDistDistributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->pointTrackDistDistributions.push_back(distr);
		}
		for (list_type::const_iterator iter = proc.pointTrackDist2Distributions.begin();iter != proc.pointTrackDist2Distributions.end();++iter) {
			const distribution_type &distr = *iter;
			Bool_t added = false;
			for (list_type::iterator iterMy = this->pointTrackDist2Distributions.begin();iterMy != this->pointTrackDist2Distributions.end();++iterMy) added |= (*iterMy).add(distr);
			if (!added) this->pointTrackDist2Distributions.push_back(distr);
		}
	}
	if (this->debug) cout << "TCandidateDataProcessor::add finished: " << result << endl;
	return result;
}

Bool_t TCandidateDataProcessor::process(const void *data, const void *evt, Float_t wRef) {
	Bool_t result = this->inherited::process(data, evt, wRef);
	if (data && result) {
		const cuts_type &cuts = this->getCuts();
		//const cuts_type::parameters_type &cutParameters = cuts.getParameters();
		const weight_calculator_type &weightCalculator = this->getWeightCalculator();
		const TMyCandidateTreeData &candidate = *((const TMyCandidateTreeData*)data);
		result = false;
		TEventParameters eventParameters1;
		TEventParameters eventParameters2;
		Int_t passedEventCuts1 = cuts.passEventCuts(candidate.point1.event, eventParameters1);
		Int_t passedEventCuts2 = cuts.passEventCuts(candidate.point2.event, eventParameters2);
		TPointParameters pointParameters1;
		TPointParameters pointParameters2;
		Int_t passedPointCuts1 = cuts.passPointCuts(candidate.point1.event, candidate.point1.point, eventParameters1, pointParameters1);
		Int_t passedPointCuts2 = cuts.passPointCuts(candidate.point2.event, candidate.point2.point, eventParameters2, pointParameters2);
		TCandidateParameters candidateParameters;
		Int_t passedCandidateCuts = cuts.passCandidateCuts(candidate, candidate.point1.event, eventParameters1, candidate.point1.point, pointParameters1, candidate.point2.event, eventParameters2, candidate.point2.point, pointParameters2, candidateParameters);
		if (passedEventCuts1 && passedEventCuts2 && passedPointCuts1 && passedPointCuts2 && passedCandidateCuts) {
			result = true;
			this->numPassedAllCuts++;

			Float_t w = (wRef < 0) ? weightCalculator.getWeight(candidate.point1.event.simulatedParticle.pT) : wRef;

			Int_t year, day, runday;
			parseRunId(candidate.point1.event.runId, year, day, runday);
			Float_t deta = candidate.point1.point.etaCoord - candidate.point2.point.etaCoord;
			Float_t dphi = candidate.point1.point.phiCoord - candidate.point2.point.phiCoord;
			while(dphi < -TMath::Pi()) dphi += TMath::TwoPi();
			while(dphi >= TMath::Pi()) dphi -= TMath::TwoPi();

			for (list_type::iterator iter = this->invariantMassDistributions.begin();iter != this->invariantMassDistributions.end();++iter) {
				distribution_type &distr = *iter;
				distr.fill(candidateParameters, w);
			}
			TCandidateParameters tempPar(candidateParameters);
			for (list_type::iterator iter = this->simulatedPtDistributions.begin();iter != this->simulatedPtDistributions.end();++iter) {
				distribution_type &distr = *iter;
				tempPar.m = candidate.point1.event.simulatedParticle.pT;
				distr.fill(tempPar, w);
			}
			for (list_type::iterator iter = this->pointPtDistributions.begin();iter != this->pointPtDistributions.end();++iter) {
				distribution_type &distr = *iter;
				tempPar.m = pointParameters1.pTRec;
				distr.fill(tempPar, w);
				tempPar.m = pointParameters2.pTRec;
				distr.fill(tempPar, w);
			}
			for (list_type::iterator iter = this->multiplicityPrimaryDistributions.begin();iter != this->multiplicityPrimaryDistributions.end();++iter) {
				distribution_type &distr = *iter;
				//tempPar.m = candidate.point1.event.nPrimary;
				tempPar.m = candidate.point1.event.uncorrectedNumberOfTpcPrimaries;
				distr.fill(tempPar, w);
			}
			for (list_type::iterator iter = this->multiplicityPointsDistributions.begin();iter != this->multiplicityPointsDistributions.end();++iter) {
				distribution_type &distr = *iter;
				tempPar.m = candidate.point1.event.nPoints;
				distr.fill(tempPar, w);
			}
			for (list_type::iterator iter = this->pointTrackDistDistributions.begin();iter != this->pointTrackDistDistributions.end();++iter) {
				distribution_type &distr = *iter;
				tempPar.m = candidateParameters.distTrackClosest;
				distr.fill(tempPar, w);
			}
			for (list_type::iterator iter = this->pointTrackDist2Distributions.begin();iter != this->pointTrackDist2Distributions.end();++iter) {
				distribution_type &distr = *iter;
				tempPar.m = candidateParameters.distTrackClosest2;
				distr.fill(tempPar, w);
			}
			this->point1.process(&candidate.point1.point, &candidate.point1.event, w);
			this->point2.process(&candidate.point2.point, &candidate.point2.event, w);
			this->points.process(&candidate.point1.point, &candidate.point1.event, w);
			this->points.process(&candidate.point2.point, &candidate.point2.event, w);
			this->event1.process(&candidate.point1.event, &candidate.point1.event, w);
			this->event2.process(&candidate.point2.event, &candidate.point2.event, w);
			this->events.process(&candidate.point1.event, &candidate.point1.event, w);
			this->events.process(&candidate.point2.event, &candidate.point2.event, w);
#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       if (this->hist##NAME) this->hist##NAME->Fill(X, w);
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    if (this->hist##NAME) this->hist##NAME->Fill(X, Y, w);
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) if (this->hist##NAME) this->hist##NAME->Fill(X, Y, Z, w);
#include "TCandidateDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D
		}
	}
	return result;
}
