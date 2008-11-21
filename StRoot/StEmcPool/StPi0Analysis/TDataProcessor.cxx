#include "TDataProcessor.h"

#include <TClass.h>

//#include <iostream>
//using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

ClassImp(TDataProcessor);

Bool_t TDataProcessor::compareIgnoreWeight = false;
Bool_t TDataProcessor::compareIgnoreCuts = false;

TDataProcessor::TDataProcessor(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->debug = 0;
	HISTO_INIT(Histogram);
	this->mCuts.SetNameTitle("cuts", "Cuts");
	this->mWeightCalculator.SetNameTitle("weight_calculator", "Weight calculator");
	this->numTotal = 0;
	this->numPassedAllCuts = 0;
}

TDataProcessor::TDataProcessor(const this_type &processor)
	: inherited() {
	this->debug = 0;
	HISTO_INIT(Histogram);
	this->mCuts.SetNameTitle("cuts", "Cuts");
	this->mWeightCalculator.SetNameTitle("weight_calculator", "Weight calculator");
	this->numTotal = 0;
	this->numPassedAllCuts = 0;
	this->operator=(processor);
}

TDataProcessor::~TDataProcessor() {
	//cout << "~TDataProcessor " << this << " " << this->GetName() << endl;
	HISTO_DELETE(Histogram)
}

TDataProcessor::this_type &TDataProcessor::operator=(const this_type &processor) {
	if (this->debug) cout << "TDataProcessor::operator=() started" << endl;
	this->inherited::operator=(processor);

	MEMBER_SET(processor, Cuts);
	MEMBER_SET(processor, WeightCalculator);
	HISTO_SET(processor, Histogram)
	this->setTreeName(processor.getTreeName());
	this->setBranchName(processor.getBranchName());
	this->setHistogramName(processor.getHistogramName());
	this->debug = processor.debug;
	this->numTotal = processor.numTotal;
	this->numPassedAllCuts = processor.numPassedAllCuts;

	if (this->debug) cout << "TDataProcessor::operator=() finished" << endl;
	return *this;
}

Bool_t TDataProcessor::operator<(const this_type &processor) const {
	return true;
}

Bool_t TDataProcessor::operator==(const this_type &processor) const {
	if (this->debug) cout << "TDataProcessor::operator==() started" << endl;
	Bool_t result = true;

	if (result) result &= (strcmp(this->GetName(), processor.GetName()) == 0);
	if (this->debug) cout << "name: " << result << endl;
	if (result) result &= (strcmp(this->getTreeName(), processor.getTreeName()) == 0);
	if (this->debug) cout << "tree name: " << result << endl;
	if (result) result &= (strcmp(this->getBranchName(), processor.getBranchName()) == 0);
	if (this->debug) cout << "branch name: " << result << endl;
	if (result) result &= (strcmp(this->getHistogramName(), processor.getHistogramName()) == 0);
	if (this->debug) cout << "histogram name: " << result << endl;
	if (!this_type::compareIgnoreCuts) {
	    if (result) result &= (this->getCuts() == processor.getCuts());
	    if (this->debug) cout << "cuts: " << result << endl;
	}
	if (!this_type::compareIgnoreWeight) {
	    if (result) result &= (this->getWeightCalculator() == processor.getWeightCalculator());
	    if (this->debug) cout << "weight: " << result << endl;
	}

	if (this->debug) cout << "TDataProcessor::operator==() finished: " << result << endl;
	return result;
}

Bool_t TDataProcessor::operator!=(const this_type &processor) const {
	return !this->operator==(processor);
}

void TDataProcessor::Print(Option_t* option) const {
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
	cout << prefix << tab << "treeName = " << (this->getTreeName() ? this->getTreeName() : "") << endl;
	cout << prefix << tab << "branchName = " << (this->getBranchName() ? this->getBranchName() : "") << endl;
	cout << prefix << tab << "histogramName = " << (this->getHistogramName() ? this->getHistogramName() : "") << endl;
	cout << prefix << tab << "histogram = " << this->getHistogram() << endl;
	cout << prefix << tab << "debug = " << this->debug << endl;
	cout << prefix << tab << "numTotal = " << this->numTotal << endl;
	cout << prefix << tab << "numPassedAllCuts = " << this->numPassedAllCuts << endl;
	TString newPrefix(prefix);
	newPrefix += tab;
	const cuts_type &cuts = this->getCuts();
	cuts.Print(newPrefix.Data());
	const weight_calculator_type &weightCalculator = this->getWeightCalculator();
	weightCalculator.Print(newPrefix.Data());
}

Bool_t TDataProcessor::process(const void *data, const void *evt, Float_t wRef) {
	if (this->debug) cout << "TDataProcessor::process() started" << endl;
	Bool_t result = false;
	if (data) {
		result = true;
		this->numTotal++;
		if (strcmp(this->getHistogramName(), "") != 0) {
		    const TH1 *histogram = (const TH1*)data;
		    if (histogram && (strcmp(this->getHistogramName(), histogram->GetName()) == 0)) {
			this->histHistogram = (TH1F*)addHistogram(this->histHistogram, histogram);
			this->numPassedAllCuts++;
		    }
		}
	}
	if (this->debug) cout << "TDataProcessor::process() finished" << endl;
	return result;
}

Bool_t TDataProcessor::add(const this_type &processor) {
	if (this->debug) cout << "TDataProcessor::add() started" << endl;
	Bool_t result = false;
	if (*this == processor) {
		result = true;
		this->numTotal += processor.numTotal;
		this->mCuts.add(processor.getCuts());
		this->numPassedAllCuts += processor.numPassedAllCuts;
		HISTO_ADD(processor, TH1F, Histogram)
	}
	if (this->debug) cout << "TDataProcessor::add() finished: " << result << endl;
	return result;
}
