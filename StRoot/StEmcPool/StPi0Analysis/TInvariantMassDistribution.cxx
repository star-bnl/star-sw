#include "TInvariantMassDistribution.h"
#include "TCuts.h"


#include <TClass.h>

//#include <iostream>
//using namespace std;
#include <StEmcPool/StPi0Common/Logger.h>

ClassImp(TInvariantMassDistribution);

TInvariantMassDistribution::TInvariantMassDistribution(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->debug = 0;
	this->mBinParameters.SetNameTitle("bin_parameters", "Bin parameters");
	HISTO_INIT(Distribution)
	HISTO_INIT(DistributionTower)
	HISTO_INIT(DistributionEtaCoord)
}

TInvariantMassDistribution::TInvariantMassDistribution(const this_type &inv)
	: inherited() {
	this->debug = 0;
	this->mBinParameters.SetNameTitle("bin_parameters", "Bin parameters");
	HISTO_INIT(Distribution)
	HISTO_INIT(DistributionTower)
	HISTO_INIT(DistributionEtaCoord)
	this->operator=(inv);
	if (this->debug) cout << "TInvariantMassDistribution::TInvariantMassDistribution(inv) finished" << endl;
}

TInvariantMassDistribution::TInvariantMassDistribution(const parameters_type &par)
	: inherited() {
	this->debug = 0;
	this->mBinParameters.SetNameTitle("bin_parameters", "Bin parameters");
	HISTO_INIT(Distribution)
	HISTO_INIT(DistributionTower)
	HISTO_INIT(DistributionEtaCoord)
	this->setBinParameters(par);
}

TInvariantMassDistribution::~TInvariantMassDistribution() {
	if (this->debug) cout << "~TInvariantMassDistribution " << this << " " << this->GetName() << endl;
	HISTO_DELETE(Distribution)
	HISTO_DELETE(DistributionTower)
	HISTO_DELETE(DistributionEtaCoord)
}

TInvariantMassDistribution &TInvariantMassDistribution::operator=(const this_type &inv) {
	if (this->debug) cout << "TInvariantMassDistribution::operator= started, debug=" << this->debug << endl;
	this->inherited::operator=(inv);
	if (this->debug) cout << "TInvariantMassDistribution::inherited::operator= passed" << endl;

	this->debug = inv.debug;
	MEMBER_SET(inv, BinParameters)
	HISTO_SET(inv, Distribution)
	HISTO_SET(inv, DistributionTower)
	HISTO_SET(inv, DistributionEtaCoord)
	
	if (this->debug) cout << "TInvariantMassDistribution::operator= finished" << endl;
	return *this;
}

Bool_t TInvariantMassDistribution::operator<(const this_type &inv) const {
	if (this->debug) cout << "TInvariantMassDistribution::operator< started" << endl;
	Bool_t result = true;
	const parameters_type &binParameters = this->getBinParameters();
	const parameters_type &binParametersNew = inv.getBinParameters();
	result = (binParameters < binParametersNew);
	if (this->debug) cout << "TInvariantMassDistribution::operator< finished: " << result << endl;
	return result;
}

Bool_t TInvariantMassDistribution::operator==(const parameters_type &par) const {
	Bool_t result = false;
	const parameters_type &binParameters = this->getBinParameters();
	result = (binParameters == par);
	return result;
}

Bool_t TInvariantMassDistribution::operator==(const this_type &inv) const {
	if (this->debug) cout << "TInvariantMassDistribution::operator== started" << endl;
	Bool_t result = false;
	const parameters_type &binParameters = this->getBinParameters();
	const parameters_type &binParametersNew = inv.getBinParameters();
	result = (binParameters == binParametersNew);
	if (this->debug) cout << "TInvariantMassDistribution::operator== finished: " << result << endl;
	return result;
}

Bool_t TInvariantMassDistribution::operator!=(const this_type &inv) const {
	return !this->operator==(inv);
}

Bool_t TInvariantMassDistribution::add(const this_type &inv, Bool_t check) {
	if (this->debug) cout << "TInvariantMassDistribution::add started" << endl;
	Bool_t result = false;
	if ((!check) || ((*this) == inv)) {
		result = true;
		HISTO_ADD(inv, distribution_type, Distribution)
		HISTO_ADD(inv, distribution_2d_type, DistributionTower)
		HISTO_ADD(inv, distribution_2d_type, DistributionEtaCoord)
	}
	if (this->debug) cout << "TInvariantMassDistribution::add finished: " << result << endl;
	return result;
}

void TInvariantMassDistribution::Print(Option_t* option) const {
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
	cout << prefix << tab << "debug = " << this->debug << endl;
	TString newPrefix(prefix);
	newPrefix += tab;
	const parameters_type &parameters = this->getBinParameters();
	parameters.Print(newPrefix.Data());
	const distribution_type *distribution = this->getDistribution();
	if (distribution) distribution->Print(newPrefix.Data());
	const distribution_2d_type *distributionTower = this->getDistributionTower();
	if (distributionTower) distributionTower->Print(newPrefix.Data());
	const distribution_2d_type *distributionEtaCoord = this->getDistributionEtaCoord();
	if (distributionEtaCoord) distributionEtaCoord->Print(newPrefix.Data());
}

Bool_t TInvariantMassDistribution::fill(Float_t binValue, Float_t value, Int_t towerId, Float_t etaCoord, Float_t w) {
	const parameters_type &binParameters = this->getBinParameters();
	distribution_type *distribution = this->getDistribution();
	distribution_2d_type *distributionTower = this->getDistributionTower();
	distribution_2d_type *distributionEtaCoord = this->getDistributionEtaCoord();
	
	Bool_t result = false;
	if ((binValue >= binParameters.min) && (binValue < binParameters.max)) {
		if (distribution) distribution->Fill(value, w);
		if (distributionTower) distributionTower->Fill(value, towerId, w);
		if (distributionEtaCoord) distributionEtaCoord->Fill(value, etaCoord, w);
		result = true;
	}
	return result;
}

Bool_t TInvariantMassDistribution::fill(const TCandidateParameters &candidate, Float_t w) {
	if (this->debug) cout << "TInvariantMassDistribution::fill started: w=" << w << endl;
	const parameters_type &binParameters = this->getBinParameters();
	
	Bool_t result = false;
	const Float_t *value = 0;
	if (binParameters.variable == pT) {
		value = &candidate.pTRec;
	} else if (binParameters.variable == eGamma) {
		value = &candidate.egamma;
	}
	if (value) {
		result = this->fill(*value, candidate.m, candidate.towerId, candidate.pointEtaCoord, w);
	}
	if (this->debug) cout << "TInvariantMassDistribution::fill finished: " << result << endl;
	return result;
}

Bool_t TInvariantMassDistribution::fill(const TPointParameters &point, Float_t val, Float_t w) {
	if (this->debug) cout << "TInvariantMassDistribution::fill started: w=" << w << endl;
	const parameters_type &binParameters = this->getBinParameters();
	
	Bool_t result = false;
	const Float_t *value = 0;
	if (binParameters.variable == pT) {
		value = &point.pTRec;
	} else if (binParameters.variable == eGamma) {
		value = &point.energy;
	}
	if (value) {
		result = this->fill(*value, val, point.towerId, point.etaCoord, w);
	}
	if (this->debug) cout << "TInvariantMassDistribution::fill finished: " << result << endl;
	return result;
}

