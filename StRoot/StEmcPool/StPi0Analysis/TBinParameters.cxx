#include "TBinParameters.h"

#include "TClass.h"

#include <StEmcPool/StPi0Common/Logger.h>
#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

ClassImp(TBinParameters);

TBinParameters::TBinParameters(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->variable = (TBinVariable)0;
	this->min = 0;
	this->max = 0;
	this->trueCenter = 0;
}

TBinParameters::TBinParameters(const this_type &binparam)
	: inherited() {
	this->variable = (TBinVariable)0;
	this->min = 0;
	this->max = 0;
	this->trueCenter = 0;
	this->operator=(binparam);
}

TBinParameters::TBinParameters(TBinVariable avariable, Float_t amin, Float_t amax)
	: inherited() {
	this->variable = avariable;
	this->min = amin;
	this->max = amax;
	this->trueCenter = 0;
}

TBinParameters::~TBinParameters() {
}

TBinParameters &TBinParameters::operator=(const this_type &binparams) {
	this->inherited::operator=(binparams);

	this->variable = binparams.variable;
	this->min = binparams.min;
	this->max = binparams.max;
	this->trueCenter = binparams.trueCenter;

	return *this;
}

Bool_t TBinParameters::operator<(const this_type &binparams) const {
	Bool_t result = true;
	if (this->variable == binparams.variable) {
		result = ((this->min + this->max) < (binparams.min + binparams.max));
	}
	return result;
}

Bool_t TBinParameters::operator==(const this_type &binparams) const {
	Bool_t result = true;

	result &= (this->variable == binparams.variable);
#ifdef USE_FLOAT_COMPARE_PRECISION
	result &= !floatCompare(this->min, binparams.min);
	result &= !floatCompare(this->max, binparams.max);
#else
	result &= (this->min == binparams.min);
	result &= (this->max == binparams.max);
#endif

	return result;
}

Bool_t TBinParameters::operator!=(const this_type &binparams) const {
	return !this->operator==(binparams);
}

void TBinParameters::Print(Option_t* option) const {
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
	cout << prefix << tab << "variable = " << this->variable << endl;
	cout << prefix << tab << "min = " << this->min << endl;
	cout << prefix << tab << "max = " << this->max << endl;
	cout << prefix << tab << "trueCenter = " << this->trueCenter << endl;
}

Float_t TBinParameters::getCenter() const {
	return (this->trueCenter != 0) ? this->trueCenter : ((this->max + this->min) / 2.0);
}
