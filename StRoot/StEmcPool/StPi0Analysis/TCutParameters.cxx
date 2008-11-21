#include "TCutParameters.h"
#include "TWeightCalculator.h"

#include <TClass.h>

#include <StEmcPool/StPi0Common/Logger.h>
#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

ClassImp(TCutParameters);

TCutParameters::TCutParameters(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
#define DEFINE_CUT_PARAMETERS
#define DEFINE_CUT_PARAMETER(TYPE, NAME) this->NAME = (TYPE)(0);
#include "TCutParameters.h"
#undef DEFINE_CUT_PARAMETER
}

TCutParameters::TCutParameters(const this_type &parameters)
	: inherited() {
#define DEFINE_CUT_PARAMETERS
#define DEFINE_CUT_PARAMETER(TYPE, NAME) this->NAME = (TYPE)(0);
#include "TCutParameters.h"
#undef DEFINE_CUT_PARAMETER
	this->operator=(parameters);
}

TCutParameters::~TCutParameters() {
}

TCutParameters &TCutParameters::operator=(const this_type &parameters) {
	this->inherited::operator=(parameters);
#define DEFINE_CUT_PARAMETERS
#define DEFINE_CUT_PARAMETER(TYPE, NAME) this->NAME = parameters.NAME;
#include "TCutParameters.h"
#undef DEFINE_CUT_PARAMETER
	return *this;
}

Bool_t isEqual(const Float_t &op1, const Float_t &op2) {
#ifdef USE_FLOAT_COMPARE_PRECISION 
    return !floatCompare(op1, op2);
#else
    return op1 == op2;
#endif
}

Bool_t isEqual(const TString &op1, const TString &op2) {
    return op1 == op2;
}

Bool_t isEqual(const Int_t &op1, const Int_t &op2) {
    return op1 == op2;
}

Bool_t isEqual(const ULong_t &op1, const ULong_t &op2) {
    return op1 == op2;
}

Bool_t isEqual(const Char_t *op1, const Char_t *op2) {
    return (op1 && op2) ? (strcmp(op1, op2) == 0) : (op1 == op2);
}

Bool_t isEqual(const TWeightCalculator *op1, const TWeightCalculator *op2) {
    Bool_t result = (op1 && op2) ? ((*op1) == (*op2)) : (op1 == op2);
    return result;
}

Bool_t isEqual(const list<pair<Int_t, Int_t> > *op1, const list<pair<Int_t, Int_t> > *op2) {
    Bool_t result = (op1 && op2) ? ((*op1) == (*op2)) : (op1 == op2);
    return result;
}

Bool_t TCutParameters::operator==(const this_type &parameters) const {
	Bool_t result = true;
#define DEFINE_CUT_PARAMETERS
#define DEFINE_CUT_PARAMETER(TYPE, NAME) result &= isEqual(this->NAME, parameters.NAME);
#include "TCutParameters.h"
#undef DEFINE_CUT_PARAMETER
	return result;
}

Bool_t TCutParameters::operator!=(const this_type &parameters) const {
	return !this->operator==(parameters);
}

void TCutParameters::Print(Option_t* option) const {
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
#define DEFINE_CUT_PARAMETERS
#define DEFINE_CUT_PARAMETER(TYPE, NAME) cout << prefix << tab << #NAME " = " << this->NAME << endl;
#include "TCutParameters.h"
#undef DEFINE_CUT_PARAMETER
}
