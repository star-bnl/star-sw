#include "TCuts.h"

#include <TClass.h>
#include <TString.h>

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>
#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

using namespace std;

TCuts::cut_type TCuts::noCutsRequired = 0;

#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) \
    TCuts::cut_type CLASS##_##NAME##_CUT = 0;
#include "CutDefinitions.h"
#undef DEFINE_CUT

#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) \
    if (strcmp(curClass, #CLASS) != 0) {i = 1; curClass = #CLASS; cutIndex = 0;} \
    if (cutIndex < ((sizeof(TCuts::cut_type) * 8) - 1)) { \
	CLASS##_##NAME##_CUT = i; \
	i = i << 1; \
	cutIndex++; \
	/*cout << #CLASS << "_" << #NAME << "_CUT=" << CLASS##_##NAME##_CUT << endl;*/ \
    } else cerr << #CLASS << "_" << #NAME << "_CUT not accepted (maximum " << ((sizeof(TCuts::cut_type) * 8) - 1) << " cuts allowed)!" << endl;
void initCuts() {
    TCuts::noCutsRequired = (1 << ((sizeof(TCuts::cut_type) * 8) - 1));
    TCuts::cut_type i = 1; const Char_t *curClass = ""; UInt_t cutIndex = 0;
#include "CutDefinitions.h"
}
#undef DEFINE_CUT

#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) \
    if (strcmp(CUT_CLASS, #CLASS) == 0) { \
	if (CUT == CLASS##_##NAME##_CUT) { \
		result = #NAME; \
	} \
    }
const Char_t *getCutName(const Char_t *CUT_CLASS, TCuts::cut_type CUT) {
	const Char_t *result = 0;
#include "CutDefinitions.h"
	return result;
}
#undef DEFINE_CUT

#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) \
    if (strcmp(CUT_CLASS, #CLASS) == 0) { \
	if (CUT == CLASS##_##NAME##_CUT) { \
		result = #TITLE; \
	} \
    }
const Char_t *getCutTitle(const Char_t *CUT_CLASS, TCuts::cut_type CUT) {
	const Char_t *result = 0;
#include "CutDefinitions.h"
	return result;
}
#undef DEFINE_CUT

void printCutNames(TCuts::cut_type cuts, const Char_t *type, ostream &ostr, const Char_t *separator, Bool_t *separatorNeeded) {
    Bool_t separatorNeededTmp = false;
    if (!separatorNeeded) separatorNeeded = &separatorNeededTmp;
    if (!separator) separator = " ";
    TCuts::cut_type i_cut = 1;
    for (UInt_t i = 0;i < ((sizeof(TCuts::cut_type) * 8) - 1);++i, i_cut = i_cut << 1) {
	if ((cuts & i_cut) == i_cut) {
	    if (separatorNeeded && (*separatorNeeded)) ostr << separator;
	    ostr << getCutName(type, i_cut);
	    if (separatorNeeded) *separatorNeeded = true;
	}
    }
}

ClassImp(TCuts);

TCuts::TCuts(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->debug = 0;
	this->mParametersEvent.SetNameTitle("parametersEvent", "Parameters - event");
	this->EVENT_ALL_CUTS = 0;
	this->EVENT_ALL_CUTS_NOT = 0;
        this->numPassedEventCuts = 0;
	this->mParametersPoint.SetNameTitle("parametersPoint", "Parameters - point");
	this->POINT_ALL_CUTS = 0;
	this->POINT_ALL_CUTS_NOT = 0;
        this->numPassedPointCuts = 0;
	this->mParametersCandidate.SetNameTitle("parametersCandidate", "Parameters - candidate");
	this->CANDIDATE_ALL_CUTS = 0;
	this->CANDIDATE_ALL_CUTS_NOT = 0;
        this->numPassedCandidateCuts = 0;
	this->mParametersGamma.SetNameTitle("parametersGamma", "Parameters - gamma");
	this->GAMMA_ALL_CUTS = 0;
	this->GAMMA_ALL_CUTS_NOT = 0;
        this->numPassedGammaCuts = 0;
	this->mParametersPion.SetNameTitle("parametersPion", "Parameters - pion");
	this->PION_ALL_CUTS = 0;
	this->PION_ALL_CUTS_NOT = 0;
        this->numPassedPionCuts = 0;
}

TCuts::TCuts(const this_type &cuts) 
	: inherited() {
	this->debug = 0;
	this->mParametersEvent.SetNameTitle("parametersEvent", "Parameters - event");
	this->EVENT_ALL_CUTS = 0;
	this->EVENT_ALL_CUTS_NOT = 0;
        this->numPassedEventCuts = 0;
	this->mParametersPoint.SetNameTitle("parametersPoint", "Parameters - point");
	this->POINT_ALL_CUTS = 0;
	this->POINT_ALL_CUTS_NOT = 0;
        this->numPassedPointCuts = 0;
	this->mParametersCandidate.SetNameTitle("parametersCandidate", "Parameters - candidate");
	this->CANDIDATE_ALL_CUTS = 0;
	this->CANDIDATE_ALL_CUTS_NOT = 0;
        this->numPassedCandidateCuts = 0;
	this->mParametersGamma.SetNameTitle("parametersGamma", "Parameters - gamma");
	this->GAMMA_ALL_CUTS = 0;
	this->GAMMA_ALL_CUTS_NOT = 0;
        this->numPassedGammaCuts = 0;
	this->mParametersPion.SetNameTitle("parametersPion", "Parameters - pion");
	this->PION_ALL_CUTS = 0;
	this->PION_ALL_CUTS_NOT = 0;
        this->numPassedPionCuts = 0;
	this->operator=(cuts);
}

TCuts::~TCuts() {
}

TCuts &TCuts::operator=(const this_type &cuts) {
	if (this->debug) cout << "TCuts::operator=() started" << endl;

	this->debug = cuts.debug;

	MEMBER_SET(cuts, ParametersEvent);
	this->EVENT_ALL_CUTS = cuts.EVENT_ALL_CUTS;
	this->EVENT_ALL_CUTS_NOT = cuts.EVENT_ALL_CUTS_NOT;
        this->numPassedEventCuts = cuts.numPassedEventCuts;
        this->EVENT_passedCuts = cuts.EVENT_passedCuts;
        this->EVENT_passedCuts_all = cuts.EVENT_passedCuts_all;
        this->EVENT_passedCuts_separate = cuts.EVENT_passedCuts_separate;

	MEMBER_SET(cuts, ParametersPoint);
	this->POINT_ALL_CUTS = cuts.POINT_ALL_CUTS;
	this->POINT_ALL_CUTS_NOT = cuts.POINT_ALL_CUTS_NOT;
        this->numPassedPointCuts = cuts.numPassedPointCuts;
        this->POINT_passedCuts = cuts.POINT_passedCuts;
        this->POINT_passedCuts_all = cuts.POINT_passedCuts_all;
        this->POINT_passedCuts_separate = cuts.POINT_passedCuts_separate;

	MEMBER_SET(cuts, ParametersCandidate);
	this->CANDIDATE_ALL_CUTS = cuts.CANDIDATE_ALL_CUTS;
	this->CANDIDATE_ALL_CUTS_NOT = cuts.CANDIDATE_ALL_CUTS_NOT;
        this->numPassedCandidateCuts = cuts.numPassedCandidateCuts;
        this->CANDIDATE_passedCuts = cuts.CANDIDATE_passedCuts;
        this->CANDIDATE_passedCuts_all = cuts.CANDIDATE_passedCuts_all;
        this->CANDIDATE_passedCuts_separate = cuts.CANDIDATE_passedCuts_separate;

	MEMBER_SET(cuts, ParametersGamma);
	this->GAMMA_ALL_CUTS = cuts.GAMMA_ALL_CUTS;
	this->GAMMA_ALL_CUTS_NOT = cuts.GAMMA_ALL_CUTS_NOT;
        this->numPassedGammaCuts = cuts.numPassedGammaCuts;
        this->GAMMA_passedCuts = cuts.GAMMA_passedCuts;
        this->GAMMA_passedCuts_all = cuts.GAMMA_passedCuts_all;
        this->GAMMA_passedCuts_separate = cuts.GAMMA_passedCuts_separate;

	MEMBER_SET(cuts, ParametersPion);
	this->PION_ALL_CUTS = cuts.PION_ALL_CUTS;
	this->PION_ALL_CUTS_NOT = cuts.PION_ALL_CUTS_NOT;
        this->numPassedPionCuts = cuts.numPassedPionCuts;
        this->PION_passedCuts = cuts.PION_passedCuts;
        this->PION_passedCuts_all = cuts.PION_passedCuts_all;
        this->PION_passedCuts_separate = cuts.PION_passedCuts_separate;

	this->inherited::operator=(cuts);
	if (this->debug) cout << "TCuts::inherited::operator=() passed" << endl;

	if (this->debug) cout << "TCuts::operator=() finished" << endl;
	return *this;
}

Bool_t TCuts::operator==(const this_type &cuts) const {
	if (this->debug) cout << "TCuts::operator==() started" << endl;
	Bool_t result = true;

	result &= (this->getParametersEvent() == cuts.getParametersEvent());
	result &= (this->EVENT_ALL_CUTS == cuts.EVENT_ALL_CUTS);
	result &= (this->EVENT_ALL_CUTS_NOT == cuts.EVENT_ALL_CUTS_NOT);
	result &= (this->getParametersGamma() == cuts.getParametersGamma());
	result &= (this->POINT_ALL_CUTS == cuts.POINT_ALL_CUTS);
	result &= (this->POINT_ALL_CUTS_NOT == cuts.POINT_ALL_CUTS_NOT);
	result &= (this->getParametersCandidate() == cuts.getParametersCandidate());
	result &= (this->CANDIDATE_ALL_CUTS == cuts.CANDIDATE_ALL_CUTS);
	result &= (this->CANDIDATE_ALL_CUTS_NOT == cuts.CANDIDATE_ALL_CUTS_NOT);
	result &= (this->getParametersPion() == cuts.getParametersPion());
	result &= (this->GAMMA_ALL_CUTS == cuts.GAMMA_ALL_CUTS);
	result &= (this->GAMMA_ALL_CUTS_NOT == cuts.GAMMA_ALL_CUTS_NOT);
	result &= (this->getParametersPoint() == cuts.getParametersPoint());
	result &= (this->PION_ALL_CUTS == cuts.PION_ALL_CUTS);
	result &= (this->PION_ALL_CUTS_NOT == cuts.PION_ALL_CUTS_NOT);

	if (this->debug) cout << "TCuts::operator==() finished: " << result << endl;
	return result;
}

Bool_t TCuts::operator!=(const this_type &cuts) const {
	return !this->operator==(cuts);
}

void TCuts::Print(Option_t* option) const {
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
	cout << prefix << tab << "debug = " << this->debug << endl;

	cout << prefix << tab << "EVENT_ALL_CUTS = ";
	printCutNames(this->EVENT_ALL_CUTS, "EVENT", cout);
	cout << endl;
	cout << prefix << tab << "EVENT_ALL_CUTS_NOT = ";
	printCutNames(this->EVENT_ALL_CUTS_NOT, "EVENT", cout);
	cout << endl;
	cout << prefix << tab << "numPassedEventCuts = " << this->numPassedEventCuts << endl;
	cout << prefix << tab << "passedEventCuts:" << endl;
	for (cuts_map_type::const_iterator iter = this->EVENT_passedCuts.begin();iter != this->EVENT_passedCuts.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "EVENT", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedEventCuts_all:" << endl;
	for (cuts_map_type::const_iterator iter = this->EVENT_passedCuts_all.begin();iter != this->EVENT_passedCuts_all.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "EVENT", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedEventCuts_separate:" << endl;
	for (cuts_map_type::const_iterator iter = this->EVENT_passedCuts_separate.begin();iter != this->EVENT_passedCuts_separate.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "EVENT", cout);
	    cout << " = " << (*iter).second << endl;
	}

	cout << prefix << tab << "POINT_ALL_CUTS = ";
	printCutNames(this->POINT_ALL_CUTS, "POINT", cout);
	cout << endl;
	cout << prefix << tab << "POINT_ALL_CUTS_NOT = ";
	printCutNames(this->POINT_ALL_CUTS_NOT, "POINT", cout);
	cout << endl;
	cout << prefix << tab << "numPassedPointCuts = " << this->numPassedPointCuts << endl;
	cout << prefix << tab << "passedPointCuts:" << endl;
	for (cuts_map_type::const_iterator iter = this->POINT_passedCuts.begin();iter != this->POINT_passedCuts.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "POINT", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedPointCuts_all:" << endl;
	for (cuts_map_type::const_iterator iter = this->POINT_passedCuts_all.begin();iter != this->POINT_passedCuts_all.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "POINT", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedPointCuts_separate:" << endl;
	for (cuts_map_type::const_iterator iter = this->POINT_passedCuts_separate.begin();iter != this->POINT_passedCuts_separate.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "POINT", cout);
	    cout << " = " << (*iter).second << endl;
	}

	cout << prefix << tab << "CANDIDATE_ALL_CUTS = ";
	printCutNames(this->CANDIDATE_ALL_CUTS, "CANDIDATE", cout);
	cout << endl;
	cout << prefix << tab << "CANDIDATE_ALL_CUTS_NOT = ";
	printCutNames(this->CANDIDATE_ALL_CUTS_NOT, "CANDIDATE", cout);
	cout << endl;
	cout << prefix << tab << "numPassedCandidateCuts = " << this->numPassedCandidateCuts << endl;
	cout << prefix << tab << "passedCandidateCuts:" << endl;
	for (cuts_map_type::const_iterator iter = this->CANDIDATE_passedCuts.begin();iter != this->CANDIDATE_passedCuts.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "CANDIDATE", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedCandidateCuts_all:" << endl;
	for (cuts_map_type::const_iterator iter = this->CANDIDATE_passedCuts_all.begin();iter != this->CANDIDATE_passedCuts_all.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "CANDIDATE", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedCandidateCuts_separate:" << endl;
	for (cuts_map_type::const_iterator iter = this->CANDIDATE_passedCuts_separate.begin();iter != this->CANDIDATE_passedCuts_separate.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "CANDIDATE", cout);
	    cout << " = " << (*iter).second << endl;
	}

	cout << prefix << tab << "GAMMA_ALL_CUTS = ";
	printCutNames(this->GAMMA_ALL_CUTS, "GAMMA", cout);
	cout << endl;
	cout << prefix << tab << "GAMMA_ALL_CUTS_NOT = ";
	printCutNames(this->GAMMA_ALL_CUTS_NOT, "GAMMA", cout);
	cout << endl;
	cout << prefix << tab << "numPassedGammaCuts = " << this->numPassedGammaCuts << endl;
	cout << prefix << tab << "passedGammaCuts:" << endl;
	for (cuts_map_type::const_iterator iter = this->GAMMA_passedCuts.begin();iter != this->GAMMA_passedCuts.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "GAMMA", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedGammaCuts_all:" << endl;
	for (cuts_map_type::const_iterator iter = this->GAMMA_passedCuts_all.begin();iter != this->GAMMA_passedCuts_all.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "GAMMA", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedGammaCuts_separate:" << endl;
	for (cuts_map_type::const_iterator iter = this->GAMMA_passedCuts_separate.begin();iter != this->GAMMA_passedCuts_separate.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "GAMMA", cout);
	    cout << " = " << (*iter).second << endl;
	}

	cout << prefix << tab << "PION_ALL_CUTS = ";
	printCutNames(this->GAMMA_ALL_CUTS, "PION", cout);
	cout << endl;
	cout << prefix << tab << "PION_ALL_CUTS_NOT = ";
	printCutNames(this->GAMMA_ALL_CUTS_NOT, "PION", cout);
	for (cut_type j = 0, i = 1;j < 32;++j, i *= 2) if ((this->PION_ALL_CUTS_NOT & i) == i) cout << " " << getCutName("PION", i);
	cout << endl;
	cout << prefix << tab << "numPassedPionCuts = " << this->numPassedPionCuts << endl;
	cout << prefix << tab << "passedPionCuts:" << endl;
	for (cuts_map_type::const_iterator iter = this->PION_passedCuts.begin();iter != this->PION_passedCuts.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "PION", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedPionCuts_all:" << endl;
	for (cuts_map_type::const_iterator iter = this->PION_passedCuts_all.begin();iter != this->PION_passedCuts_all.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "PION", cout);
	    cout << " = " << (*iter).second << endl;
	}
	cout << prefix << tab << "passedPionCuts_separate:" << endl;
	for (cuts_map_type::const_iterator iter = this->PION_passedCuts_separate.begin();iter != this->PION_passedCuts_separate.end();++iter) {
	    cout << prefix << tab << tab;
	    printCutNames((*iter).first, "PION", cout);
	    cout << " = " << (*iter).second << endl;
	}

	TString newPrefix(prefix);
	newPrefix += tab;
	this->getParametersEvent().Print(newPrefix.Data());
	this->getParametersPoint().Print(newPrefix.Data());
	this->getParametersCandidate().Print(newPrefix.Data());
	this->getParametersGamma().Print(newPrefix.Data());
	this->getParametersPion().Print(newPrefix.Data());
}

#define ADD_CUTS_COUNT(TO, KEY, COUNT) \
	if ((TO).count(KEY) > 0) { \
		(TO)[KEY] += (COUNT); \
	} else { \
		(TO)[KEY] = (COUNT); \
	}

#define ADD_CUTS(TO, FROM) \
	for (cuts_map_type::const_iterator iter = (FROM).begin();iter != (FROM).end();++iter) { \
	    ADD_CUTS_COUNT((TO), (*iter).first, (*iter).second) \
	}

#define ADD_CUTS_COUNT_SEPARATE(TO, KEY, COUNT) \
	for (UInt_t ind = 0;ind < ((sizeof(TCuts::cut_type) * 8) - 1);ind++) { \
	    TCuts::cut_type i = 1 << ind; \
	    if (KEY & i) { \
		ADD_CUTS_COUNT(TO, KEY & i, COUNT) \
	    } \
	}

void TCuts::add(const this_type &cuts) const {
        if (this->debug) cout << "TCuts::add() started" << endl;

        this->numPassedEventCuts += cuts.numPassedEventCuts;
	ADD_CUTS(this->EVENT_passedCuts, cuts.EVENT_passedCuts)
	ADD_CUTS(this->EVENT_passedCuts_all, cuts.EVENT_passedCuts_all)
	ADD_CUTS(this->EVENT_passedCuts_separate, cuts.EVENT_passedCuts_separate)

        this->numPassedPointCuts += cuts.numPassedPointCuts;
	ADD_CUTS(this->POINT_passedCuts, cuts.POINT_passedCuts)
	ADD_CUTS(this->POINT_passedCuts_all, cuts.POINT_passedCuts_all)
	ADD_CUTS(this->POINT_passedCuts_separate, cuts.POINT_passedCuts_separate)

        this->numPassedCandidateCuts += cuts.numPassedCandidateCuts;
	ADD_CUTS(this->CANDIDATE_passedCuts, cuts.CANDIDATE_passedCuts)
	ADD_CUTS(this->CANDIDATE_passedCuts_all, cuts.CANDIDATE_passedCuts_all)
	ADD_CUTS(this->CANDIDATE_passedCuts_separate, cuts.CANDIDATE_passedCuts_separate)

        this->numPassedGammaCuts += cuts.numPassedGammaCuts;
	ADD_CUTS(this->GAMMA_passedCuts, cuts.GAMMA_passedCuts)
	ADD_CUTS(this->GAMMA_passedCuts_all, cuts.GAMMA_passedCuts_all)
	ADD_CUTS(this->GAMMA_passedCuts_separate, cuts.GAMMA_passedCuts_separate)

        this->numPassedPionCuts += cuts.numPassedPionCuts;
	ADD_CUTS(this->PION_passedCuts, cuts.PION_passedCuts)
	ADD_CUTS(this->PION_passedCuts_all, cuts.PION_passedCuts_all)
	ADD_CUTS(this->PION_passedCuts_separate, cuts.PION_passedCuts_separate)

        if (this->debug) cout << "TCuts::add() finished" << endl;
}

TCuts::cut_type TCuts::passEventCuts(const TMyEventData &event, TEventParameters &eventParameters) const {
	if (this->debug) cout << "TCuts::passEventCuts started" << endl;
	cut_type result = 0;
    	const parameters_type &cutParameters = this->getParametersEvent();
	getEventParams(event, cutParameters, eventParameters);
#define INCLUDE_EVENT_CUTS
#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) if (CONDITION) result |= CLASS##_##NAME##_CUT;
#include "CutDefinitions.h"
#undef DEFINE_CUT
	ADD_CUTS_COUNT(this->EVENT_passedCuts, result & (this->EVENT_ALL_CUTS | this->EVENT_ALL_CUTS_NOT), 1)
	ADD_CUTS_COUNT(this->EVENT_passedCuts_all, result, 1)
	ADD_CUTS_COUNT_SEPARATE(this->EVENT_passedCuts_separate, result, 1)
        if (((this->EVENT_ALL_CUTS == 0) || PASSED(result, this->EVENT_ALL_CUTS)) && ((this->EVENT_ALL_CUTS_NOT == 0) || NOT_PASSED(result, this->EVENT_ALL_CUTS_NOT))) {
	} else {
	    result = 0;
	}
        if (!(this->EVENT_ALL_CUTS | this->EVENT_ALL_CUTS_NOT)) result |= noCutsRequired;
	if (result) this->numPassedEventCuts++;
	return result;
}

TCuts::cut_type TCuts::passPointCuts(const TMyEventData &event, const TMyPointData &point, const TEventParameters &eventParameters, TPointParameters &pointParameters) const {
	if (this->debug) cout << "TCuts::passPointCuts started" << endl;
	cut_type result = 0;
	const parameters_type &cutParameters = this->getParametersPoint();
	getPointParams(event, point, eventParameters, cutParameters, pointParameters);
#define INCLUDE_POINT_CUTS
#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) if (CONDITION) result |= CLASS##_##NAME##_CUT;
#include "CutDefinitions.h"
#undef DEFINE_CUT
	ADD_CUTS_COUNT(this->POINT_passedCuts, result & (this->POINT_ALL_CUTS | this->POINT_ALL_CUTS_NOT), 1)
	ADD_CUTS_COUNT(this->POINT_passedCuts_all, result, 1)
	ADD_CUTS_COUNT_SEPARATE(this->POINT_passedCuts_separate, result/* & (this->POINT_ALL_CUTS | this->POINT_ALL_CUTS_NOT)*/, 1)
        if (((this->POINT_ALL_CUTS == 0) || PASSED(result, this->POINT_ALL_CUTS)) && ((this->POINT_ALL_CUTS_NOT == 0) || NOT_PASSED(result, this->POINT_ALL_CUTS_NOT))) {
	} else {
	    result = 0;
	}
        if (!(this->POINT_ALL_CUTS | this->POINT_ALL_CUTS_NOT)) result |= noCutsRequired;
	if (result) this->numPassedPointCuts++;
	return result;
}

TCuts::cut_type TCuts::passCandidateCuts(const TMyCandidateTreeData &candidate
    , const TMyEventData &event1, const TEventParameters &event1Parameters
    , const TMyPointData &point1, const TPointParameters &point1Parameters
    , const TMyEventData &event2, const TEventParameters &event2Parameters
    , const TMyPointData &point2, const TPointParameters &point2Parameters
    , TCandidateParameters &candidateParameters) const {
	if (this->debug) cout << "TCuts::passCandidateCuts started" << endl;
	cut_type result = 0;
        const parameters_type &cutParameters = this->getParametersCandidate();
	getCandidateParams(candidate, event1, event1Parameters, point1, point1Parameters, event2, event2Parameters, point2, point2Parameters, cutParameters, candidateParameters);
	Float_t massRegionLeft = 0;
	Float_t massRegionRight = 0;
#define INCLUDE_CANDIDATE_CUTS
#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) if (CONDITION) result |= CLASS##_##NAME##_CUT;
#include "CutDefinitions.h"
#undef DEFINE_CUT
	ADD_CUTS_COUNT(this->CANDIDATE_passedCuts, result & (this->CANDIDATE_ALL_CUTS | this->CANDIDATE_ALL_CUTS_NOT), 1)
	ADD_CUTS_COUNT(this->CANDIDATE_passedCuts_all, result, 1)
	ADD_CUTS_COUNT_SEPARATE(this->CANDIDATE_passedCuts_separate, result, 1)
        if (((this->CANDIDATE_ALL_CUTS == 0) || PASSED(result, this->CANDIDATE_ALL_CUTS)) && ((this->CANDIDATE_ALL_CUTS_NOT == 0) || NOT_PASSED(result, this->CANDIDATE_ALL_CUTS_NOT))) {
	} else {
	    result = 0;
	}
        if (!(this->CANDIDATE_ALL_CUTS | this->CANDIDATE_ALL_CUTS_NOT)) result |= noCutsRequired;
	if (result) this->numPassedCandidateCuts++;
	return result;
}

TCuts::cut_type TCuts::passGammaCuts(const TMyEventData &event, const TMySimulatedParticleData &gamma, const TEventParameters &eventParameters, TGammaParameters &gammaParameters) const {
	if (this->debug) cout << "TCuts::passGammaCuts started" << endl;
	cut_type result = 0;
	const parameters_type &cutParameters = this->getParametersGamma();
	getGammaParams(event, gamma, eventParameters, cutParameters, gammaParameters);
#define INCLUDE_GAMMA_CUTS
#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) if (CONDITION) result |= CLASS##_##NAME##_CUT;
#include "CutDefinitions.h"
#undef DEFINE_CUT
	ADD_CUTS_COUNT(this->GAMMA_passedCuts, result & (this->GAMMA_ALL_CUTS | this->GAMMA_ALL_CUTS_NOT), 1)
	ADD_CUTS_COUNT(this->GAMMA_passedCuts_all, result, 1)
	ADD_CUTS_COUNT_SEPARATE(this->GAMMA_passedCuts_separate, result, 1)
        if (((this->GAMMA_ALL_CUTS == 0) || PASSED(result, this->GAMMA_ALL_CUTS)) && ((this->GAMMA_ALL_CUTS_NOT == 0) || NOT_PASSED(result, this->GAMMA_ALL_CUTS_NOT))) {
	} else {
	    result = 0;
	}
        if (!(this->GAMMA_ALL_CUTS | this->GAMMA_ALL_CUTS_NOT)) result |= noCutsRequired;
	if (result) this->numPassedGammaCuts++;
	return result;
}

TCuts::cut_type TCuts::passPionCuts(const TMyEventData &event, const TMySimulatedDecayData &pion, const TEventParameters &eventParameters, const TGammaParameters &gamma1Parameters, const TGammaParameters &gamma2Parameters, TPionParameters &pionParameters) const {
	if (this->debug) cout << "TCuts::passSimuCuts started" << endl;
	cut_type result = 0;
	const parameters_type &cutParameters = this->getParametersPion();
	getPionParams(event, pion, eventParameters, gamma1Parameters, gamma2Parameters, cutParameters, pionParameters);
#define INCLUDE_PION_CUTS
#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) if (CONDITION) result |= CLASS##_##NAME##_CUT;
#include "CutDefinitions.h"
#undef DEFINE_CUT
	ADD_CUTS_COUNT(this->PION_passedCuts, result & (this->PION_ALL_CUTS | this->PION_ALL_CUTS_NOT), 1)
	ADD_CUTS_COUNT(this->PION_passedCuts_all, result, 1)
	ADD_CUTS_COUNT_SEPARATE(this->PION_passedCuts_separate, result, 1)
        if (((this->PION_ALL_CUTS == 0) || PASSED(result, this->PION_ALL_CUTS)) && ((this->PION_ALL_CUTS_NOT == 0) || NOT_PASSED(result, this->PION_ALL_CUTS_NOT))) {
	} else {
	    result = 0;
	}
        if (!(this->PION_ALL_CUTS | this->PION_ALL_CUTS_NOT)) result |= noCutsRequired;
	if (result) this->numPassedPionCuts++;
	return result;
}
