#ifndef StPi0Analysis_TCuts_H
#define StPi0Analysis_TCuts_H

#include <TNamed.h>

#include <map>
#include <iostream>
using std::map;
using std::ostream;

class TMyEventData;
class TMyPointData;
class TMyCandidateTreeData;
class TMySimulatedParticleData;
class TMySimulatedDecayData;

#include "StPi0AnalysisVersion.h"
#include "StPi0AnalysisUtil.h"
#include "TCutParameters.h"

#ifndef __CINT__
// rootcint hangs trying to process this, because the file has many lines over 256 characters
// This can be fixed by recompiling CINT with #define G__LONGBUF in $ROOTSYS/cint/inc/G__ci.h (SPECIAL CHANGES section)

#define DEFINE_CUT(CLASS, NAME, CONDITION, TITLE) extern Int_t CLASS##_##NAME##_CUT; 
#include "CutDefinitions.h"
#undef DEFINE_CUT

#endif

#define PASSED(result, CUTS) (((result) & (CUTS)) == (CUTS))
#define NOT_PASSED(result, CUTS) (((result) & (CUTS)) != (CUTS))

class TCuts : public TNamed {
	public:
		typedef TCuts this_type;
		typedef TNamed inherited;
		typedef TCutParameters parameters_type;
		typedef Int_t cut_type;
		typedef Int_t cut_number_type;
		typedef map<cut_type, cut_number_type> cuts_map_type;

		TCuts(const Char_t *name = 0, const Char_t *title = 0);
		TCuts(const this_type &cuts);
		virtual ~TCuts();
		
		this_type &operator=(const this_type &cuts);
		Bool_t operator==(const this_type &cuts) const;
		Bool_t operator!=(const this_type &cuts) const;

		virtual void Print(Option_t* option) const;

		Int_t debug;

		MEMBER_DEF(parameters_type, ParametersEvent)
		cut_type EVENT_ALL_CUTS;
		cut_type EVENT_ALL_CUTS_NOT;
		cut_type passEventCuts(const TMyEventData &event, TEventParameters &eventParameters) const;
                mutable cut_number_type numPassedEventCuts;
		mutable cuts_map_type EVENT_passedCuts;
		mutable cuts_map_type EVENT_passedCuts_all;
		mutable cuts_map_type EVENT_passedCuts_separate;

		MEMBER_DEF(parameters_type, ParametersPoint)
		cut_type POINT_ALL_CUTS;
		cut_type POINT_ALL_CUTS_NOT;
		cut_type passPointCuts(const TMyEventData &event, const TMyPointData &point, const TEventParameters &eventParameters, TPointParameters &pointParameters) const;
                mutable cut_number_type numPassedPointCuts;
		mutable cuts_map_type POINT_passedCuts;
		mutable cuts_map_type POINT_passedCuts_all;
		mutable cuts_map_type POINT_passedCuts_separate;

		MEMBER_DEF(parameters_type, ParametersCandidate)
		cut_type CANDIDATE_ALL_CUTS;
		cut_type CANDIDATE_ALL_CUTS_NOT;
		cut_type passCandidateCuts(const TMyCandidateTreeData &candidate
		    , const TMyEventData &event1, const TEventParameters &event1Parameters
		    , const TMyPointData &point1, const TPointParameters &point1Parameters
		    , const TMyEventData &event2, const TEventParameters &event2Parameters
		    , const TMyPointData &point2, const TPointParameters &point2Parameters
		    , TCandidateParameters &candidateParameters) const;
                mutable cut_number_type numPassedCandidateCuts;
		mutable cuts_map_type CANDIDATE_passedCuts;
		mutable cuts_map_type CANDIDATE_passedCuts_all;
		mutable cuts_map_type CANDIDATE_passedCuts_separate;

		MEMBER_DEF(parameters_type, ParametersGamma)
		cut_type GAMMA_ALL_CUTS;
		cut_type GAMMA_ALL_CUTS_NOT;
		cut_type passGammaCuts(const TMyEventData &event, const TMySimulatedParticleData &gamma, const TEventParameters &eventParameters, TGammaParameters &gammaParameters) const;
                mutable cut_number_type numPassedGammaCuts;
		mutable cuts_map_type GAMMA_passedCuts;
		mutable cuts_map_type GAMMA_passedCuts_all;
		mutable cuts_map_type GAMMA_passedCuts_separate;

		MEMBER_DEF(parameters_type, ParametersPion)
		cut_type PION_ALL_CUTS;
		cut_type PION_ALL_CUTS_NOT;
		cut_type passPionCuts(const TMyEventData &event, const TMySimulatedDecayData &pion, const TEventParameters &eventParameters, const TGammaParameters &gamma1Parameters, const TGammaParameters &gamma2Parameters, TPionParameters &pionParameters) const;
                mutable cut_number_type numPassedPionCuts;
		mutable cuts_map_type PION_passedCuts;
		mutable cuts_map_type PION_passedCuts_all;
		mutable cuts_map_type PION_passedCuts_separate;

                void add(const this_type &cuts) const;

		static cut_type noCutsRequired;

		ClassDef(TCuts, STPI0ANALYSIS_VERSION);
};

void initCuts();
const Char_t *getCutName(const Char_t *CUT_CLASS, TCuts::cut_type CUT);
const Char_t *getCutTitle(const Char_t *CUT_CLASS, TCuts::cut_type CUT);
void printCutNames(TCuts::cut_type cuts, const Char_t *type, ostream &ostr, const Char_t *separator = 0, Bool_t *separatorNeeded = 0);

#endif
