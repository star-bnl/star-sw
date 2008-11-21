#ifndef StPi0Analysis_TBinParameters_H
#define StPi0Analysis_TBinParameters_H

#include <TNamed.h>

#include "StPi0AnalysisVersion.h"
#include "StPi0AnalysisUtil.h"

class TBinParameters : public TNamed {
	public:
		typedef TBinParameters this_type;
		typedef TNamed inherited;

		TBinParameters(const Char_t *name = 0, const Char_t *title = 0);
		TBinParameters(const this_type &binparam);
		TBinParameters(TBinVariable avariable, Float_t amin, Float_t amax);
		virtual ~TBinParameters();

		this_type &operator=(const this_type &binparams);

		Bool_t operator<(const this_type &binparams) const;
		Bool_t operator==(const this_type &binparams) const;
		Bool_t operator!=(const this_type &binparams) const;

		TBinVariable variable;
		Float_t min;
		Float_t max;
		Float_t trueCenter;

		Float_t getCenter() const;
		Float_t getWidth() const {return (max - min);}

		virtual void Print(Option_t* option) const;

		ClassDef(TBinParameters, STPI0ANALYSIS_VERSION);
};

#endif
