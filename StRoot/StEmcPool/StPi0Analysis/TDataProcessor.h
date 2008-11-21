#ifndef StPi0Analysis_TDataProcessor_H
#define StPi0Analysis_TDataProcessor_H

#include <TNamed.h>
#include <TString.h>
#include <TH1.h>
#include <TH1F.h>

#include "StPi0AnalysisVersion.h"
#include "StPi0AnalysisUtil.h"
#include "TCuts.h"
#include "TWeightCalculator.h"

class TDataProcessor : public TNamed {
protected:
		TString mTreeName;
		TString mBranchName;
		TString mHistogramName;

public:
		typedef TDataProcessor this_type;
		typedef TNamed inherited;
		typedef TCuts cuts_type;
		typedef TWeightCalculator weight_calculator_type;

		TDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		TDataProcessor(const this_type &processor);
		virtual ~TDataProcessor();

		this_type &operator=(const this_type &processor);
		virtual Bool_t operator<(const this_type &processor) const;
		virtual Bool_t operator==(const this_type &processor) const;
		virtual Bool_t operator!=(const this_type &processor) const;

		virtual void Print(Option_t* option) const;

		Int_t debug;

		MEMBER_DEF(cuts_type, Cuts)
		MEMBER_DEF(weight_calculator_type, WeightCalculator)
                HISTO_DEF(TH1F, Histogram, 0)
		TH1 *getHistogram() {return this->histHistogram;}

		const Char_t *getTreeName() const {return this->mTreeName;}
		void setTreeName(const Char_t *name) {this->mTreeName = name;}

		const Char_t *getBranchName() const {return this->mBranchName;}
		void setBranchName(const Char_t *name) {this->mBranchName = name;}

		const Char_t *getHistogramName() const {return this->mHistogramName;}
		void setHistogramName(const Char_t *name) {this->mHistogramName = name;}

		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

		virtual Bool_t add(const this_type &processor);

		Int_t numTotal;
		Int_t numPassedAllCuts;

		static Bool_t compareIgnoreWeight;
		static Bool_t compareIgnoreCuts;

		ClassDef(TDataProcessor, STPI0ANALYSIS_VERSION)
};

#endif
