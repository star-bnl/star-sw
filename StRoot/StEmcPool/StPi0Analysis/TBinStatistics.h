#ifndef StPi0Analysis_TBinStatistics_H
#define StPi0Analysis_TBinStatistics_H

#include <TNamed.h>
#include <TH1F.h>
#include <TF1.h>
#include <TGraphErrors.h>

#include <list>
using namespace std;

#include "StPi0AnalysisVersion.h"
#include "StPi0AnalysisUtil.h"
#include "TBinParameters.h"

class TBinStatistics : public TNamed {
public:
		typedef TBinStatistics this_type;
		typedef TNamed inherited;
		typedef TH1F histogram_type;
		typedef TBinParameters parameters_type;

		TBinStatistics(const Char_t *name = 0, const Char_t *title = 0);
		TBinStatistics(const this_type &stat);
		TBinStatistics(const parameters_type &parameters, Float_t value = 0, Float_t error = 0);
		virtual ~TBinStatistics();

		void init();

		virtual void Print(Option_t* option) const;

		this_type &operator=(const this_type &stat);

		Bool_t operator<(const this_type &stat) const;
		Bool_t operator==(const this_type &stat) const;
		Bool_t operator!=(const this_type &stat) const;

		Bool_t add(const this_type &stat, Bool_t check = true);
		Bool_t add(const Float_t x);
		
		Bool_t multiply(const this_type &stat);
		Bool_t divide(const this_type &stat, Float_t nom, Float_t denom, Option_t *option);

		void scale(Float_t s);

		MEMBER_DEF(parameters_type, Parameters)
		HISTO_DEF(histogram_type, Histogram, 0)

		Float_t getValue() const;
		void setValue(Float_t val);

		Float_t getError() const;
		void setError(Float_t err);
		
		void fill(Float_t w);
		void fill(Float_t value, TBinVariable variable, Float_t w);

		ClassDef(TBinStatistics, STPI0ANALYSIS_VERSION);
};

typedef list<TBinStatistics> bin_stat_list_type;

extern Bool_t useBinDenomInterpolation;
void divideBins(const bin_stat_list_type &nomList, const bin_stat_list_type &denomList
	, bin_stat_list_type &ratioList, Float_t denomI, Float_t denomIdrift
	, Bool_t normalizeByBinWidth, Bool_t binomialErrors);
void divideBinsFunc(const bin_stat_list_type &nomList, TF1 *funcDenom
	, bin_stat_list_type &ratioList, Float_t denomI, Float_t denomIdrift
	, Bool_t normalizeByBinWidth, Bool_t binomialErrors, Bool_t integral);

void multiplyBins(const bin_stat_list_type &x, const bin_stat_list_type &y, bin_stat_list_type &z);

void addBins(const bin_stat_list_type &x, const bin_stat_list_type &y, bin_stat_list_type &z);
void addBins(const bin_stat_list_type &x, const Float_t y, bin_stat_list_type &z);
void subtractBins(const bin_stat_list_type &x, const bin_stat_list_type &y, bin_stat_list_type &z);

void scaleBins(const bin_stat_list_type &x, const Float_t y, bin_stat_list_type &z);

void resetBinsError(const bin_stat_list_type &binsList, bin_stat_list_type &binsListOut);

void smoothPoints(const bin_stat_list_type &input, bin_stat_list_type &output, bin_stat_list_type &outputPoint);

void mergePoints(const bin_stat_list_type &input, bin_stat_list_type &output, bin_stat_list_type &outputSys);
void mergePoints(const bin_stat_list_type &input, bin_stat_list_type &output);
void mergePoints(const list<bin_stat_list_type> &inputs, bin_stat_list_type &output, bin_stat_list_type &outputSys);

void cropPoints(const bin_stat_list_type &input, bin_stat_list_type &output, Float_t low, Float_t high);

TGraphErrors *createGraph(const bin_stat_list_type &points);

bin_stat_list_type operator+(const bin_stat_list_type &a, const bin_stat_list_type &b);
bin_stat_list_type operator+(const bin_stat_list_type &a, const Float_t b);
bin_stat_list_type operator+(const Float_t b, const bin_stat_list_type &a);

bin_stat_list_type operator-(const bin_stat_list_type &a, const bin_stat_list_type &b);
bin_stat_list_type operator-(const bin_stat_list_type &a, const Float_t b);
bin_stat_list_type operator-(const Float_t b, const bin_stat_list_type &a);

bin_stat_list_type operator*(const bin_stat_list_type &a, const bin_stat_list_type &b);
bin_stat_list_type operator*(const bin_stat_list_type &a, const Float_t b);
bin_stat_list_type operator*(const Float_t b, const bin_stat_list_type &a);

bin_stat_list_type operator/(const bin_stat_list_type &a, const bin_stat_list_type &b);
bin_stat_list_type operator/(const bin_stat_list_type &a, const Float_t b);
bin_stat_list_type operator/(const Float_t b, const bin_stat_list_type &a);

#endif
