#include "TWeightCalculator.h"

#include "TClass.h"
#include "TMath.h"
#include "TGraphErrors.h"

//#include <iostream>
//using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>
#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

ClassImp(TWeightCalculator);

TWeightCalculator::TWeightCalculator(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->debug = 0;
	this->mult = 1.0;
	this->multDrift = 0.0;
	this->pt0 = 0.0;
	this->power = 0.0;
	this->histogram = 0;
	this->rangeLow = 0.0;
	this->rangeHigh = 1000.0;
}

TWeightCalculator::TWeightCalculator(const this_type &w)
	: inherited() {
	this->debug = 0;
	this->mult = 1.0;
	this->multDrift = 0.0;
	this->pt0 = 0.0;
	this->power = 0.0;
	this->histogram = 0;
	this->rangeLow = 0.0;
	this->rangeHigh = 1000.0;
	this->operator=(w);
}

TWeightCalculator::~TWeightCalculator() {
}

TWeightCalculator::this_type &TWeightCalculator::operator=(const this_type &w) {
	if (this->debug) cout << "TWeightCalculator::operator= started" << endl;
	this->inherited::operator=(w);

	this->debug = w.debug;
	this->mult = w.mult;
	this->multDrift = w.multDrift;
	this->pt0 = w.pt0;
	this->power = w.power;
	this->histogram = w.histogram;
	this->rangeLow = w.rangeLow;
	this->rangeHigh = w.rangeHigh;

	if (this->debug) cout << "TWeightCalculator::operator= finished" << endl;
	return *this;
}

Bool_t TWeightCalculator::operator==(const this_type &w) const {
	if (this->debug) cout << "TWeightCalculator::operator== started" << endl;
	Bool_t result = true;

#ifdef USE_FLOAT_COMPARE_PRECISION 
	if (result) result &= !floatCompare(this->mult, w.mult);
	if (this->debug) cout << "mult: " << result << endl;
	if (result) result &= !floatCompare(this->multDrift, w.multDrift);
	if (this->debug) cout << "multDrift: " << result << endl;
	if (result) result &= !floatCompare(this->pt0, w.pt0);
	if (this->debug) cout << "pt0: " << result << endl;
	if (result) result &= !floatCompare(this->power, w.power);
	if (this->debug) cout << "power: " << result << endl;
	if (result) result &= !floatCompare(this->rangeLow, w.rangeLow);
	if (this->debug) cout << "rangeLow: " << result << endl;
	if (result) result &= !floatCompare(this->rangeHigh, w.rangeHigh);
	if (this->debug) cout << "rangeHigh: " << result << endl;
#else
	if (result) result &= (this->mult == w.mult);
	if (this->debug) cout << "mult: " << result << endl;
	if (result) result &= (this->multDrift == w.multDrift);
	if (this->debug) cout << "multDrift: " << result << endl;
	if (result) result &= (this->pt0 == w.pt0);
	if (this->debug) cout << "pt0: " << result << endl;
	if (result) result &= (this->power == w.power);
	if (this->debug) cout << "power: " << result << endl;
	if (result) result &= (this->rangeLow == w.rangeLow);
	if (this->debug) cout << "rangeLow: " << result << endl;
	if (result) result &= (this->rangeHigh == w.rangeHigh);
	if (this->debug) cout << "rangeHigh: " << result << endl;
#endif
	if (result) {
	    if (this->histogram && w.histogram) {
    		UInt_t nbins1 = this->histogram->GetXaxis() ? this->histogram->GetXaxis()->GetNbins() : 0;
    		UInt_t nbins2 = w.histogram->GetXaxis() ? w.histogram->GetXaxis()->GetNbins() : 0;
		result &= (nbins1 == nbins2);
		for (UInt_t i = 1;result && (i <= nbins1);++i) {
#ifdef USE_FLOAT_COMPARE_PRECISION 
		    result &= !floatCompare(this->histogram->GetBinContent(i), w.histogram->GetBinContent(i));
#else
		    result &= (this->histogram->GetBinContent(i) == w.histogram->GetBinContent(i));
#endif
		}
	    } else {
		result &= (this->histogram == w.histogram);
	    }
	}
	if (this->debug) cout << "histogram: " << result << endl;

	if (this->debug) cout << "TWeightCalculator::operator== finished: " << result << endl;
	return result;
}

Bool_t TWeightCalculator::operator!=(const this_type &w) const {
	return !this->operator==(w);
}

void TWeightCalculator::Print(Option_t* option) const {
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
	cout << prefix << tab << "debug = " << this->debug << endl;
	cout << prefix << tab << "mult = " << this->mult << endl;
	cout << prefix << tab << "multDrift = " << this->multDrift << endl;
	cout << prefix << tab << "pt0 = " << this->pt0 << endl;
	cout << prefix << tab << "power = " << this->power << endl;
	cout << prefix << tab << "histogram = " << this->histogram << endl;
	cout << prefix << tab << "rangeLow = " << this->rangeLow << endl;
	cout << prefix << tab << "rangeHigh = " << this->rangeHigh << endl;
}

Double_t getWeightFunc(Double_t *x, Double_t *p) {
	Double_t w = (p[0] + (p[1] * x[0])) * TMath::Power(1 + (x[0]/p[2]),-p[3]);
	return w;
}

Double_t getWeightFuncDraw(Double_t *x, Double_t *p) {
	Float_t w = (x && x[0]) ? (getWeightFunc(x, p) / x[0]) : 0;
	return w;
}

Float_t TWeightCalculator::getWeight(Float_t value) const {
	Float_t w = 0;
	if ((value >= this->rangeLow) && (value < this->rangeHigh)) {
		if (this->histogram) {
			Int_t bin1 = this->histogram->GetXaxis()->FindBin(value);
			Float_t bin1center = this->histogram->GetXaxis()->GetBinCenter(bin1);
			Int_t bin2 = bin1;
			Float_t bin2center = bin1center;
			Bool_t doAverage = false;
			if (value < bin1center) {
				if (bin1 > 1) {
					bin2 = bin1 - 1;
					doAverage = true;
				}
			} else if (value > bin1center) {
				if (bin1 < this->histogram->GetXaxis()->GetNbins()) {
					bin2 = bin1 + 1;
					doAverage = true;
				}
			}
			if (doAverage) {
				bin2center = this->histogram->GetXaxis()->GetBinCenter(bin2);
				Float_t w1 = this->histogram->GetBinContent(bin1);
				Float_t w2 = this->histogram->GetBinContent(bin2);
				Float_t dist1 = TMath::Abs(bin1center - value);
				Float_t dist2 = TMath::Abs(bin2center - value);
				w = ((w1 * dist1) + (w2 * dist2)) / (dist1 + dist2);
			} else {
				w = this->histogram->GetBinContent(bin1);
			}
			w *= (this->mult * 1) + (this->multDrift * value);
		} else {
			Double_t p[4];
			p[0] = this->mult;
			p[1] = this->multDrift;
			p[2] = this->pt0;
			p[3] = this->power;
			Double_t x[1];
			x[0] = value;
			w = getWeightFunc(x, p);
		}
	}
	if (this->debug) cout << "TWeightCalculator::getWeight(" << value << ") = " << w << endl;
	return w;
}

TF1 *TWeightCalculator::createFunc(Bool_t forDrawing) const {
	TF1 *func = new TF1("funcLeft", forDrawing ? &getWeightFuncDraw : &getWeightFunc, this->rangeLow, this->rangeHigh, 4);
	if (func) {
		Double_t p[4];
		p[0] = this->mult;
		p[1] = this->multDrift;
		p[2] = this->pt0;
		p[3] = this->power;
		func->SetParameters(p);
	}
	return func;
}

void TWeightCalculator::Fit(const bin_list_type &points) {
	if (points.size() == 0) return;
	Float_t *x = new Float_t[points.size()];
	Float_t *xErr = new Float_t[points.size()];
	Float_t *y = new Float_t[points.size()];
	Float_t *yErr = new Float_t[points.size()];
	Int_t npoints = 0;
	Float_t xMax = -1000, xMin = 1000;
	for (list<TBinStatistics>::const_iterator iter = points.begin();iter != points.end();++iter) {
		const TBinStatistics &binStat = *iter;
		Float_t xx = binStat.getParameters().getCenter();
		x[npoints] = xx;
		xErr[npoints] = 0; //binStat.getParameters().getWidth() / 2.0;
		y[npoints] = binStat.getValue();
		yErr[npoints] = binStat.getError();
		npoints++;
		if (xx > xMax) xMax = xx;
		if (xx < xMin) xMin = xx;
	}
	this->rangeLow = xMin;
	this->rangeHigh = xMax;
	TGraphErrors *graph = new TGraphErrors(npoints, x, y, xErr, yErr);
	if (graph) {
		Float_t chi2min = -1;
		//Float_t switchMin = xMin;
		Double_t par[] = {0, 0, 0, 0, 0};
		Float_t chi2 = 0;
		Float_t ndf = 0;
		TF1 *func = this->createFunc(false);
		if (func) {
			func->SetParameter(0, y[0]);
			func->FixParameter(1, 0.0);
			func->SetParameter(2, 1.0);
			func->SetParameter(3, 9.0);
			graph->Fit(func, "RQN");
			graph->Fit(func, "RQN");
			graph->Fit(func, "RQN");
			chi2 += func->GetChisquare();
			ndf += func->GetNDF();
		}
		chi2min = chi2;
		if (func) func->GetParameters(par);
		if (func) delete func;
		this->mult = 0.0;//par[0];
		this->multDrift = par[0];
		this->pt0 = par[2];
		this->power = par[3];
	}
	if (graph) delete graph;
	if (x) delete [] x;
	if (y) delete [] y;
	if (xErr) delete [] xErr;
	if (yErr) delete [] yErr;
}

void TWeightCalculator::Fit(const TH1 *hist) {
	if (!hist) return;
	bin_list_type bins;
	for (Int_t ibin = hist->GetXaxis()->GetFirst();ibin <= hist->GetXaxis()->GetLast();ibin++) {
		bin_type bin;
		TBinParameters param = bin.getParameters();
		param.min = hist->GetBinLowEdge(ibin);
		param.max = hist->GetBinLowEdge(ibin) + hist->GetBinWidth(ibin);
		bin.setParameters(param);
		bin.setValue(hist->GetBinContent(ibin));
		bin.setError(hist->GetBinError(ibin));
//bin.Print("");
		bins.push_back(bin);
	}
	this->Fit(bins);
}

void TWeightCalculator::DrawCopy(Option_t* option) const {
	if (this->histogram) {
		this->histogram->DrawCopy(option);
	} else {
		TF1 *func = this->createFunc(true);
		if (func) {
			func->Draw(option);
		} else {
			cout << "No func!!!" << endl;
		}
	}
}
