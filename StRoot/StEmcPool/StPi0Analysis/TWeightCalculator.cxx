#include "TWeightCalculator.h"

#include <TROOT.h>
#include <TClass.h>
#include <TMath.h>
#include <TGraphErrors.h>

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
	this->exponent = 0.0;
	this->p0_1 = 0.0;
	this->p1 = 0.0;
	this->p2 = 0.0;
	this->p3 = 0.0;
	this->p4 = 0.0;
	this->p5 = 0.0;
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
	this->exponent = 0.0;
	this->p0_1 = 0.0;
	this->p1 = 0.0;
	this->p2 = 0.0;
	this->p3 = 0.0;
	this->p4 = 0.0;
	this->p5 = 0.0;
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
	this->exponent = w.exponent;
	this->p0_1 = w.p0_1;
	this->p1 = w.p1;
	this->p2 = w.p2;
	this->p3 = w.p3;
	this->p4 = w.p4;
	this->p5 = w.p5;
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
	if (result) result &= !floatCompare(this->exponent, w.exponent);
	if (this->debug) cout << "exponent: " << result << endl;
	if (result) result &= !floatCompare(this->p0_1, w.p0_1);
	if (this->debug) cout << "p0_1: " << result << endl;
	if (result) result &= !floatCompare(this->p1, w.p1);
	if (this->debug) cout << "p1: " << result << endl;
	if (result) result &= !floatCompare(this->p2, w.p2);
	if (this->debug) cout << "p2: " << result << endl;
	if (result) result &= !floatCompare(this->p3, w.p3);
	if (this->debug) cout << "p3: " << result << endl;
	if (result) result &= !floatCompare(this->p4, w.p4);
	if (this->debug) cout << "p4: " << result << endl;
	if (result) result &= !floatCompare(this->p5, w.p5);
	if (this->debug) cout << "p5: " << result << endl;
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
	if (result) result &= (this->exponent == w.exponent);
	if (this->debug) cout << "exponent: " << result << endl;
	if (result) result &= (this->p0_1 == w.p0_1);
	if (this->debug) cout << "p0_1: " << result << endl;
	if (result) result &= (this->p1 == w.p1);
	if (this->debug) cout << "p1: " << result << endl;
	if (result) result &= (this->p2 == w.p2);
	if (this->debug) cout << "p2: " << result << endl;
	if (result) result &= (this->p3 == w.p3);
	if (this->debug) cout << "p3: " << result << endl;
	if (result) result &= (this->p4 == w.p4);
	if (this->debug) cout << "p4: " << result << endl;
	if (result) result &= (this->p5 == w.p5);
	if (this->debug) cout << "p5: " << result << endl;
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
	cout << prefix << tab << "exponent = " << this->exponent << endl;
	cout << prefix << tab << "p0_1 = " << this->p0_1 << endl;
	cout << prefix << tab << "p1 = " << this->p1 << endl;
	cout << prefix << tab << "p2 = " << this->p2 << endl;
	cout << prefix << tab << "p3 = " << this->p3 << endl;
	cout << prefix << tab << "p4 = " << this->p4 << endl;
	cout << prefix << tab << "p5 = " << this->p5 << endl;
	cout << prefix << tab << "histogram = " << this->histogram << endl;
	cout << prefix << tab << "rangeLow = " << this->rangeLow << endl;
	cout << prefix << tab << "rangeHigh = " << this->rangeHigh << endl;
}

Double_t getWeightFunc(Double_t *x, Double_t *p) {
	Double_t w = (p[0] + (p[1] * x[0])) * TMath::Power(1 + (x[0]/p[2]),-p[3]) * TMath::Exp(-x[0]*p[10]) * (1.0 + p[4] + (x[0]*p[5]) + (x[0]*x[0]*p[6]) + (x[0]*x[0]*x[0]*p[7]) + (x[0]*x[0]*x[0]*x[0]*p[8]) + (x[0]*x[0]*x[0]*x[0]*x[0]*p[9]));
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
			Double_t p[11];
			p[0] = this->mult;
			p[1] = this->multDrift;
			p[2] = this->pt0;
			p[3] = this->power;
			p[4] = this->p0_1;
			p[5] = this->p1;
			p[6] = this->p2;
			p[7] = this->p3;
			p[8] = this->p4;
			p[9] = this->p5;
			p[10] = this->exponent;
			Double_t x[1];
			x[0] = value;
			w = getWeightFunc(x, p);
		}
	}
	if (this->debug) cout << "TWeightCalculator::getWeight(" << value << ") = " << w << endl;
	return w;
}

TF1 *TWeightCalculator::createFunc(Bool_t forDrawing) const {
        TString newName;
        {
            UInt_t i = 0;
            Bool_t goodNewName = false;
            TCollection *listF = gROOT->GetListOfFunctions();
            do {
                newName = this->GetName(); newName += "_func"; if (i) {newName += "_"; newName += i;}
                if (listF && listF->Contains(newName.Data())) i++; else goodNewName = true;
            } while (!goodNewName);
        }
	TString funcStr = "([0] + ([1] * x)) * TMath::Power(1 + (x/[2]),-[3]) * TMath::Exp(-x*[10]) * (1.0 + [4] + (x*[5]) + (x*x*[6]) + (x*x*x*[7]) + (x*x*x*x*[8]) + (x*x*x*x*x*[9]))";
	TString funcDrawStr = funcStr + " / x";
	TF1 *func = new TF1(newName.Data(), forDrawing ? funcDrawStr.Data() : funcStr.Data(), this->rangeLow, this->rangeHigh);
	if (func) {
		Double_t p[11];
		p[0] = this->mult;
		p[1] = this->multDrift;
		p[2] = this->pt0;
		p[3] = this->power;
		p[4] = this->p0_1;
		p[5] = this->p1;
		p[6] = this->p2;
		p[7] = this->p3;
		p[8] = this->p4;
		p[9] = this->p5;
		p[10] = this->exponent;
		func->SetParameters(p);
	}
	return func;
}

void TWeightCalculator::Fit(const bin_list_type &points, Option_t *option, Option_t *optionWeight, Float_t pTLow, Float_t pTHigh, TF1 *extFunc) {
	if (points.size() == 0) return;
	if (!option) option = "RQN";
        TString optionWeightStr = optionWeight;
	if (optionWeightStr == "") optionWeightStr = "POWERLAW POLYNOMIAL POL5";
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
		Float_t chi2 = 0;
		Float_t ndf = 0;
		TF1 *func = extFunc;
		if (!func) func = this->createFunc(false);
		if (func && (func != extFunc)) {
                    func->SetParameter(0, y[0]);
                    func->SetParameter(1, 0.0);
                    func->SetParameter(2, 1.0);
                    func->SetParameter(3, 0.0);
                    func->SetParameter(4, 0.0);
                    func->SetParameter(5, 0.0);
                    func->SetParameter(6, 0.0);
                    func->SetParameter(7, 0.0);
                    func->SetParameter(8, 0.0);
                    func->SetParameter(9, 0.0);
                    func->SetParameter(10, 0.0);
                    if (optionWeightStr.Contains("POWERLAW")) {
                        func->SetParameter(0, y[0]);
                        func->FixParameter(1, func->GetParameter(1));
                        func->SetParameter(2, 1.0);
                        func->SetParLimits(2, 0.0, 3.0);
                        func->SetParameter(3, 9.0);
                        func->SetParLimits(3, 5.0, 15.0);
                        func->FixParameter(4, func->GetParameter(4));
                        func->FixParameter(5, func->GetParameter(5));
                        func->FixParameter(6, func->GetParameter(6));
                        func->FixParameter(7, func->GetParameter(7));
                        func->FixParameter(8, func->GetParameter(8));
                        func->FixParameter(9, func->GetParameter(9));
                        func->FixParameter(10, func->GetParameter(10));
                    }
                    if (optionWeightStr.Contains("EXPONENT")) {
                        func->SetParameter(0, y[0]);
                        func->FixParameter(1, func->GetParameter(1));
                        func->FixParameter(2, func->GetParameter(2));
                        func->FixParameter(3, func->GetParameter(3));
                        func->FixParameter(4, func->GetParameter(4));
                        func->FixParameter(5, func->GetParameter(5));
                        func->FixParameter(6, func->GetParameter(6));
                        func->FixParameter(7, func->GetParameter(7));
                        func->FixParameter(8, func->GetParameter(8));
                        func->FixParameter(9, func->GetParameter(9));
                        func->SetParameter(10, 1.0);
                        func->SetParLimits(10, 0.0, 5.0);
                    }
                    if (optionWeightStr.Contains("POLYNOMIAL")) {
                        func->FixParameter(0, func->GetParameter(0));
                        func->FixParameter(1, func->GetParameter(1));
                        func->FixParameter(2, func->GetParameter(2));
                        func->FixParameter(3, func->GetParameter(3));
                        if (optionWeightStr.Contains("POL0") || optionWeightStr.Contains("POL1") || optionWeightStr.Contains("POL2") || optionWeightStr.Contains("POL3") || optionWeightStr.Contains("POL4") || optionWeightStr.Contains("POL5")) {
                            func->SetParameter(4, func->GetParameter(4));
                            func->SetParLimits(4, -10, +10);
                        } else {
                            func->FixParameter(4, 0.0);
                        }
                        if (optionWeightStr.Contains("POL1") || optionWeightStr.Contains("POL2") || optionWeightStr.Contains("POL3") || optionWeightStr.Contains("POL4") || optionWeightStr.Contains("POL5")) {
                            func->SetParameter(5, func->GetParameter(5));
                            func->SetParLimits(5, -10, +10);
                        } else {
                            func->FixParameter(5, 0.0);
                        }
                        if (optionWeightStr.Contains("POL2") || optionWeightStr.Contains("POL3") || optionWeightStr.Contains("POL4") || optionWeightStr.Contains("POL5")) {
                            func->SetParameter(6, func->GetParameter(6));
                            func->SetParLimits(6, -10, +10);
                        } else {
                            func->FixParameter(6, 0.0);
                        }
                        if (optionWeightStr.Contains("POL3") || optionWeightStr.Contains("POL4") || optionWeightStr.Contains("POL5")) {
                            func->SetParameter(7, func->GetParameter(7));
                            func->SetParLimits(7, -1, +1);
                        } else {
                            func->FixParameter(7, 0.0);
                        }
                        if (optionWeightStr.Contains("POL4") || optionWeightStr.Contains("POL5")) {
                            func->SetParameter(8, func->GetParameter(8));
                            func->SetParLimits(8, -1, +1);
                        } else {
                            func->FixParameter(8, 0.0);
                        }
                        if (optionWeightStr.Contains("POL5")) {
                            func->SetParameter(9, func->GetParameter(9));
                            func->SetParLimits(9, -0.001, +0.001);
                        } else {
                            func->FixParameter(9, 0.0);
                        }
                        func->FixParameter(10, func->GetParameter(10));
                    }
		}
		if (func) {
                    graph->Fit(func, option, "", pTLow, pTHigh);
		    chi2 += func->GetChisquare();
		    ndf += func->GetNDF();
		}
		chi2min = chi2;
		Double_t par[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
		if (func) func->GetParameters(par);
		if (func && (func != extFunc)) delete func;
		this->mult = 0.0;//par[0];
		this->multDrift = par[0];
		this->pt0 = par[2];
		this->power = par[3];
		this->p0_1 = par[4];
		this->p1 = par[5];
		this->p2 = par[6];
		this->p3 = par[7];
		this->p4 = par[8];
		this->p5 = par[9];
		this->exponent = par[10];
	}
	if (graph) delete graph;
	if (x) delete [] x;
	if (y) delete [] y;
	if (xErr) delete [] xErr;
	if (yErr) delete [] yErr;
}

void TWeightCalculator::Fit(const TH1 *hist, Option_t *option, Option_t *optionWeight, Float_t pTLow, Float_t pTHigh) {
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
	this->Fit(bins, option, optionWeight, pTLow, pTHigh);
}

void TWeightCalculator::Fit(const TGraphErrors *graph, Option_t *option, Option_t *optionWeight, Float_t pTLow, Float_t pTHigh) {
	if (!graph) return;
	bin_list_type bins;
	for (Int_t ibin = 0;ibin < graph->GetN();ibin++) {
		bin_type bin;
		TBinParameters param = bin.getParameters();
                Double_t x,y;
                graph->GetPoint(ibin, x, y);
		param.min = x - graph->GetErrorX(ibin);
		param.max = x + graph->GetErrorX(ibin);
		bin.setParameters(param);
		bin.setValue(y);
		bin.setError(graph->GetErrorY(ibin));
//bin.Print("");
		bins.push_back(bin);
	}
	this->Fit(bins, option, optionWeight, pTLow, pTHigh);
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
