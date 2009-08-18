#include "TBinStatistics.h"

#include <TMath.h>
#include <TGraphErrors.h>
#include <TClass.h>

//#include <iostream>
//using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

ClassImp(TBinStatistics);

Bool_t useBinDenomInterpolation = false;

TBinStatistics::TBinStatistics(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	this->mParameters.SetNameTitle("parameters", "Parameters");
	HISTO_INIT(Histogram)
}

TBinStatistics::TBinStatistics(const this_type &stat)
	: inherited() {
	this->mParameters.SetNameTitle("parameters", "Parameters");
	HISTO_INIT(Histogram)
	if (!this->histHistogram) this->init();
	this->operator=(stat);
}

TBinStatistics::TBinStatistics(const parameters_type &parameters, Float_t value, Float_t error)
	: inherited() {
	this->mParameters.SetNameTitle("parameters", "Parameters");
	HISTO_INIT(Histogram)
	this->setParameters(parameters);
	this->setValue(value);
	this->setError(error);
}

TBinStatistics::~TBinStatistics() {
	//cout << "~TBinStatistics " << this << " " << this->GetName() << endl;
	HISTO_DELETE(Histogram)
}

Bool_t TBinStatistics::operator<(const this_type &stat) const {
	Bool_t result = true;
	const parameters_type &binParameters = this->getParameters();
	const parameters_type &binParametersNew = stat.getParameters();
	result = (binParameters < binParametersNew);
	return result;
}

Bool_t TBinStatistics::operator==(const this_type &stat) const {
	Bool_t result = false;
	const parameters_type &binParameters = this->getParameters();
	const parameters_type &binParametersNew = stat.getParameters();
	result = (binParameters == binParametersNew);
	return result;
}

Bool_t TBinStatistics::operator!=(const this_type &stat) const {
	return !this->operator==(stat);
}

void TBinStatistics::init() {
	if (!this->histHistogram) {
		this->histHistogram = new histogram_type("histHistogram", "histHistogram", 1, 0, 1);
	}
	if (this->histHistogram) {
		this->histHistogram->Sumw2();
	}
}

Bool_t TBinStatistics::add(const this_type &stat, Bool_t check) {
	Bool_t result = false;
	if (!this->histHistogram) this->init();
	const parameters_type &binParameters = this->getParameters();
	const parameters_type &binParametersNew = stat.getParameters();
	if ((!check) || (binParameters == binParametersNew)) {
		result = true;
		HISTO_ADD(stat, histogram_type, Histogram);
	}
	return result;
}
Bool_t TBinStatistics::add(const Float_t x) {
	Bool_t result = true;
	if (!this->histHistogram) this->init();
	if (this->histHistogram) this->histHistogram->SetBinContent(1, this->histHistogram->GetBinContent(1) + x);
	return result;
}

Bool_t TBinStatistics::multiply(const this_type &stat) {
	Bool_t result = false;
	if (!this->histHistogram) this->init();
	const parameters_type &binParameters = this->getParameters();
	const parameters_type &binParametersNew = stat.getParameters();
	if (binParameters == binParametersNew) {
		result = true;
		this->histHistogram->Multiply(stat.histHistogram);
	}
	return result;
}

Bool_t TBinStatistics::divide(const this_type &stat, Float_t nom, Float_t denom, Option_t *option) {
	Bool_t result = false;
	if (!this->histHistogram) this->init();
	const parameters_type &binParameters = this->getParameters();
	const parameters_type &binParametersNew = stat.getParameters();
	if (binParameters == binParametersNew) {
		result = true;
		this->histHistogram->Divide(this->histHistogram, stat.histHistogram, nom, denom, option);
	}
	return result;
}

void TBinStatistics::scale(Float_t s) {
	if (!this->histHistogram) this->init();
	this->histHistogram->Scale(s);
}

void TBinStatistics::Print(Option_t* option) const {
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
	cout << prefix << tab << "value = " << this->getValue() << endl;
	cout << prefix << tab << "error = " << this->getError() << endl;
	TString newPrefix(prefix);
	newPrefix += tab;
	const parameters_type &parameters = this->getParameters();
	parameters.Print(newPrefix.Data());
}

TBinStatistics::this_type &TBinStatistics::operator=(const this_type &stat) {
	this->inherited::operator=(stat);

	if (!this->histHistogram) this->init();
	HISTO_SET(stat, Histogram)
	MEMBER_SET(stat, Parameters);

	return *this;
}

Float_t TBinStatistics::getValue() const {
	return this->histHistogram ? this->histHistogram->GetBinContent(1) : 0;
}

void TBinStatistics::setValue(Float_t val) {
	if (!this->histHistogram) this->init();
	if (this->histHistogram) this->histHistogram->SetBinContent(1, val);
}

Float_t TBinStatistics::getError() const {
	return this->histHistogram ? this->histHistogram->GetBinError(1) : 0;
}

void TBinStatistics::setError(Float_t err) {
	if (!this->histHistogram) this->init();
	if (this->histHistogram) this->histHistogram->SetBinError(1, err);
}

void TBinStatistics::fill(Float_t w) {
	if (!this->histHistogram) this->init();
	if (this->histHistogram) this->histHistogram->Fill(0.5, w);
}

void TBinStatistics::fill(Float_t value, TBinVariable variable, Float_t w) {
	const parameters_type &parameters = this->getParameters();
	if ((variable == parameters.variable) && (value >= parameters.min) && (value < parameters.max)) {
		this->fill(w);
	}
}

void divideBins(const bin_stat_list_type &nomList, const bin_stat_list_type &denomList
	, bin_stat_list_type &ratioList, Float_t denomI, Float_t denomIdrift
	, Bool_t normalizeByBinWidth, Bool_t binomialErrors
) {
	for (bin_stat_list_type::const_iterator iter = nomList.begin();iter != nomList.end();++iter) {
		const TBinStatistics &bin = *iter;
		Bool_t found = false;
		Float_t Y = 0, Yerr = 0;
		Float_t X = bin.getParameters().getCenter();
		Float_t denom = denomI + (denomIdrift * X);
		if (normalizeByBinWidth) denom *= bin.getParameters().getWidth();
		TBinStatistics vStat = bin;
		for (bin_stat_list_type::const_iterator iterSim = denomList.begin();(iterSim != denomList.end()) && (!found);++iterSim) {
			const TBinStatistics &binSim = *iterSim;
			if (bin == binSim) {
				vStat.divide(binSim, 1, denom, (binomialErrors ? "b" : ""));
				found = true;
			}
		}
		if (!found && useBinDenomInterpolation) {
//cout << "Denominator bin not found for:" << endl;
//bin.Print("");
			const TBinParameters &binParameters = bin.getParameters();
			Float_t diffMinLeft = -1;
			TBinStatistics binLeft;
			Float_t diffMinRight = -1;
			TBinStatistics binRight;
			for (bin_stat_list_type::const_iterator iterSim = denomList.begin();(iterSim != denomList.end()) && (!found);++iterSim) {
				const TBinStatistics &binSim = *iterSim;
				const TBinParameters &simParameters = binSim.getParameters();
				Float_t diffCenter = simParameters.getCenter() - binParameters.getCenter();
				Float_t diffWidth = simParameters.getWidth() - binParameters.getWidth();
				Float_t diff = TMath::Abs(diffCenter) + (4 * TMath::Abs(diffWidth));
				if (diffCenter < 0) {
					if ((diff < diffMinLeft) || (diffMinLeft < 0)) {
						diffMinLeft = diff;
						binLeft = binSim;
					}
				}
				if (diffCenter > 0) {
					if ((diff < diffMinRight) || (diffMinRight < 0)) {
						diffMinRight = diff;
						binRight = binSim;
					}
				}
			}
			if ((diffMinLeft >= 0) && (diffMinRight >= 0)) {
//cout << "Denominator left found:" << endl;
//binLeft.Print("");
//cout << "Denominator right found:" << endl;
//binRight.Print("");
				TBinStatistics denomBin = bin;
				Float_t newDenomValue = ((binLeft.getValue() * diffMinLeft) + (binRight.getValue() * diffMinRight)) / (diffMinLeft + diffMinRight);
				Float_t newDenomErr = ((binLeft.getError() * diffMinLeft) + (binRight.getError() * diffMinRight)) / (diffMinLeft + diffMinRight);
				denomBin.setValue(newDenomValue);
				denomBin.setError(newDenomErr);
//cout << "Denominator mean calculated:" << endl;
//denomBin.Print("");
				vStat.divide(denomBin, 1, denom, (binomialErrors ? "b" : ""));
				found = true;
			}
		}
		Y = vStat.getValue();
		Yerr = vStat.getError();
//cout << "x=" << bin.getParameters().getCenter() << ", nom=" << bin.getValue() << ", denom=" << denom << ", ratio=" << Y << endl;
		if (found) {
			TBinStatistics binRatio(bin);
			binRatio.setValue(Y);
			binRatio.setError(Yerr);
			ratioList.push_back(binRatio);
		} else {
//cout << "Denominator bin not found for:" << endl;
//bin.Print("");
		}
	}
}

void multiplyBins(const bin_stat_list_type &x, const bin_stat_list_type &y, bin_stat_list_type &z) {
	for (bin_stat_list_type::const_iterator iter = x.begin();iter != x.end();++iter) {
		const TBinStatistics &bin = *iter;
		Bool_t found = false;
		Float_t Y = 0, Yerr = 0;
		const TBinParameters par = bin.getParameters();
		TBinStatistics vStat = bin;
		for (bin_stat_list_type::const_iterator iterSim = y.begin();(iterSim != y.end()) && (!found);++iterSim) {
			const TBinStatistics &binSim = *iterSim;
			if (bin == binSim) {
				vStat.multiply(binSim);
				found = true;
			}
		}
		Y = vStat.getValue();
		Yerr = vStat.getError();
		if (found) {
			TBinStatistics binRatio(bin);
			binRatio.setValue(Y);
			binRatio.setError(Yerr);
			z.push_back(binRatio);
		}
	}
}

void divideBinsFunc(const bin_stat_list_type &nomList, TF1 *funcDenom
	, bin_stat_list_type &ratioList, Float_t denomI, Float_t denomIdrift
	, Bool_t normalizeByBinWidth, Bool_t binomialErrors, Bool_t integral) {
	bin_stat_list_type denomList;
	for (bin_stat_list_type::const_iterator iter = nomList.begin();iter != nomList.end();++iter) {
		const TBinStatistics &bin = *iter;
		TBinStatistics newBin = bin;
		if (funcDenom) {
			Float_t newValue = integral ? funcDenom->Integral(bin.getParameters().min, bin.getParameters().max) : funcDenom->Eval(bin.getParameters().getCenter());
//cout << "x=" << bin.getParameters().getCenter() << ", nom=" << bin.getValue() << ", denom=" << newValue << endl;
			newBin.setValue(newValue);
			newBin.setError(0);
		}
		denomList.push_back(newBin);
	}
	divideBins(nomList, denomList, ratioList, denomI, denomIdrift, normalizeByBinWidth, binomialErrors);
}

void resetBinsError(const bin_stat_list_type &binsList, bin_stat_list_type &binsListOut) {
	for (bin_stat_list_type::const_iterator iter = binsList.begin();iter != binsList.end();++iter) {
		const TBinStatistics &bin = *iter;
		TBinStatistics newBin = bin;
		newBin.setError(0);
		binsListOut.push_back(newBin);
	}
}

void addBins(const bin_stat_list_type &x, const bin_stat_list_type &y, bin_stat_list_type &z) {
	for (bin_stat_list_type::const_iterator iter = x.begin();iter != x.end();++iter) {
		const TBinStatistics &bin = *iter;
		Bool_t found = false;
		TBinStatistics Z = bin;
		for (bin_stat_list_type::const_iterator iterSim = y.begin();(iterSim != y.end()) && (!found);++iterSim) {
			const TBinStatistics &binSim = *iterSim;
			if (bin == binSim) {
				TBinStatistics Y = binSim;
				//Y.scale(1.0);
				Z.add(Y);
				found = true;
			}
		}
		if (found) z.push_back(Z);
	}
}

void addBins(const bin_stat_list_type &x, const Float_t y, bin_stat_list_type &z) {
	for (bin_stat_list_type::const_iterator iter = x.begin();iter != x.end();++iter) {
		const TBinStatistics &bin = *iter;
		TBinStatistics Z = bin;
		Z.add(y);
		z.push_back(Z);
	}
}

void scaleBins(const bin_stat_list_type &x, const Float_t y, bin_stat_list_type &z) {
	for (bin_stat_list_type::const_iterator iter = x.begin();iter != x.end();++iter) {
		const TBinStatistics &bin = *iter;
		TBinStatistics Z = bin;
		Z.scale(y);
		z.push_back(Z);
	}
}

void subtractBins(const bin_stat_list_type &x, const bin_stat_list_type &y, bin_stat_list_type &z) {
	for (bin_stat_list_type::const_iterator iter = x.begin();iter != x.end();++iter) {
		const TBinStatistics &bin = *iter;
		Bool_t found = false;
		TBinStatistics Z = bin;
		for (bin_stat_list_type::const_iterator iterSim = y.begin();(iterSim != y.end()) && (!found);++iterSim) {
			const TBinStatistics &binSim = *iterSim;
			if (bin == binSim) {
				TBinStatistics Y = binSim;
				Y.scale(-1.0);
				Z.add(Y);
				found = true;
			}
		}
		if (found) z.push_back(Z);
	}
}

void smoothPoints(const bin_stat_list_type &input, bin_stat_list_type &output, bin_stat_list_type &outputPoint) {
	if (input.size() == 0) return;
	Float_t *bufferX = new Float_t[input.size()];
	Float_t *bufferXerr = new Float_t[input.size()];
	Float_t *bufferY = new Float_t[input.size()];
	Float_t *bufferYerr = new Float_t[input.size()];
	if (bufferX && bufferY && bufferXerr && bufferYerr) {
		Int_t npoints = 0;
		Float_t xMax = -1000, xMin = 1000;
		for (list<TBinStatistics>::const_iterator iter = input.begin();iter != input.end();iter++) {
			const TBinStatistics &bin = *iter;
			Float_t x = bin.getParameters().getCenter();
			bufferX[npoints] = x;
			bufferY[npoints] = bin.getValue();
			bufferXerr[npoints] = bin.getParameters().getWidth() / 2.0;
			bufferYerr[npoints] = bin.getError();
			if (x > xMax) xMax = x;
			if (x < xMin) xMin = x;
			npoints++;
		}
		xMax += 0.01;
		xMin -= 0.01;
		TGraphErrors *graph = new TGraphErrors(npoints, bufferX, bufferY, bufferXerr, bufferYerr);
		Int_t polPower = npoints / 2;
		if (polPower > 4) polPower = 4;
		TString polStr = "pol"; polStr += (polPower - 1);
		TF1 *func = new TF1("func", polStr.Data());
		if (graph && func && npoints) {
			Float_t range = polPower * 4.0 * ((xMax - xMin) / npoints);
			Int_t pointInd = 0;
			for (bin_stat_list_type::const_iterator iter = input.begin();iter != input.end();iter++) {
				const TBinStatistics &bin = *iter;
				TBinStatistics binNew = bin;
				Float_t x = bin.getParameters().getCenter();
				Float_t xLow = x - (range / 2.0), xHigh = x + (range / 2.0);
				if (xHigh > xMax) {xHigh -= xHigh - xMax; xLow -= xHigh - xMax;}
				if (xLow < xMin) {xLow += xMin - xLow; xHigh += xMin - xLow;}
				func->SetRange(xLow, xHigh);
				graph->Fit(func, "RQN");
				Float_t newY = func->Eval(x);
				//Float_t newYerr = (bufferY[pointInd] != 0) ? (bufferYerr[pointInd] * newY / bufferY[pointInd]) : bufferYerr[pointInd];
				binNew.setValue(newY);
				//binNew.setError(newYerr);
				output.push_back(binNew);
				TBinParameters par = binNew.getParameters();
				par.min = x;
				par.max = x;
				binNew.setParameters(par);
				binNew.setError(0);
				outputPoint.push_back(binNew);
				pointInd++;
			}
		}
		if (graph) delete graph;
		if (func) delete func;
	}
	if (bufferX) delete [] bufferX;
	if (bufferY) delete [] bufferY;
}

void mergePoints(const bin_stat_list_type &input, bin_stat_list_type &output, bin_stat_list_type &/*outputSys*/) {
	for (bin_stat_list_type::const_iterator iterIn = input.begin();iterIn != input.end();iterIn++) {
		const TBinStatistics &binIn = *iterIn;
		Bool_t found = false;
		for (bin_stat_list_type::iterator iterOut = output.begin();iterOut != output.end();iterOut++) {
			TBinStatistics &binOut = *iterOut;
//			if (binOut.add(binIn)) {
//				found = true;
//				binOut.scale(0.5);
//			}
			if (binOut.getParameters() == binIn.getParameters()) {
				found = true;
				Float_t val1 = binIn.getValue();
				Float_t val2 = binOut.getValue();
				Float_t err1 = binIn.getError();
				Float_t err2 = binOut.getError();
				Float_t val = (val1 + val2) / 2.0;
				Float_t err = (err1 + err2) / 2.0;
				if ((err1 != 0) && (err2 != 0)) {
					Float_t w1 = 1.0 / (err1 * err1);
					Float_t w2 = 1.0 / (err2 * err2);
					Float_t w = w1 + w2;
					val = (val1 * w1 / w) + (val2 * w2 / w);
					err = 1.0 / TMath::Sqrt(w);
				}
				binOut.setValue(val);
				binOut.setError(err);
			}
		}
		if (!found) {
			output.push_back(binIn);
		}
	}
}
void mergePoints(const bin_stat_list_type &input, bin_stat_list_type &output) {
	bin_stat_list_type dummy_out;
	mergePoints(input, output, dummy_out);
}
void mergePoints(const list<bin_stat_list_type> &inputs, bin_stat_list_type &output, bin_stat_list_type &outputSys) {
	for (list<bin_stat_list_type>::const_iterator iter = inputs.begin();iter != inputs.end();iter++)
		mergePoints(*iter, output, outputSys);
}

void cropPoints(const bin_stat_list_type &input, bin_stat_list_type &output, Float_t low, Float_t high) {
    for (bin_stat_list_type::const_iterator iterIn = input.begin();iterIn != input.end();iterIn++) {
        const TBinStatistics &binIn = *iterIn;
        if ((binIn.getParameters().getCenter() >= low) && (binIn.getParameters().getCenter() < high)) {
            output.push_back(binIn);
        }
    }
}

TGraphErrors *createGraph(const bin_stat_list_type &points) {
        Float_t *x = new Float_t[points.size()];
        Float_t *xErr = new Float_t[points.size()];
        Float_t *y = new Float_t[points.size()];
        Float_t *yErr = new Float_t[points.size()];
	Int_t npoints = 0;
	for (list<TBinStatistics>::const_iterator iter = points.begin();iter != points.end();++iter) {
	    const TBinStatistics &binStat = *iter;
            Float_t xx = binStat.getParameters().getCenter();
            x[npoints] = xx;
            xErr[npoints] = 0; //binStat.getParameters().getWidth() / 2.0;
            y[npoints] = binStat.getValue();
            yErr[npoints] = binStat.getError();
            npoints++;
        }
        TGraphErrors *graph = new TGraphErrors(npoints, x, y, xErr, yErr);
	return graph;
}


bin_stat_list_type operator+(const bin_stat_list_type &a, const bin_stat_list_type &b) {
    bin_stat_list_type c;
    addBins(a, b, c);
    return c;
}
bin_stat_list_type operator+(const bin_stat_list_type &a, const Float_t b) {
    bin_stat_list_type c;
    addBins(a, b, c);
    return c;
}
bin_stat_list_type operator+(const Float_t b, const bin_stat_list_type &a) {
    bin_stat_list_type c;
    addBins(a, b, c);
    return c;
}

bin_stat_list_type operator-(const bin_stat_list_type &a, const bin_stat_list_type &b) {
    bin_stat_list_type bb;
    scaleBins(b, -1, bb);
    bin_stat_list_type c;
    addBins(a, bb, c);
    return c;
}
bin_stat_list_type operator-(const bin_stat_list_type &a, const Float_t b) {
    bin_stat_list_type c;
    addBins(a, -b, c);
    return c;
}
bin_stat_list_type operator-(const Float_t b, const bin_stat_list_type &a) {
    bin_stat_list_type c;
    addBins(a, -b, c);
    return c;
}

bin_stat_list_type operator*(const bin_stat_list_type &a, const bin_stat_list_type &b) {
    bin_stat_list_type c;
    multiplyBins(a, b, c);
    return c;
}
bin_stat_list_type operator*(const bin_stat_list_type &a, const Float_t b) {
    bin_stat_list_type c;
    scaleBins(a, b, c);
    return c;
}
bin_stat_list_type operator*(const Float_t b, const bin_stat_list_type &a) {
    bin_stat_list_type c;
    scaleBins(a, b, c);
    return c;
}

bin_stat_list_type operator/(const bin_stat_list_type &a, const bin_stat_list_type &b) {
    bin_stat_list_type c;
    divideBins(a, b, c, 1.0, 0.0, false, false);
    return c;
}
bin_stat_list_type operator/(const bin_stat_list_type &a, const Float_t b) {
    bin_stat_list_type c;
    scaleBins(a, 1.0 / b, c);
    return c;
}
bin_stat_list_type operator/(const Float_t b, const bin_stat_list_type &a) {
    bin_stat_list_type c;
    scaleBins(a, 1.0 / b, c);
    return c;
}
