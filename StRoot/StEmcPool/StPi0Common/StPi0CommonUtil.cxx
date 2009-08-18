#include "StPi0CommonUtil.h"

#include <TROOT.h>
#include <TSystem.h>
#include <TMath.h>
#include <TH1.h>
#include <TLegend.h>
#include <TLatex.h>
#include <TVirtualPad.h>

// From StEpcMaker/StEpcCut.cxx: point maker assigns point a radius RAD_SMD_E = 231.23 cm
const Float_t EMCRadius = 231.23;//223.5;
const Float_t truePionMass = 0.1349764;
const Float_t truePionBranchingRatio = 0.98798;
const Float_t trueEtaMass = 0.54730;
const Float_t trueEtaBranchingRatio = 0.3921;
const Float_t trueOmegaMass = 0.78194;

TString findFile(const Char_t *filename) {
    TString result;
    if (filename) {
	TString filenameStr(filename);
	if (gSystem) gSystem->ExpandPathName(filenameStr);
	TString prefix;
	TString pre = "://";
	Int_t colPos = filenameStr.Index(pre);
	if (colPos >= 0) {
	    prefix = filenameStr(0, colPos + pre.Length());
	    filenameStr = filenameStr(colPos + pre.Length(), filenameStr.Length() - (colPos + pre.Length()));
	} else {
	    pre = ":";
	    colPos = filenameStr.Index(pre);
	    if (colPos >= 0) {
		prefix = filenameStr(0, colPos + pre.Length());
		filenameStr = filenameStr(colPos + pre.Length(), filenameStr.Length() - (colPos + pre.Length()));
	    }
	}
	if (filenameStr[0] != '/') {
	    TString macroPath = gROOT ? gROOT->GetMacroPath() : "";
    	    TString incPath = gSystem ? gSystem->GetIncludePath() : "";
    	    incPath += " ";
	    incPath += macroPath;
	    TString fileLocation = gSystem ? gSystem->DirName(filenameStr) : "";
	    incPath.Append(":").Prepend(" ");
	    incPath.ReplaceAll(" -I",":");       // of form :dir1 :dir2:dir3
	    incPath.ReplaceAll(";",":"); // Windows ";" to Unix ":"
	    while (incPath.Index(" :") != -1 ) incPath.ReplaceAll(" :",":");
	    incPath.Prepend(fileLocation+":.:");
	    Char_t *actual = gSystem ? gSystem->Which(incPath, filenameStr) : 0;
	    if (actual) {
		result = prefix + actual;
	    } else {
		result = prefix + filenameStr;
	    }
	} else {
	    result = prefix + filenameStr;
	}
    }
    return result;
}

Int_t floatCompare(const Float_t &f1, const Float_t &f2, Float_t relPrec) {
    Int_t result = 0;
    if (TMath::Abs(f1 - f2) <= relPrec * TMath::Abs(f1)) {
	result = 0;
    } else {
	if (f1 < f2) {
	    result = -1;
	} else {
	    result = +1;
	}

    }
    /*
    const Float_t *precCmp = (f1 != 0) ? &f1 : ((f2 != 0) ? &f2 : (const Float_t*)0);
    if (precCmp) {
	Float_t diff = f1 - f2;
	if (diff < 0) diff = -diff;
	Float_t diffPrec = relPrec * (*precCmp);
	if (diffPrec < 0) diffPrec = -diffPrec;
	if (diff < diffPrec) {
	    result = 0;
	} else {
	    if (f1 < f2) {
	        result = -1;
	    } else {
	        result = +1;
	    }
	}
    }
    */
    return result;
}


Float_t getPseudorapidity(Float_t m, Float_t pT, Float_t y) {
    Float_t mT2 = (m*m) + (pT*pT);
    Float_t shy = TMath::SinH(y);
    Float_t chy = TMath::CosH(y);
    Float_t s = TMath::Sqrt((mT2*chy*chy) - (m*m));
    Float_t t = TMath::Sqrt(mT2) * shy;
    return 0.5 * TMath::Log((s + t) / (s - t));
}

Float_t getRapidity(Float_t m, Float_t pT, Float_t eta) {
    Float_t theta = 2.0 * TMath::ATan(TMath::Exp(-eta));
    Float_t p = pT / TMath::Sin(theta);
    Float_t pL = pT / TMath::Tan(theta);
    Float_t E = TMath::Sqrt((m*m) + (p*p));
    return 0.5 * TMath::Log((E + pL) / (E - pL));
}

Float_t getFontSize(const TVirtualPad *pad, Float_t padWidthPt, Float_t fontSizePt) {
    if (!pad || (padWidthPt == 0)) return 0;
    Float_t wh = TMath::Abs((Float_t)pad->XtoPixel(pad->GetX2()) - (Float_t)pad->XtoPixel(pad->GetX1()));
    Float_t hh = TMath::Abs((Float_t)pad->YtoPixel(pad->GetY2()) - (Float_t)pad->YtoPixel(pad->GetY1()));
    Float_t fontRel = fontSizePt / padWidthPt;
    if (wh > hh) fontRel *= (hh != 0) ? (wh / hh) : 1.0;
    return fontRel * 1.30; // fudge factor
}

void setHistFontSize(TH1 *hist, Float_t padWidthPt, Float_t fontSizePt, const TVirtualPad *pad) {
    if (!pad) pad = gPad;
    if (!pad || !hist) return;
    Float_t fontSizeRel = getFontSize(pad, padWidthPt, fontSizePt);
    hist->GetXaxis()->SetTitleSize(fontSizeRel);
    hist->GetXaxis()->SetLabelSize(fontSizeRel);
    hist->GetYaxis()->SetTitleSize(fontSizeRel);
    hist->GetYaxis()->SetLabelSize(fontSizeRel);
    hist->GetYaxis()->SetTitleSize(fontSizeRel);
    hist->GetYaxis()->SetLabelSize(fontSizeRel);
}

void setLegendFontSize(TLegend *legend, Float_t padWidthPt, Float_t fontSizePt, const TVirtualPad *pad) {
    if (!pad) pad = gPad;
    if (!pad || !legend) return;
    Float_t fontSizeRel = getFontSize(pad, padWidthPt, fontSizePt);
    legend->SetTextSize(fontSizeRel);
}

void setLatexFontSize(TLatex *latex, Float_t padWidthPt, Float_t fontSizePt, const TVirtualPad *pad) {
    if (!pad) pad = gPad;
    if (!pad || !latex) return;
    Float_t fontSizeRel = getFontSize(pad, padWidthPt, fontSizePt);
    latex->SetTextSize(fontSizeRel);
}

void SetShadowColor(TPave *pave, Int_t color) {
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,18,0)
    if (pave) pave->SetShadowColor(color);
#endif
}
