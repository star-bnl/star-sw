#include "StPi0CommonUtil.h"

#include <TROOT.h>
#include <TSystem.h>
#include <TMath.h>

// From StEpcMaker/StEpcCut.cxx: point maker assigns point a radius RAD_SMD_E = 231.23 cm
const Float_t EMCRadius = 231.23;//223.5;
const Float_t truePionMass = 0.1349764;
const Float_t truePionBranchingRatio = 0.98798;
const Float_t trueEtaMass = 0.54730;
const Float_t trueEtaBranchingRatio = 0.3921;

TString findFile(const Char_t *filename) {
    TString result;
    if (filename) {
	TString filenameStr(filename);
	if (gSystem) gSystem->ExpandPathName(filenameStr);
	TString prefix;
	Int_t colPos = filenameStr.Index(":");
	if (colPos >= 0) {
	    prefix = filenameStr(0, colPos + 1);
	    filenameStr = filenameStr(colPos + 1, filenameStr.Length() - (colPos + 1));
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
    return result;
}
