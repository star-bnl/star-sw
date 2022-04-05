#ifndef common_H
#define common_H

char title[200], name[200];
TString sTitle,sName;

const Int_t nVarBin = 2;
const Int_t nWeight = 4;

const char* border = "************************* ";


const Int_t  nDcaFraction = 6;
Axis_t dcaFraction[nDcaFraction] = {.5,.6,.7,.8,.9,1.};

const Int_t nValue = 8;
Axis_t hitValues[] = { 25,28,30,32,34,36,38,40};


Int_t  nBin, nBinX, lowBin, hiBin;
Axis_t xMin, xMax;

Axis_t lowPt=1.7, highPt=6.0;

TString sPM[2];
sPM[0] = "plus"; sPM[1] = "minus";

TString sEW[2];
sEW[0] = "East"; sEW[1] = "West";

#endif
