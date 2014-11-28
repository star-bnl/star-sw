#ifndef READTEXTFILE_H
#define READTEXTFILE_H

#include <iostream.h>
#include <getopt.h>
#include <math.h>
#include <fstream.h>
#include <stdlib.h>

#include <TColor.h>
#include <TStyle.h>
#include <TString.h>
#include <TFile.h>
#include <TH2.h>
#include <TH3.h> 
#include <TApplication.h>
#include <TROOT.h>
#include <TCanvas.h>
#include <TIterator.h>
#include <TKey.h>

extern void InitGui();
VoidFuncPtr_t initfuncs[] = { InitGui, 0 };
TROOT root("slice", "Residual Histogram projection", initfuncs);

void parse(TString& a, char* b);
TString split(TString&);
void removeLead(TString &a);
void removeTrail(TString &a);
#endif
