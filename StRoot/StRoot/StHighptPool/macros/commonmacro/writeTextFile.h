#ifndef WRITETEXTFILE_H
#define WRITETEXTFILE_H

#include <iostream.h>
#include <getopt.h>
#include <math.h>
#include <fstream.h>

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

int main(int,char**);


#endif
