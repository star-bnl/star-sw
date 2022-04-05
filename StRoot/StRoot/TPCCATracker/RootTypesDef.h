#ifndef RootTypesDef_h
#define RootTypesDef_h 1

#ifndef HLTCA_STANDALONE

#include "TROOT.h"

#else
typedef int Int_t;
typedef float Float_t;
typedef double Double_t;
typedef bool Bool_t;

class TObject
{
};
#endif

#endif // RootTypesDef_h
