#if !defined(__CINT__) || defined(__MAKECINT__)

#include <TROOT.h>

#include <StEmcPool/StPi0Results/StPi0ResultsFinal.h>

#endif

void show_analysis(const Char_t *DATA_DIR) {
        gROOT->Macro("style.C");
	show_analysis_final(DATA_DIR);
}
