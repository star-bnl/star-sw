#if !defined(__CINT__) || defined(__MAKECINT__)

#include <TSystem.h>

#endif

void run_jprof() {
    gSystem->Setenv("JPROF_FLAGS", "JP_START JP_PERIOD=0.001");
    gSystem->Load("libJprof");
}
