#ifndef BEMCPlotsPresenter_H
#define BEMCPlotsPresenter_H

#include <GenericFile.h>

class TPad;

typedef GenericFile FileType;

class BEMCPlotsPresenter {
public:
    // tab=0 "BEMC"
    // panel=0 "BTOW ADC"
    // panel=1 "JetPatch HighTower Spectra"
    // panel=2 "JetPatch PatchSum Spectra"
    // panel=3 "DSM Level-0 Input"
    // panel=4 "DSM Level-1 Input"
    // panel=5 "DSM level-2 Input"
    // panel=6 "BSMD FEE Sum"
    // panel=7 "Trigger corruption"
    // panel=8 "BPRS FEE Sum"
    // panel=9 "BPRS ADC"
    static void displayTab(int tab, int panel, FileType file, TPad *pad, int mDebug);
    static void displayRawAdc(FileType file, TPad *pad, bool psd, int mDebug);
    static void displayJetPatchHT(FileType file, TPad *pad, int mDebug);
    static void displayJetPatchSum(FileType file, TPad *pad, int mDebug);
    static void displayL0Input(FileType file, TPad *pad, int mDebug);
    static void displayL1Input(FileType file, TPad *pad, int mDebug);
    static void displayL2Input(FileType file, TPad *pad, int mDebug);
    static void displayL3Input(FileType file, TPad *pad, int mDebug);
    static void displaySmdFeeSum(FileType file, TPad *pad, int mDebug);
    static void displayPsdFeeSum(FileType file, TPad *pad, int mDebug);
    static void displayTriggerCorruption(FileType file, TPad *pad, int mDebug);
};

#endif

