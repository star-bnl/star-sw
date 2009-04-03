#ifndef BEMCPlotsPresenter_H
#define BEMCPlotsPresenter_H


class TPad;
class GenericFile;
class BemcTwMask;

typedef GenericFile FileType;

class BEMCPlotsPresenter {
public:

  static void displayTab(int tab, int panel, FileType file, TPad *pad, const char *bemcStatusFilename, int mDebug);
  static void displayTab(int tab, int panel, FileType file, TPad *pad, int mDebug); // temporary

  // tab=0 "BEMC"
  // panel=0 "Status"
  // panel=1 "Towers"
  // panel=2 "SMD/PSD"
  // panel=3 "Trigger"
  // panel=4 "Jet"
  // panel=5 "BTOW ADC"
  // panel=6 "JetPatch HighTower Spectra"
  // panel=7 "JetPatch PatchSum Spectra"
  // panel=8 "DSM Level-0 Input"
  // panel=9 "DSM Level-1 Input"
  // panel=10 "DSM level-2 Input"
  // panel=11 "BSMD FEE Sum"
  // panel=12 "Trigger corruption"
  // panel=13 "BPRS FEE Sum"
  // panel=14 "BPRS ADC"
  static void displayStatus(FileType file, TPad *pad, int mDebug);
  static void displayTowers(FileType file, TPad *pad, int mDebug);
  static void displaySMDPSD(FileType file, TPad *pad, int mDebug);
  static void displayTrigger(FileType file, TPad *pad, int mDebug);
  static void displayJet(FileType file, TPad *pad, int mDebug);
  static void displayRawAdc(FileType file, TPad *pad, bool psd, bool zoom, BemcTwMask *twMask, int mDebug);
  static void displayJetPatchHT(FileType file, TPad *pad, int mDebug);
  static void displayJetPatchSum(FileType file, TPad *pad, int mDebug);
  static void displayL0Input(FileType file, TPad *pad, int mDebug);
  static void displayL1Input(FileType file, TPad *pad, int mDebug);
  static void displayL2Input(FileType file, TPad *pad, int mDebug);
  static void displayL3Input(FileType file, TPad *pad, int mDebug);
  static void displaySmdFeeSum(FileType file, TPad *pad, int mDebug);
  static void displayPsdFeeSum(FileType file, TPad *pad, int mDebug);
  static void displayTriggerCorruption(FileType file, TPad *pad, bool hold, int mDebug);
  static void displayAdcEtaPhi(FileType file, TPad *pad, int mDebug);

};

#endif

