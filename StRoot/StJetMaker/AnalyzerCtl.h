#ifndef ANALYZERCTL_H
#define ANALYZERCTL_H


#include <string>

class StppJetAnalyzer;
class StJets;

struct AnalyzerCtl {
  std::string mBranchName;
  StppJetAnalyzer* mAnalyzer;
  StJets *mJets;
};

#endif // ANALYZERCTL_H

