#ifndef ANALYZERCTL_H
#define ANALYZERCTL_H


#include <string>

class StppJetAnalyzer;
class StJets;

namespace StSpinJet {

struct AnalyzerCtl {
  std::string mBranchName;
  StppJetAnalyzer* mAnalyzer;
  StJets *mJets;
};

}

#endif // ANALYZERCTL_H

