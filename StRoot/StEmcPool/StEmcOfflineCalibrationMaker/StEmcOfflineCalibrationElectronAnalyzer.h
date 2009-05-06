#include "TObject.h"
#include "TChain.h"
#include <vector>
#include <map>
using namespace std;

class StEmcOfflineCalibrationElectronAnalyzer : public TObject 
{
 private:

 public:
  StEmcOfflineCalibrationElectronAnalyzer(){}
  virtual ~StEmcOfflineCalibrationElectronAnalyzer(){}

  vector<int> HTtrigs;

  void analyze(TChain*, char*,char*);
  ClassDef(StEmcOfflineCalibrationElectronAnalyzer,1)
};
