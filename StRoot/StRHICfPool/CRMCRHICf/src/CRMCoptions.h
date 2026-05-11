#ifndef _CRMCoptions_h_
#define _CRMCoptions_h_
#include <vector>
#include <string>

class CRMCoptions {

  friend class OutputPolicyROOT;

  CRMCoptions();

 public:

  enum EOutputMode {
    eHepMC,
    eHepMCGZ,
    eHepMC3,
    eHepMC3GZ,
    eLHE,
    eLHEGZ,
    eROOT,
    eRivet,
    eNone,
  };

  CRMCoptions(int argc, char** argv);
  virtual ~CRMCoptions() {}

  bool OptionsError() const { return fError; }
  void DumpConfig() const;
  EOutputMode GetOutputMode() const { return fOutputMode; }
  std::string GetOutputTypeEnding() const;
  std::string GetOutputFileName() const;

  const std::string GetRHICfRunType() const { return fRHICfRunType;}
  const std::string GetJobIndex() const { return fJobIndex;}

  std::string ParticleName(const int pid) const;
  const std::string& GetParamFileName() const { return fParamFileName; }

  int GetHEModel() const { return fHEModel; }
  bool IsTest() const { return fTest; }
  bool IsCSMode() const { return fCSMode; }
  int GetSeed() const { return fSeed; }
  int GetTypout() const { return fTypout; }
  bool ProduceTables() const { return fProduceTables; }
  //std::string GetFilter() const { return fFilter; }

  int GetNCollision() const { return fNCollision; }
  double GetSqrts() const { return fSqrts; }  
  void SetProjectileMomentum(const double p) { fProjectileMomentum = p; }
  void SetTargetMomentum(const double p) { fTargetMomentum = p; }
  double GetProjectileMomentum() const { return fProjectileMomentum; }
  double GetTargetMomentum() const { return fTargetMomentum; }

  int GetProjectileId() const { return fProjectileId; }
  int GetTargetId() const { return fTargetId; }
  const std::vector<std::string>& GetRivetSearch() const { return fRivetSearch;}
  const std::vector<std::string>& GetRivetPreloads() const { return fRivetPreloads;}
  const std::vector<std::string>& GetRivetAnalyses() const { return fRivetAnalyses;}

 protected:

  bool fError;
  EOutputMode fOutputMode;

  // real data members

  int fNCollision;
  int fSeed;
  int fProjectileId;
  int fTargetId;
  int fHEModel;
  int fTypout;
  double fProjectileMomentum;
  double fTargetMomentum;
  double fSqrts;
  std::string fParamFileName;
  std::string fOutputFileName;
  std::string fRHICfRunType;
  std::string fJobIndex;
  std::vector<std::string> fRivetAnalyses;
  std::vector<std::string> fRivetSearch;
  std::vector<std::string> fRivetPreloads;

  bool fProduceTables;
  bool fSeedProvided;
  //std::string fFilter;
  bool fTest;
  bool fCSMode;

 private:

  void CheckEnvironment();
  void ParseOptions(int argc, char** argv);

};

#endif
