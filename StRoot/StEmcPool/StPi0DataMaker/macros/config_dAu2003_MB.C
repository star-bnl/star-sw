// Configuration to run pi0 reconstruction chain on the real data.
// 2003 d+Au MinBias
{
    gROOT->Macro("config_dAu2003.C"); // load d+Au 2003 configuration

    dataAnalysisSettings.saveCandidatesWithoutSMD = true;
    dataAnalysisSettings.saveCandidatesWithoutSMDBoth = true;

    dataAnalysisSettings.ClusterSizeMaxSMDE = 0;
    dataAnalysisSettings.ClusterEnSeedSMDE = 1000;
    dataAnalysisSettings.ClusterEnAddSMDE = 1000;
    dataAnalysisSettings.ClusterEnTotSMDE = 1000;
    dataAnalysisSettings.ClusterCheckSMDE = kFALSE;
    dataAnalysisSettings.ClusterSizeMaxSMDP = 0;
    dataAnalysisSettings.ClusterEnSeedSMDP = 1000;
    dataAnalysisSettings.ClusterEnAddSMDP = 1000;
    dataAnalysisSettings.ClusterEnTotSMDP = 1000;
    dataAnalysisSettings.ClusterCheckSMDP = kFALSE;
}
