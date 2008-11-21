// Configuration to run pi0 reconstruction chain on the real data.
// 2006 pp MinBias
{
    gROOT->Macro("config_pp2006.C"); // load pp2006 configuration

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

