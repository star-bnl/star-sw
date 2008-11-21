// Configuration to run pi0 reconstruction chain on the pp2005 PYTHIA simulated data - MinBias timestamps.
{
    gROOT->Macro("config_pythia_pp2005.C"); // load pp2005 PYTHIA simulation configuration

    dataAnalysisSettings.timestampsFilename = "pp2005Times_MB.txt";

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
