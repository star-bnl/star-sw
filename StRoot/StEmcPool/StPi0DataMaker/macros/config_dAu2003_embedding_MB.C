// Configuration to run pi0 reconstruction chain on the embedding.
// 2003 d-Au MinBias
{
    gROOT->Macro("config_dAu2003_embedding.C"); // load dAu2003 configuration

    dataAnalysisSettings.saveCandidatesWithoutSMD = true;
    dataAnalysisSettings.saveCandidatesWithoutSMDBoth = true;
}

