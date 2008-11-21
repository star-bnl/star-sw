// Configuration to run pi0 reconstruction chain on the real data.
// 2006 pp
{
    gROOT->Macro("config_default.C"); // load default configuration

    dataAnalysisSettings.triggers[0] = 117001;
    dataAnalysisSettings.triggers[1] = 117201;
    dataAnalysisSettings.triggers[2] = 117211;
    dataAnalysisSettings.triggers[3] = 117212;
    dataAnalysisSettings.triggers[4] = 127212;
    dataAnalysisSettings.triggers[5] = 127213;
    dataAnalysisSettings.triggers[6] = 137213;
    dataAnalysisSettings.triggers[7] = 0;
    dataAnalysisSettings.triggersSim = 1;

    dataAnalysisSettings.jetFullMakerUse2006Cuts = true;
}

