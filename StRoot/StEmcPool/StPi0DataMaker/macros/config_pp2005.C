// Configuration to run pi0 reconstruction chain on the real data.
// 2005 pp
{
    gROOT->Macro("config_default.C"); // load default configuration

    dataAnalysisSettings.triggers[0] = 96011;
    dataAnalysisSettings.triggers[1] = 96201;
    dataAnalysisSettings.triggers[2] = 96211;
    dataAnalysisSettings.triggers[3] = 0;
    dataAnalysisSettings.triggersSim = 1;

    dataAnalysisSettings.jetFullMakerUse2005Cuts = true;
}
