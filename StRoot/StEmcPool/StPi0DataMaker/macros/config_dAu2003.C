// Configuration to run pi0 reconstruction chain on the real data.
// 2003 d-Au
{
    gROOT->Macro("config_default.C"); // load default configuration

    dataAnalysisSettings.useTriggerSimulatorOriginal = true;

    dataAnalysisSettings.triggers[0] = 2001;
    dataAnalysisSettings.triggers[1] = 2003;
    dataAnalysisSettings.triggers[2] = 2201;
    dataAnalysisSettings.triggers[3] = 2202;
    dataAnalysisSettings.triggers[4] = 0;
    dataAnalysisSettings.triggersSim = 1 + 2;

    dataAnalysisSettings.jetFullMakerUse2003Cuts = true;
}
