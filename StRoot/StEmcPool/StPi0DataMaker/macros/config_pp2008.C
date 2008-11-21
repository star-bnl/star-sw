// Configuration to run pi0 reconstruction chain on the real data.
// 2008 p+p
{
    gROOT->Macro("config_default.C"); // load default configuration

    dataAnalysisSettings.useTriggerSimulatorOriginal = true;

    dataAnalysisSettings.triggers[0] = 220500; // BHT0-MB
    dataAnalysisSettings.triggers[1] = 220510; // BHT1-MB
    dataAnalysisSettings.triggers[2] = 220520; // BHT2-MB-slow
    dataAnalysisSettings.triggers[3] = 220980; // FMS-slow
    dataAnalysisSettings.triggers[4] = 220981; // FMS-slow
    dataAnalysisSettings.triggers[5] = 0;
    dataAnalysisSettings.triggersSim = 1;

}
