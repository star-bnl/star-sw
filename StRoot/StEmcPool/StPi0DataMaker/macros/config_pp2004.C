// Configuration to run pi0 reconstruction chain on the real data.
// 2004 pp
{
    gROOT->Macro("config_default.C"); // load default configuration

    //dataAnalysisSettings.HT1Threshold = 10;
    //dataAnalysisSettings.HT2Threshold = 20;
    //dataAnalysisSettings.TriggerAdc = 32;
    //dataAnalysisSettings.useFullEmcTriggerSimulator = true;
    dataAnalysisSettings.useTriggerSimulatorOriginal = false;

    dataAnalysisSettings.triggers[0] = 45010;
    dataAnalysisSettings.triggers[1] = 45201;
    dataAnalysisSettings.triggers[2] = 45202;
    dataAnalysisSettings.triggers[3] = 0;
    dataAnalysisSettings.triggersSim = 1;
    dataAnalysisSettings.triggersMB = 1;
    dataAnalysisSettings.triggersHT1 = 2;
    dataAnalysisSettings.triggersHT2 = 4;
}
