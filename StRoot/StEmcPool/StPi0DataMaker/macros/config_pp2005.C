// Configuration to run pi0 reconstruction chain on the real data.
// 2005 pp
{
    gROOT->Macro("config_default.C"); // load default configuration

    //dataAnalysisSettings.HT1Threshold = 13;
    //dataAnalysisSettings.HT2Threshold = 17;
    //dataAnalysisSettings.TriggerAdc = 16;
    //dataAnalysisSettings.useFullEmcTriggerSimulator = true;
    dataAnalysisSettings.useTriggerSimulatorOriginal = false;

    dataAnalysisSettings.triggers[0] = 96011;
    dataAnalysisSettings.triggers[1] = 106011;
    dataAnalysisSettings.triggers[2] = 96201;
    dataAnalysisSettings.triggers[3] = 106201;
    dataAnalysisSettings.triggers[4] = 96211;
    dataAnalysisSettings.triggers[5] = 106211;
    dataAnalysisSettings.triggers[6] = 0;
    dataAnalysisSettings.triggersSim = 1 + 2;
    dataAnalysisSettings.triggersMB = 1 + 2;
    dataAnalysisSettings.triggersHT1 = 4 + 8;
    dataAnalysisSettings.triggersHT2 = 16 + 32;
}

