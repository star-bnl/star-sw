// Configuration to run pi0 reconstruction chain on the real data.
// 2003 d-Au
{
    gROOT->Macro("config_default.C"); // load default configuration

    dataAnalysisSettings.HT1Threshold = 8;
    dataAnalysisSettings.HT2Threshold = 13;
    dataAnalysisSettings.TriggerAdc = 32;
    dataAnalysisSettings.useFullEmcTriggerSimulator = false;

    dataAnalysisSettings.triggers[0] = 2001;
    dataAnalysisSettings.triggers[1] = 2003;
    dataAnalysisSettings.triggers[2] = 2201;
    dataAnalysisSettings.triggers[3] = 2202;
    dataAnalysisSettings.triggers[4] = 0;
    dataAnalysisSettings.triggersSim = 1 + 2;
    dataAnalysisSettings.triggersMB = 1 + 2;
    dataAnalysisSettings.triggersHT1 = 4;
    dataAnalysisSettings.triggersHT2 = 8;

    dataAnalysisSettings.jetFullMakerUse2003TowerCuts = true;
}
