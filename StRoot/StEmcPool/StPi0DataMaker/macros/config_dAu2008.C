// Configuration to run pi0 reconstruction chain on the real data.
// 2008 d-Au
{
    gROOT->Macro("config_default.C"); // load default configuration

    dataAnalysisSettings.useTriggerSimulatorOriginal = true;

    dataAnalysisSettings.triggers[0] = 210501; // BEMC-HT0
    dataAnalysisSettings.triggers[1] = 210511; // BEMC-HT1
    dataAnalysisSettings.triggers[2] = 210520; // BEMC-HT2
    dataAnalysisSettings.triggers[3] = 210541; // BEMC-HT4
    dataAnalysisSettings.triggers[4] = 210011; // BBC
    dataAnalysisSettings.triggers[5] = 0;
    dataAnalysisSettings.triggersSim = 1;

}
