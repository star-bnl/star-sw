// Configuration to run pi0 reconstruction chain on the embedding.
// 2003 d-Au
{
    gROOT->Macro("config_dAu2003.C"); // load dAu2003 configuration
    gROOT->Macro("config_add_simulation.C"); // add simulation configuration

    dataAnalysisSettings.isSimulation = false;
    dataAnalysisSettings.isEmbedding = true;

    dataAnalysisSettings.useTriggerSimulatorEmbed = true;

    dataAnalysisSettings.CalibSpreadTower = 0.1;
    dataAnalysisSettings.CalibOffsetTower = 0.0;
    dataAnalysisSettings.CalibSpreadPreshower = 0.0;
    dataAnalysisSettings.CalibOffsetPreshower = 0.0;
    dataAnalysisSettings.CalibSpreadSMDE = 0.2;
    dataAnalysisSettings.CalibOffsetSMDE = 0.1;
    dataAnalysisSettings.CalibSpreadSMDP = 0.2;
    dataAnalysisSettings.CalibOffsetSMDP = 0.0;

    dataAnalysisSettings.datasetNameStEvent = "StEvent";
}

