// Configuration to run pi0 reconstruction chain on the pp2005 simulated data.
{
    gROOT->Macro("config_pp2005.C"); // load normal pp2005 data configuration
    gROOT->Macro("config_add_simulation.C"); // add simulation configuration

    dataAnalysisSettings.useTriggerSimulatorOriginal = false;
    dataAnalysisSettings.useTriggerSimulatorEmbed = false;

    dataAnalysisSettings.timestampsFilename = "";
    dataAnalysisSettings.timeRandomNormEventsTotal = 20000;
  
    dataAnalysisSettings.CalibSpreadTower = 0.05;
    dataAnalysisSettings.CalibOffsetTower = 0.0;
    dataAnalysisSettings.CalibSpreadPreshower = 0.0;
    dataAnalysisSettings.CalibOffsetPreshower = 0.0;
    dataAnalysisSettings.CalibSpreadSMDE = 0.2;
    dataAnalysisSettings.CalibOffsetSMDE = 0.1;
    dataAnalysisSettings.CalibSpreadSMDP = 0.2;
    dataAnalysisSettings.CalibOffsetSMDP = 0.0;
}

