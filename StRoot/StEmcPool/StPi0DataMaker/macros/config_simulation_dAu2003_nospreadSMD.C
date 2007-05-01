// Configuration to run pi0 reconstruction chain on the dAu2003 simulated data w/o SMD calibration spread.
{
    gROOT->Macro("config_dAu2003.C"); // load normal dAu2003 data configuration
    gROOT->Macro("config_add_simulation.C"); // add simulation configuration

    dataAnalysisSettings.useTriggerSimulatorOriginal = false;
    dataAnalysisSettings.useTriggerSimulatorEmbed = false;

    dataAnalysisSettings.timestampsFilename = "";
    dataAnalysisSettings.timeRandomNormEventsTotal = 20000;

    dataAnalysisSettings.CalibSpreadTower = 0.1;
    dataAnalysisSettings.CalibOffsetTower = 0.0;
    dataAnalysisSettings.CalibSpreadPreshower = 0.0;
    dataAnalysisSettings.CalibOffsetPreshower = 0.0;
    dataAnalysisSettings.CalibSpreadSMDE = 0.2 * 0.0;
    dataAnalysisSettings.CalibOffsetSMDE = 0.1 * 0.0;
    dataAnalysisSettings.CalibSpreadSMDP = 0.2 * 0.0;
    dataAnalysisSettings.CalibOffsetSMDP = 0.0 * 0.0;

}

