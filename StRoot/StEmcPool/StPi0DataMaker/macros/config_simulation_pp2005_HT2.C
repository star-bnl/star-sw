// Configuration to run pi0 reconstruction chain on the pp2005 HT2 simulated data.
{
    gROOT->Macro("config_simulation_pp2005.C"); // load general pp2005 simulation configuration

    dataAnalysisSettings.timestampsFilename = "pp2005Times_HT2.txt";
}

