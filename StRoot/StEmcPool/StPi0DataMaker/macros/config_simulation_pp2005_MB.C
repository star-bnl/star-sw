// Configuration to run pi0 reconstruction chain on the pp2005 MB simulated data.
{
    gROOT->Macro("config_simulation_pp2005.C"); // load general pp2005 simulation configuration

    dataAnalysisSettings.timestampsFilename = "pp2005Times_MB.txt";
}

