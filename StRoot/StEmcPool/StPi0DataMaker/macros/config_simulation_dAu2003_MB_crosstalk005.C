// Configuration to run pi0 reconstruction chain on the dAu2003 MB simulated data with SMD crosstalk.
{
    gROOT->Macro("config_simulation_dAu2003.C"); // load dAu2003 simulation configuration
    gROOT->Macro("config_crosstalk_005.C"); // SMD crosstalk

    dataAnalysisSettings.timestampsFilename = "dAu2003Times_MB.txt";
}

