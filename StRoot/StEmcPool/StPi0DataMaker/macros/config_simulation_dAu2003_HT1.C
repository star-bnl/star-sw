// Configuration to run pi0 reconstruction chain on the dAu2003 HT1 simulated data.
{
    gROOT->Macro("config_simulation_dAu2003.C"); // load general dAu2003 simulation configuration

    dataAnalysisSettings.timestampsFilename = "dAu2003Times_HT1.txt";
}

