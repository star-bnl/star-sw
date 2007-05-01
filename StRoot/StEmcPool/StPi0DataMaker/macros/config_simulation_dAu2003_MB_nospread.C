// Configuration to run pi0 reconstruction chain on the dAu2003 MB simulated data w/o calibration spread.
{
    gROOT->Macro("config_simulation_dAu2003_nospread.C"); // load dAu2003 simulation configuration w/o calibration spread

    dataAnalysisSettings.timestampsFilename = "dAu2003Times_MB.txt";
}

