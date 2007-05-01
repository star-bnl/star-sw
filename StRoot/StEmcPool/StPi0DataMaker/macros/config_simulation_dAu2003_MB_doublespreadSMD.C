// Configuration to run pi0 reconstruction chain on the dAu2003 MB simulated data w/ 2x of SMD calibration spread.
{
    gROOT->Macro("config_simulation_dAu2003_doublespreadSMD.C"); // load dAu2003 simulation configuration w/ 2x of SMD calibration spread

    dataAnalysisSettings.timestampsFilename = "dAu2003Times_MB.txt";
}

