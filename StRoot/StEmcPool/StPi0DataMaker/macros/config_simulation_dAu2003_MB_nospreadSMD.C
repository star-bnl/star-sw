// Configuration to run pi0 reconstruction chain on the dAu2003 MB simulated data w/o SMD calibration spread.
{
    gROOT->Macro("config_simulation_dAu2003_nospreadSMD.C"); // load dAu2003 simulation configuration w/o SMD calibration spread

    dataAnalysisSettings.timestampsFilename = "dAu2003Times_MB.txt";
}

