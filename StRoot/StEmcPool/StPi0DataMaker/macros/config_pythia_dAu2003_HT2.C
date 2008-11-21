// Configuration to run pi0 reconstruction chain on the pp2005 PYTHIA simulated data - dAu2003 HighTower-2 timestamps.
{
    gROOT->Macro("config_pythia_dAu2003.C"); // load dAu2003 simulation configuration

    dataAnalysisSettings.timestampsFilename = "dAu2003Times_HT2.txt";
}
