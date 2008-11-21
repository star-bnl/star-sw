// Configuration to run pi0 reconstruction chain on the pp2005 PYTHIA simulated data - HighTower-1 timestamps.
{
    gROOT->Macro("config_pythia_pp2005.C"); // load pp2005 PYTHIA simulation configuration

    dataAnalysisSettings.timestampsFilename = "pp2005Times_HT1.txt";
}
