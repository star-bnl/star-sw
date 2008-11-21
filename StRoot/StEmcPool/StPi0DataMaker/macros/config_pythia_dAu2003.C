// Configuration to run pi0 reconstruction chain on the pp2005 PYTHIA simulated data.
{
    gROOT->Macro("config_simulation_dAu2003.C"); // load general dAu2003 simulation configuration

    dataAnalysisSettings.isPythia = true;

    dataAnalysisSettings.saveFirstMCGammaOnly = false;
    dataAnalysisSettings.saveFirstMCPionOnly = false;
    dataAnalysisSettings.saveFirstMCEtaOnly = false;
    dataAnalysisSettings.saveFirstMCNbarOnly = false;
}

