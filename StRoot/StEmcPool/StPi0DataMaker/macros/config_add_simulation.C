// Configuration to run pi0 reconstruction chain on the simulated data.
{
    dataAnalysisSettings.isSimulation = true;
    dataAnalysisSettings.isEmbedding = false;

    dataAnalysisSettings.saveMCPions = false;
    dataAnalysisSettings.saveMCPionsPlain = true;
    dataAnalysisSettings.saveMCEtas = false;
    dataAnalysisSettings.saveMCEtasPlain = true;
    dataAnalysisSettings.saveMCNbars = false;
    dataAnalysisSettings.saveMCNbarsPlain = true;
    dataAnalysisSettings.saveMCGammas = false;
    dataAnalysisSettings.saveMCGammasPlain = true;

    dataAnalysisSettings.datasetNameStEvent = "IO_Root/.data/bfcTree/eventBranch/StEvent";
}

