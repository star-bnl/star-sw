#include "TSystem.h"
#include "TString.h"
#include "TObjString.h"
#include "TObjArray.h"
#include <iostream>
#include <vector>

#include "EmbeddingChainOptions.h"

void runHFTWrapper(
    const int   nevents_ , 
    const char* daqfile_   = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.daq", 
    const char* tagfile_   = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.tags.root" , 
    double ptmn_           = 0. , 
    double ptmx_           = 5. ,
    double etamn_          = -1., 
    double etamx_          = 1. , 
    double vzmn_           = -6., 
    double vzmx_           = 6. , 
    double vr_             = 99., 
    int pid_               = 8,
    double mult_           = 1, 
    std::vector<int> triggers_  = {530003} , 
    const char* prodname_  = "P17iddAu200hft" , 
    const char* kintype_   = "FlatPT"      ,
    bool simIn_            = false         ,
    const char* macroDir_  = "StRoot/macros/embedding"  
) {

    TString dir(macroDir_);
    if (!dir.EndsWith("/") && dir.Length() > 0) {
        dir += "/";
    }
    TString simMacro = dir + "runEmbeddingSimulationHftG4.C";
    TString embMacro = dir + "bfcMixer_HftG4.C";

    EmbeddingChains getChainOptions;
    auto opts = getChainOptions( geant4star, prodname_, false );
    
    if (!opts.isValid) {
        std::cerr << "Production " << prodname_ << " is not valid in EmbeddingChainOptions.h" << std::endl;
        return;
    }

    TString dbv = "";
    TString geom = "";

    TString allOpts = opts.loadopts.c_str();
    TObjArray* tokens = allOpts.Tokenize(" ,");
    for(int i = 0; i < tokens->GetEntries(); ++i) {
        TString tok = ((TObjString*)tokens->At(i))->GetString();
        if (tok.BeginsWith("dbv")) {
            dbv = tok;
        } else if (tok.BeginsWith("ry2") || tok.BeginsWith("y2")) {
            // TODO: report that removing the r crashs in StGeant4Maker::ConstructGeometry
            if (tok.BeginsWith("ry2")) tok.Remove(0, 1);
            geom = tok;
        }
    }
    delete tokens;

    if (dbv == "" || geom == "") {
        std::cerr << "Could not parse DbV or geometry from chain options!" << std::endl;
        return;
    }

    TString simfile_ = gSystem->BaseName(daqfile_);
    simfile_.ReplaceAll(".daq", "_HftG4_sim.geant.root");

    TString trigStr = "{";
    for (size_t i = 0; i < triggers_.size(); ++i) {
        trigStr += Form("%d", triggers_[i]);
        if (i < triggers_.size() - 1) trigStr += ",";
    }
    trigStr += "}";

    std::cout << "\n========================================================\n";
    std::cout << " DAQ:     " << daqfile_ << "\n";
    std::cout << " TAG:     " << tagfile_ << "\n";
    std::cout << " SIM OUT: " << simfile_ << "\n";
    std::cout << " PROD:    " << prodname_ << " (DbV: " << dbv << ", Geom: " << geom << ")\n";
    std::cout << "========================================================\n";


    TString cmdSim = Form("root -l -b -q \'%s(%d, \"%s\", \"%s\", %f,  %f, %f, %f, %f, %f, %f, %d, %f, %s, \"%s\", \"%s\")\'",
                          simMacro.Data(), nevents_, daqfile_, tagfile_, ptmn_, ptmx_, etamn_, etamx_, vzmn_, vzmx_, vr_, pid_, mult_, trigStr.Data(), dbv.Data(), geom.Data());
    
    std::cout << "\n---> Running Simulation Macro:\n" << cmdSim << "\n";

    int statusSim = gSystem->Exec(cmdSim.Data());
    
    if (statusSim != 0) {
        std::cerr << "\nSimulation macro failed. Exiting.\n";
        return;
    }

    TString cmdEmbed = Form("root4star -l -b -q \'%s(%d, \"%s\", \"%s\", \"%s\", %f, %f, %f, %s, \"%s\")\'",
                            embMacro.Data(), nevents_, daqfile_, tagfile_, simfile_.Data(), vzmn_, vzmx_, vr_, trigStr.Data(), prodname_);
    
    std::cout << "\n---> Running Embedding Macro:\n" << cmdEmbed << "\n";
    int statusEmbed = gSystem->Exec(cmdEmbed.Data());

    if (statusEmbed != 0) {
        std::cerr << "\nEmbedding macro failed. Exiting.\n";
        return;
    }

}