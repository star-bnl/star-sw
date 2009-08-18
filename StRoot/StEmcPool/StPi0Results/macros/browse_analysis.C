#if !defined(__CINT__) || defined(__MAKECINT__)

#include <TROOT.h>
#include <TFolder.h>
#include <TBrowser.h>
#include <TCanvas.h>

#include <StEmcPool/StPi0Analysis/TDataProcessorPool.h>

#include <StEmcPool/StPi0Results/TDataProcessorMemberInspector.h>

#endif

TFolder *dataFolder = 0;
TDataProcessorMemberInspector memberInspector;

void POOL(const Char_t *NAME, const Char_t *FILE) {
    if (NAME && FILE && dataFolder) {
	TDataProcessorPool *pool = new TDataProcessorPool(NAME, NAME);
	if (pool) {
	    pool->processFile(FILE);
	    memberInspector.Inspect(pool, dataFolder);
	}
    }
}

void browse_analysis() {

    gROOT->Macro("style.C");

    TFolder *rootFolder = gROOT->GetRootFolder();
    dataFolder = rootFolder->AddFolder("processedData", "Processed data");

    //TString DATA = "pi0jet";
    TString DATA = "towerMB_smd2HT1";
    //TString DATA = "towerMB_smd2HT1_gammaetacut";
    //TString DATA = "2";

    //POOL("pp data MB",  "data_output/" + DATA + "/data_pp2005_MB_ppProductionMinBias.root");
    //POOL("pp data HT1",  "data_output/" + DATA + "/data_pp2005_HT1_ppProduction.root");
    //POOL("pp data HT2",  "data_output/" + DATA + "/data_pp2005_HT2_ppProduction.root");

    POOL("dAu data HT2 UPCCombined",  "data_output/" + DATA + "/data_dAu2003_HT2_UPCCombined_nocentral.root");
    POOL("dAu data HT2 dAuCombined",  "data_output/" + DATA + "/data_dAu2003_HT2_dAuCombined_nocentral.root");

    //POOL("pp nbar data MB",  "data_output/" + DATA + "/simulation_pp2005_nbar_data_MB.root");
    //POOL("pp nbar data HT1",  "data_output/" + DATA + "/simulation_pp2005_nbar_data_HT1.root");
    //POOL("pp nbar data HT2",  "data_output/" + DATA + "/simulation_pp2005_nbar_data_HT2.root");

    //POOL("pp pi0 MB", "data_output/" + DATA + "/simulation_pp2005_pi0_MB.root");
    //POOL("pp pi0 HT1", "data_output/" + DATA + "/simulation_pp2005_pi0_HT1.root");
    //POOL("pp pi0 HT2", "data_output/" + DATA + "/simulation_pp2005_pi0_HT2.root");

    //POOL("pp eta MB", "data_output/" + DATA + "/simulation_pp2005_eta_MB.root");
    //POOL("pp eta HT1", "data_output/" + DATA + "/simulation_pp2005_eta_HT1.root");
    //POOL("pp eta HT2", "data_output/" + DATA + "/simulation_pp2005_eta_HT2.root");

    //POOL("pp gamma data MB", "data_output/" + DATA + "/simulation_pp2005_gamma_data_MB.root");
    //POOL("pp gamma data HT1", "data_output/" + DATA + "/simulation_pp2005_gamma_data_HT1.root");
    //POOL("pp gamma data HT2", "data_output/" + DATA + "/simulation_pp2005_gamma_data_HT2.root");

    //POOL("pp gamma pi0 MB", "data_output/" + DATA + "/simulation_pp2005_gamma_pi0_MB.root");
    //POOL("pp gamma pi0 HT1", "data_output/" + DATA + "/simulation_pp2005_gamma_pi0_HT1.root");
    //POOL("pp gamma pi0 HT2", "data_output/" + DATA + "/simulation_pp2005_gamma_pi0_HT2.root");

    //POOL("pp gamma eta MB", "data_output/" + DATA + "/simulation_pp2005_gamma_eta_MB.root");
    //POOL("pp gamma eta HT1", "data_output/" + DATA + "/simulation_pp2005_gamma_eta_HT1.root");
    //POOL("pp gamma eta HT2", "data_output/" + DATA + "/simulation_pp2005_gamma_eta_HT2.root");

    //POOL("dAu data HT1 dAuCombined nocentral",  "data_output/" + DATA + "/data_dAu2003_HT1_dAuCombined_nocentral.root");

    //POOL("pp2008 HT0",  "data_output/pp2008/data_pp2008_MB.root");
    //POOL("pp2008 HT1",  "data_output/pp2008/data_pp2008_HT1.root");
    //POOL("pp2008 HT2",  "data_output/pp2008/data_pp2008_HT2.root");

    //POOL("smd0103 pp data HT1",  "data_output/smd0103/data_pp2005_HT1_ppProduction.root");
    //POOL("smd0103 pp pi0 HT1", "data_output/smd0103/simulation_pp2005_pi0_HT1.root");

    //POOL("smd0305 pp data HT1",  "data_output/smd0305/data_pp2005_HT1_ppProduction.root");
    //POOL("smd0305 pp pi0 HT1", "data_output/smd0305/simulation_pp2005_pi0_HT1.root");

    //POOL("smd0507 pp data HT1",  "data_output/smd0507/data_pp2005_HT1_ppProduction.root");
    //POOL("smd0507 pp pi0 HT1", "data_output/smd0507/simulation_pp2005_pi0_HT1.root");

    //POOL("smd0709 pp data HT1",  "data_output/smd0709/data_pp2005_HT1_ppProduction.root");
    //POOL("smd0709 pp pi0 HT1", "data_output/smd0709/simulation_pp2005_pi0_HT1.root");

    new TBrowser("dataBrowser", dataFolder, "Processed data browser");
    new TCanvas("Canvas", "Processed data plot");
}
