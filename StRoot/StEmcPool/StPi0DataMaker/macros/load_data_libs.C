void load_data_libs() {
    {
	Info(__FILE__, "Enabling Logger...");
	gROOT->Macro("LoadLogger.C");
    }

    Info(__FILE__, "Start loading libraries...");

#define LOAD_LIB(NAME) { Info(__FILE__, "Loading %s", NAME); gSystem->Load(NAME); }

    LOAD_LIB("StarRoot");
    LOAD_LIB("StarClassLibrary");
    LOAD_LIB("libMySQL");
    LOAD_LIB("St_base");
    LOAD_LIB("StChain");
    LOAD_LIB("St_Tables");
    LOAD_LIB("StarMagField");
    LOAD_LIB("StMagF");
    LOAD_LIB("StUtilities");
    LOAD_LIB("StTreeMaker");
    LOAD_LIB("StIOMaker");
    LOAD_LIB("StTpcDb");
    LOAD_LIB("StDetectorDbMaker");
    LOAD_LIB("StDbUtilities");
    LOAD_LIB("StEvent");
    LOAD_LIB("StEventUtilities");
    LOAD_LIB("StMcEvent");
    LOAD_LIB("StMcEventMaker"); 
    LOAD_LIB("StStrangeMuDstMaker"); 
    LOAD_LIB("StEmcUtil"); 
    LOAD_LIB("StBichsel"); 
    LOAD_LIB("StMuDSTMaker"); 
    LOAD_LIB("StDaqLib");
    LOAD_LIB("StDbLib");
    LOAD_LIB("StDbBroker");
    LOAD_LIB("St_db_Maker");
    LOAD_LIB("libglobal_Tables");
    LOAD_LIB("libsim_Tables");
    LOAD_LIB("libgen_Tables");
    LOAD_LIB("libgeometry_Tables");
    LOAD_LIB("StDAQMaker"); 
    LOAD_LIB("StEmcSimulatorMaker");
    LOAD_LIB("StEmcRawMaker");
    LOAD_LIB("StEmcADCtoEMaker");
    LOAD_LIB("StEmcMixerMaker");
    LOAD_LIB("StEEmcUtil");
    LOAD_LIB("StPreEclMaker");
    LOAD_LIB("StEpcMaker");
    LOAD_LIB("StEmcTriggerMaker");
    LOAD_LIB("StTriggerUtilities");
    LOAD_LIB("StEEmcDbMaker");
    LOAD_LIB("StSpinDbMaker");
    LOAD_LIB("StJetFinder");
    LOAD_LIB("StJetSkimEvent");
    LOAD_LIB("StMCAsymMaker");
    LOAD_LIB("StJets");
    LOAD_LIB("StJetMaker");

    LOAD_LIB("StTimeRandomizerMaker");

    LOAD_LIB("StPi0DataMaker");

    Info(__FILE__, "Finished loading libraries");
}
