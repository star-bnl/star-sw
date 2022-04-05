// $Id: LoadLibs.C,v 1.3 2008/12/29 16:12:49 kocolosk Exp $

/*****************************************************************************
 * @author Adam Kocoloski
 *
 * Libraries needed to run a chain containing StChargedPionMaker
 *****************************************************************************/

void LoadLibs() { 
    gSystem->Load("libPhysics");
    gSystem->Load("libTable");
    gSystem->Load("StarRoot");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StTriggerDataMaker");
    gSystem->Load("StBichsel");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StTofUtil");
    gSystem->Load("StPmdUtil");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StJets");
    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StEmcSimulatorMaker");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StDbBroker");
    gSystem->Load("StTriggerFilterMaker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");
    gSystem->Load("StSpinTree");
    gSystem->Load("StMiniMcEvent");
    gSystem->Load("StChargedPionAnalysisMaker");
}

/*****************************************************************************
 * $Log: LoadLibs.C,v $
 * Revision 1.3  2008/12/29 16:12:49  kocolosk
 * added $Id$/$Log$ as needed
 *
 *****************************************************************************/
