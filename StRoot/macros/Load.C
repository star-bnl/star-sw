// $Id: Load.C,v 1.17 2003/04/30 20:40:02 perev Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(){
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTriggerDataMaker");
    gSystem->Load("StEvent");
    gSystem->Load("StBFChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
}
