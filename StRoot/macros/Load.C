// $Id: Load.C,v 1.16 2003/04/26 03:36:24 jeromel Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(){
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
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
