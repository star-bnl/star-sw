// $Id: Load.C,v 1.15 2000/07/31 21:40:36 fisyak Exp $
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
    gSystem->Load("StEvent");
    gSystem->Load("StBFChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
}
