// $Id: Load.C,v 1.9 2000/03/27 02:57:12 fine Exp $
// $Log: Load.C,v $
// Revision 1.9  2000/03/27 02:57:12  fine
// ROOT 2.24 needs libSTAR to be loaded
//
// Revision 1.8  1999/11/05 16:19:50  fisyak
// Add StUtilities
//
// Revision 1.7  1999/09/09 13:52:38  fisyak
// Add StBFChain
//
// Revision 1.6  1999/05/21 15:33:50  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(){
    gSystem->Load("libSTAR");
    gSystem->Load("Star2Root");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StBFChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
}
