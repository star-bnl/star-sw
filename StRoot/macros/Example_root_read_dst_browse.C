// $Id: Example_root_read_dst_browse.C,v 1.2 1999/05/21 15:33:49 kathy Exp $
// $Log: Example_root_read_dst_browse.C,v $
// Revision 1.2  1999/05/21 15:33:49  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does: 
//=======================================================================
//  Example_root_read_dst_browse.C
//
// Kathy (5/13/99):
//  This is an example showing how to read in a ROOT DST file
//  (created using bfc.C) and then pop a Browser to look at it.
//   - the Browser reads the file for you.
//   - you can't really do much else here because you haven't loaded
//     the tree_Maker and set up a chain
//
//===============================================================


gSystem->Load("St_base");
gSystem->Load("St_Tables");
TFile *root_file=0;
root_file  =  new TFile("/disk1/star/test/SL99d/tfs_Linux/Fri/set0020_01_50evts.dst.root","read");
root_file.ls();
root_file.Dump();
TBrowser browser1;



