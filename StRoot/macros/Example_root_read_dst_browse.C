//========================================================================
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



