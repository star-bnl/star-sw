// $Id: Example_read_dst_browse.C,v 1.2 1999/06/07 21:10:36 kathy Exp $
// $Log: Example_read_dst_browse.C,v $
// Revision 1.2  1999/06/07 21:10:36  kathy
// fixing up macros - removed or renamed some, fixed others so the default input file is there
//
// Revision 1.1  1999/06/07 17:31:23  kathy
// clean up some macros
//
// Revision 1.2  1999/05/21 15:33:49  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does:  see below
//=======================================================================
//  Example_read_dst_browse.C
//
// Kathy (6/7/99):
//  This is an example showing how to read in a ROOT DST file
//  (created using bfc.C) and then pop a Browser to look at it.
//   - the Browser reads the WHOLE file for you.
//   - click on ROOT in Browser
//   - you can't really do much else here because you haven't loaded
//     the tree_Maker and set up a chain
//
//===============================================================

{

gSystem->Load("St_base");
gSystem->Load("St_Tables");

TFile *root_file=0;
root_file  =  new TFile("/disk00000/star/test/new/tfs_Solaris/year_2a/psc0210_01_40evts.dst.root","read");
root_file.ls();
root_file.Dump();
TBrowser browser1;

}

