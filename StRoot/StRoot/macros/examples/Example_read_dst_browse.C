// $Id: Example_read_dst_browse.C,v 1.8 2006/08/15 21:42:53 jeromel Exp $
// $Log: Example_read_dst_browse.C,v $
// Revision 1.8  2006/08/15 21:42:53  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.7  2000/06/14 19:21:40  kathy
// use MainFile input
//
// Revision 1.6  2000/04/13 21:46:20  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.5  2000/04/12 16:13:40  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.4  2000/01/19 15:46:04  kathy
// change default input files to point to ones in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.3  1999/10/07 14:13:10  kathy
// changes to Example macros to make them work in dev - mostly changes were correcting input file name
//
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
Char_t *MainFile="/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root";

gSystem->Load("St_base");

gSystem->Load("libglobal_Tables");
gSystem->Load("libgen_Tables");
gSystem->Load("libsim_Tables");


TFile *root_file=0;
root_file  =  new TFile(MainFile,"read");
root_file.ls();
root_file.Dump();
TBrowser browser1;

}

