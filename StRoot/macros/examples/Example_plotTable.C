// $Id: Example_plotTable.C,v 1.11 2006/08/15 21:42:51 jeromel Exp $
// $Log: Example_plotTable.C,v $
// Revision 1.11  2006/08/15 21:42:51  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.10  2000/05/09 20:15:42  kathy
// transfer obsolete macros to /macros/obsolete;  update other macros so that they use standard default inputs plus only few events by default so they'll be easy to run in autoQA macro testing
//
// Revision 1.9  2000/04/18 20:37:24  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.8  2000/04/12 16:13:39  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.7  2000/01/20 17:03:26  genevb
// Need to cast reference pointer now???
//
// Revision 1.6  2000/01/06 19:35:48  kathy
// change to use available xdf file as input
//
// Revision 1.5  1999/10/07 05:00:29  genevb
// Update to work properly with library changes and file changes
//
// Revision 1.4  1999/06/22 20:38:48  genevb
// Changed default table
//
// Revision 1.3  1999/06/07 17:31:22  kathy
// clean up some macros
//
// Revision 1.2  1999/05/21 15:33:47  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Gene Van Buren
// what it does: read xdf file, creates ntuple, draws histogram of value
//=======================================================================

{
gSystem->Load("St_base");
gSystem->Load("St_Tables");
gSystem->Load("xdf2root");
gSystem->Load("StUtilities");
gSystem->Load("StAnalysisUtilities");
St_TableNtuple myNtuple((TTable&) St_dst_v0_vertex());
myNtuple.AddXDFFile("/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
"dst","dst_v0_vertex");
myNtuple.Draw("pos_py:pos_px");
}







