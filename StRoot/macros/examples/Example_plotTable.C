// $Id: Example_plotTable.C,v 1.4 1999/06/22 20:38:48 genevb Exp $
// $Log: Example_plotTable.C,v $
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
St_TableNtuple myNtuple(St_dst_v0_vertex());
myNtuple.AddXDFFile("/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf","dst","dst_v0_vertex");
myNtuple.Draw("pos_py:pos_px");
}
