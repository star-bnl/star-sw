// $Id: Example_plotTable.C,v 1.2 1999/05/21 15:33:47 kathy Exp $
// $Log: Example_plotTable.C,v $
// Revision 1.2  1999/05/21 15:33:47  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Gene Van Buren
// what it does: 
//=======================================================================

{
gSystem->Load("St_base");
gSystem->Load("St_Tables");
gSystem->Load("xdf2root");
St_ev0_aux temp;
St_TableNtuple myNtuple(temp);
myNtuple.AddXDFFile("/disk00000/star/mdc1_test_data/mdc1_year2a_psc079_01_46evts_dst.xdf","dst","ev0out");
myNtuple.Draw("theta");
}
