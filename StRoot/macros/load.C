// $Id: load.C,v 1.3 1999/05/21 15:33:59 kathy Exp $
// $Log: load.C,v $
// Revision 1.3  1999/05/21 15:33:59  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================
{
   gSystem->Load("St_base.so");
   gSystem->Load("xdf2root.so");
   gSystem->Load("St_Tables.so");
   gSystem->Load("libmsg.so");
   gSystem->Load("libtls.so");
   gSystem->Load("tpc.sl");
   gSystem->Load("St_tpc.so");
   gSystem->Load("StChain.so");
   gSystem->Load("St_xdfin_Maker.so");
   gSystem->Load("St_tcl_Maker.so");
   gSystem->Load("St_tpctest_Maker.so");
   gSystem->Load("St_tss_Maker.so");
   gSystem->Load("St_run_Maker.so");
}
