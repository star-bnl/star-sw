{
gSystem->Load("St_base");
gSystem->Load("St_Tables");
gSystem->Load("xdf2root");
St_ev0_aux temp;
St_TableNtuple myNtuple(temp);
myNtuple.AddXDFFile("/disk00000/star/test_data/year2a_psc079_01_46evts_dst.xdf","dst","ev0out");
myNtuple.Draw("theta");
}
