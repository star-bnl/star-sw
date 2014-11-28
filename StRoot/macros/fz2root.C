// Conver Zebra file into ROOT format
void fz2root(UInt_t nEvent=99999, const Char_t *file=
	     "/star/rcf/simu/auau200/hijing/b0_20/inverse/year2001/hadronic_on/gstardata/rcf0191_01_380evts.fzd"
	     ) {
   gROOT->LoadMacro("bfc.C");
   bfc(0,"fzin, GeantOut, nodefault,-magf,-db,noHistos,noRunco",file);
   chain->SetDEBUG(0);
   chain->EventLoop(nEvent);
   chain->Finish();
}
