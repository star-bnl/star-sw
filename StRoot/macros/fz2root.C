// Conver Zebra file into ROOT format
void fz2root(const char *file=
            "/star/rcf/simu/auau200/hijing/b0_20/inverse/year2001/hadronic_on/gstardata/rcf0191_01_380evts.fzd"
            , unsigned int nEvent=99999) 
{
   gROOT->LoadMacro("bfc.C");
   bfc(0,"fzin, GeantOut sdt20090312",file);
   chain->SetDEBUG(0);
   chain->EventLoop(nEvent);
   chain->Finish();
}
