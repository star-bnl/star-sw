int loaded=0;

void Load();
void GeoTestChain(const char *gy="y2009")
{
if (!loaded) Load();


StChain *chain =  new StChain;
TString tsy(gy);
gSystem->ExpandPathName(tsy);
StMaker *mk = new GeoTestMaker("GeoTest",tsy.Data(), 1000000);
// StMaker *mk = new GeoTestMaker("GeoTest",tsy.Data(),1);
// mk->SetAttr("EtaMin",0.50);
// mk->SetAttr("EtaMax",0.51);
// mk->SetAttr("SteppingDebug",1);

 chain->Init();
 chain->EventLoop(1);
 chain->Finish();
 
}

void Load()
{
TH1D h1d; h1d.GetPainter();
gSystem->Load("libVMC.so");
gSystem->Load("St_base.so");
gSystem->Load("St_Tables.so");
gSystem->Load("StUtilities.so");
gSystem->Load("StChain.so");
gSystem->Load("StarVMCApplication.so");
gSystem->Load("libStarMiniCern.so");
gSystem->Load("libgeant3.so");
gSystem->Load("GeoTestMaker.so");
loaded=1;

}
