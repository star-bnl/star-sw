int loaded=0;

void Load();
void GeoTestChain(const char *gy="y2009")
{
if (!loaded) Load();


StChain *chain =  new StChain;
TString tsy(gy);
gSystem->ExpandPathName(tsy);
 new GeoTestMaker("GeoTest",tsy.Data(),20000);
 chain->Init();
 chain->EventLoop(1);
 chain->Finish();
 
}

void Load()
{

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
