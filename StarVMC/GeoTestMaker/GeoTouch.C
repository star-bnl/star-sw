int loaded=0;

void Load();
class StTGeoHelper;
StTGeoHelper *geoHelp=0;

void GeoTouch(const char *gy="y2009")
{
if (!loaded) Load();
  TString ts("$STAR/StarDb/VmcGeometry/"); ts +=gy; ts +=".h";
  gROOT->Macro(ts);
  geoHelp = StTGeoHelper::Instance();
}
void Show(double z,double rxy) 
{
 geoHelp->ShootZR(z,rxy);
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
