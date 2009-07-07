void GeoTestLoad()
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
