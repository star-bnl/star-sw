void Draw3D()
{
   gROOT->Macro("Load.C");
   gEventDisplay->Draw3DTest();
   gEventDisplay->Print("Draw3DTest.wrl");
}
