void Draw3D()
{
   gROOT->Macro("Load.C");
   StDraw3DEvent *e = new StDraw3DEvent;
   e->Draw3DTest();
}
