void Draw3DDoc()
{
   gROOT->Macro("Load.C");
   THtml doc;
   doc.SetSourceDir(".:src:inc:StRoot:StRoot/StarRoot:StRoot/StEventUtilities");
   doc.MakeClass("StDraw3D");
   doc.MakeClass("StuDraw3DEvent");
}
