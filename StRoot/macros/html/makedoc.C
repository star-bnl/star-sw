{
//  gROOT.Reset();
  gSystem.MakeDirectory("Html");
//  gSystem.Load("St_Module.dll");
//  gSystem.Load("Root_html.dll");
  THtml *html = new THtml();
  html.MakeClass("St_Module");
  html.MakeClass("St_Table");
  html.MakeClass("St_DataSet");
  html.MakeClass("St_Event");
  html.MakeClass("St_TableIter");
  html.MakeClass("St_DataSetIter");
  html.MakeClass("St_XDFFile");
//  html.MakeClass("EModuleTypes");
  html.MakeIndex();
}
