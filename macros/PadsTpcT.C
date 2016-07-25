void PadsTpcT(const Char_t *files="./TpcRS/*0.root", const Char_t *Out = "") {
  gROOT->LoadMacro("TpcT.C+");
  TpcTPads(files,Out);
  gROOT->LoadMacro("DrawList.C+");
  DrawPadsAll();
}
