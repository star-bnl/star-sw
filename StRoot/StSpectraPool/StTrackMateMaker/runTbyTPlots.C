#if 0
foreach f (`ls -1d  */*/Plots.root`)
  set d = `dirname ${f}`;
  cd ${d};
  ln -s ~/macrros/.sl* .
  root.exe -q Plots.root runTbyTPlots.C
  cd -
end
#endif
void runTbyTPlots() {
  gROOT->LoadMacro("TbyTPlots.C+");
  Init();
  Draw();
}
