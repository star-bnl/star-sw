void Weights(const Char_t *file = "Geometry.y2005g.C") {
  /*
    foreach f (`ls ../geom*.C`)
      root.exe -q -b Weights.C\(\"$f\"\)
    end
    foreach f (`ls ../Geom*.C`)
      root.exe -q -b Weights.C\(\"$f\"\)
    end
    foreach f (`ls Geom*.Weight`)
      set g = `echo $f | sed -e 's/G/g/' -e 's/Weight/C/'`
      root.exe -q -b Weights.C\(\"../$g\"\)
    end
   */
  if (! gGeoManager) {
    if ( gROOT->LoadMacro(file) ) return;
    CreateTable();
  }
  if ( gROOT->LoadMacro("TopWeight.C") ) return;
  TString cmd("TopWeight(); >> ");
  TString File(gSystem->BaseName(file));
  File.ReplaceAll(".C",".Weight");
  cmd += File;
  gInterpreter->ProcessLine(cmd);
}
