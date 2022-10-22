/*
   ls -1l tpx/tpc_gains.* | awk '{print "\""$9"\",\t\""$11"\""}'
  StarDb/Calibrations/tpc $ ls -1l tpx/tpc_gains.* | awk '{print "root.exe \'MakePath2tpxGain.C(\""$9"\",\t\""$11"\")\'"}'
 */
void MakePath2tpxGain(TString tpcCVS, TString tpxRTS) {
  Int_t d, t;
  Int_t n = sscanf(tpcCVS,"tpx/tpc_gains.txt.%8i.%06i",tpcCVS.Data());
  if (n != 2) {
    cout << "Line: " << tpcCVS.Data() << " cannot be read" << endl;
    return;
  }
  TDatime t(d,t);
  UInt_t  u = t.Convert(kTRUE);
  TDatime gmt(u);
  cout << t.AsString() << " ==> " gmt.AsString() << endl;
  TString Line = tpcCVS;
  if (tpxRTS != "") {
    Line = "tpx/"; Line += tpxRTS;
  }
  Line += "\0";
  TString fOut(Form("Path2tpxGain.%8i.%06i.C",gmt.GetDate(),gmt.GetTime()));
  ofstream out;
  cout << "Create " << fOut.Data() << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcCorrection\")) return 0;" << endl;
  out << "  St_FilePath *tableSet = new St_FilePath(\"Path2tpxGain\",1);" << endl;
  out << "  FilePath_st row = \"" << Line.Data() << "\";" << endl; 
  out << "  tableSet->AddAt(&row);" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
  cout << "=================================<" << fOut.Data() << "===============================================" << endl;
} 
