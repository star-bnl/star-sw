/*
  ls -1l itpc/itpc_gains.* | awk '{print "\""$9"\",\t\""$11"\""}'
  StarDb/Calibrations/tpc $ ls -1l itpc/itpc_gains.txt.20[1-2]* | awk '{print "root.exe -q -b MakePath2itpcGain.C+\(\""$9"\",\t\""$11"\"\)"}' | tee itpc/MakePath2itpcGain.csh
 */
#include "TString.h"
#include "TDatime.h"
#include "Riostream.h"
#include <stdio.h>
void MakePath2itpcGain(TString itpcCVS, TString itpcRTS) {
  Int_t d, t;
  Int_t n = sscanf(itpcCVS.Data(),"itpc/itpc_gains.txt.%8i.%06i",&d,&t);
  if (n != 2) {
    cout << "Line: " << itpcCVS.Data() << " cannot be read" << endl;
    return;
  }
  TDatime time(d,t);
  UInt_t  u = time.Convert(kTRUE);
  TDatime gmt(u);
  cout << time.GetDate() << "." << time.GetTime() 
       << " ==> " 
       << gmt.GetDate() << "." << gmt.GetTime() << endl;
  TString Line = itpcCVS;
  if (itpcRTS != "") {
    Line = "itpc/"; Line += itpcRTS;
  }
  TString fOut(Form("Path2itpcGain.%8i.%06i.C",gmt.GetDate(),gmt.GetTime()));
  ofstream out;
  cout << "Create " << fOut.Data() << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_FilePath\")) return 0;" << endl;
  out << "  St_FilePath *tableSet = new St_FilePath(\"Path2itpcGain\",1);" << endl;
  out << "  FilePath_st row = {0};" << endl;
  out << "  strncpy(row.file,\"" <<  Line.Data() << "\"," <<  Line.Length() << ");" << endl; 
  out << "  tableSet->AddAt(&row);" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
  //  cout << "=================================<" << fOut.Data() << "===============================================" << endl;
} 
