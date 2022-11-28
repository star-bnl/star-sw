#include "tables/St_tpcCorrection_Table.h"

TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 1;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcTimeDependence",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 
  /* TimeCGFRunXVIII13.root; 
     FitP->Draw("mu:x>>T(360,762e6,771e6)","(i&&j&&dmu>0&&dmu<3e-4&&mu<0.06)/dmu**2","profg")
     Int_t bin1, bin2; Double_t min, max;
     bin1 = 30
     bin2 = 36
     TDatime t; t.Set(T->GetXaxis()->GetBinLowEdge(bin1)+788936400); t.Print()
     cout << Form("tpcTimeDependence.%8i.%6i.C",t.GetDate(),t.GetTime()) << endl;
     min = T->GetXaxis()->GetBinLowEdge(bin1)
     max = T->GetXaxis()->GetBinUpEdge(bin2)
     T->Fit("pol1","er","",min,max)
     cout << 
   */
  row.idx        = 1;    
  row.nrows      = nrows;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
