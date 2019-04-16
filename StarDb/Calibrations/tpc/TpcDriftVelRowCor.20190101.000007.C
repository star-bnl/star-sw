TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcDriftVelRowCor",1);
  memset(&row,0,tableSet->GetRowSize()); // 0
  /*
Membrane:  ZLMFreqTpcHitZTMfl0.root
1. match Inner and Outer: 
FitP->Draw("(mu-208.707):y>>Z(72,0.5,72.5)","i&&j","prof")
Z->Fit("pol2","er","",0.5,40.5)
pol2->Eval(40.5);// => -1.42279472751408842e-01
Z->Fit("pol2","er","",40.5,72.5);
pol2->Eval(40.5);// => -4.34622712046779991e-01
FitP->Draw("(mu-208.707)-(-1.42279472751408842e-01):y>>ZI(72,0.5,72.5)","i&&j&&j<=40","prof")
FitP->Draw("(mu-208.707)-(-4.34622712046779991e-01):y>>ZO(72,0.5,72.5)","i&&j&&j>40","prof")
TProfile *z = new TProfile(*ZI)
z->Add(ZO)

FitP->Draw("((mu-208.707)-(-1.42279472751408842e-01))/208.707:y>>zI(72,0.5,72.5)","i&&j&&j<=40","prof")
FitP->Draw("((mu-208.707)-(-4.34622712046779991e-01))/208.07:y>>zO(72,0.5,72.5)","i&&j&&j>40","prof")
TProfile *z = new TProfile(*zI)
z->Add(zO)
z->Fit("pol2","e")
 */
    row.idx	 =            1; // ;
    row.nrows	 =            1; // ;
    row.npar	 =            3; // ;
    row.a[0]	 = -3.52474e-04; // ;
    row.a[1]	 =  1.57375e-05;
    row.a[2]	 = -1.70223e-07;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
