TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrection",2);
memset(&row,0,tableSet->GetRowSize()); 
 row.npar        =      2;  // Outer Z3GPHist294P02gh1
 row.a[0]	 = -2.96483e-02;//   +/-5.97131e-04
 row.a[1]	 =  2.13057e-04;//   +/-4.23926e-06
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
 row.npar        =      2;  // Inner Z3GPHist294P02gh1
 row.a[0]	 =  1.61634e-02;//   +/-2.77915e-03 
 row.a[1]	 = -1.13152e-04;//   +/-1.68174e-05 
 tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
