TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrection",2);
memset(&row,0,tableSet->GetRowSize()); 
 row.npar        =            2;// default scale for Year 1
 row.a[0]	 =           0.;// 
 row.a[1]	 =           1.;//1.6716e+00;// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
 row.npar        =            2;// default scale for Year 1
 row.a[0]	 =           0.;// 
 row.a[1]	 =           1.;//1.6716e+00;// 
 tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
