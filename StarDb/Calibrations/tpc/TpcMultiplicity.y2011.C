TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcMultiplicity",2);
memset(&row,0,tableSet->GetRowSize()); 
 row.npar        =      1;       // Outer  MultiplicityPOGPHist262P02gh1
 row.a[0]	 =  3.82091e-03;// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
 row.npar        =      3;       // Inner   MultiplicityPIGPHist262P02gh1
 row.a[0]	 =  3.39280e-02;//   2.80818e-03 
 row.a[1]	 = -3.85958e-03;//   3.89317e-03
 row.a[2]	 = -5.93156e-03;//   9.87724e-04
 tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
