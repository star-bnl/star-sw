TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrection",2);
  Int_t idx = 1;
  memset(&row,0,tableSet->GetRowSize()); 
  row.type       =  10;
  row.idx      =  idx;
  row.OffSet     =  2.09560e+01; // +/- 3.23953e-02
  row.npar       =            1; //SecRow3GFHist837P04ifAuAu200
  row.a[0]	 =  6.57541e-01;//6.03276e-01;//-1.24717e-02 5.90804e-01;
#if 1
  row.npar       =            8; //  log(x_sim/x_fcf) = log(1.+[0]/exp(x)) Outer->Fit("A","r","",3.8,9.8)
  row.a[0]	 = -6.42797e+01;//-6.49234e+01;  
  row.a[1]	 =  7.10178e+01;  
  row.a[2]	 = -3.26220e+01;  
  row.a[3]	 =  8.15578e+00;  
  row.a[4]	 = -1.19870e+00;  
  row.a[5]	 =  1.03617e-01;  
  row.a[6]	 = -4.88051e-03;  
  row.a[7]	 =  9.67008e-05;  
#endif
  tableSet->AddAt(&row,idx-1); 
  idx++;
  memset(&row,0,tableSet->GetRowSize()); 
  row.type       =  10;
  row.idx      =  idx;
  row.OffSet     =  3.28490e+01; //  +/- 4.37598e-02
  row.npar       =  1;           // SecRow3GFHist837P04ifAuAu200
  row.a[0]	 =  5.61766e-01;//1.42010e-02 5.75967e-01;
#if 1
  row.npar       =            7; // Inner->Fit("A","r","",4,9.8)
  row.a[0]	 =  4.25238e+01; //4.19653e+01;  
  row.a[1]	 = -3.78599e+01;  
  row.a[2]	 =  1.41435e+01;  
  row.a[3]	 = -2.79842e+00;  
  row.a[4]	 =  3.09112e-01;  
  row.a[5]	 = -1.80676e-02;  
  row.a[6]	 =  4.36527e-04;  
#endif
  tableSet->AddAt(&row,idx-1);
  idx++;
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
