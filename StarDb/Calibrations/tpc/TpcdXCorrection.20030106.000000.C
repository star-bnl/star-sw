TDataSet *CreateTable() { // correction vs log2(dX) 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrection",2);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =            3;// Outer dXdEGPHist536P03if
  row.a[0]	 =  1.31409e-01;//
  row.a[1]	 = -2.10402e-01;//
  row.a[2]	 =  7.89447e-02;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar       =            5;// Inner dXdEGPHist536P03if
  row.a[0]	 =  1.23657e-01;// 
  row.a[1]	 = -4.37540e-01;// 
  row.a[2]	 =  5.39642e-01;// 
  row.a[3]	 = -1.73519e-01;// 
  row.a[4]	 =  2.17271e-03;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
