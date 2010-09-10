TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZDC",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      =        nrows; // 
  row.idx        =            1; // Zdc3CGFRunX25P10i_AuAu200_production_FullField
  row.npar       =            6; // chain->Draw("mu:y>>AuAu200FFO(15,2,5)","(i&&j&&i>13&&mu)/(dmu**2)","profw")
  row.min        =  2.2;
  row.max        =  4.6;
  row.a[0]	 =  3.10454e+00;//
  row.a[1]	 = -4.41173e+00;//
  row.a[2]	 =  2.48907e+00;//
  row.a[3]	 = -6.94051e-01;//
  row.a[4]	 =  9.55312e-02;//
  row.a[5]	 = -5.19447e-03;//
  tableSet->AddAt(&row); //Outer
  memset(&row,0,tableSet->GetRowSize());
  row.nrows      =        nrows; // 
  row.idx        =            2; // Zdc3CGFRunX25P10i_AuAu200_production_FullField(LL)
  row.npar       =            6; // chain->Draw("mu:y>>AuAu200FFI(15,2.,5)","(i&&j&&i<=13&&mu>-0.04)/(dmu**2)","profw")
  row.min        =  2.2;
  row.max        =  4.6;
  row.a[0]	 =  1.57209e+01;//
  row.a[1]	 = -2.29938e+01;//
  row.a[2]	 =  1.33113e+01;//
  row.a[3]	 = -3.79512e+00;//
  row.a[4]	 =  5.32374e-01;//
  row.a[5]	 = -2.94158e-02;//
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
