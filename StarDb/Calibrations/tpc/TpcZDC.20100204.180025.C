TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZDC",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      =        nrows; //  Zdc3CGFRunX25P10i_AuAu200_production_ReversedFullField.root +
  row.idx        =            1; //  Zdc3CGFRunX25P10i_LowLuminosity_2010_ReversedFullField.root
  row.npar       =            3; //  chain->Draw("mu:y>>AuAu200RFO(15,2,5)","(i&&j&&i>13)/(dmu**2)","profw")
  row.min        =  2.2;
  row.max        =  4.8;
  row.a[0]	 =  3.24310e-02;//
  row.a[1]	 = -1.48728e-02;//
  row.a[2]	 =  1.68749e-03;//
  tableSet->AddAt(&row); //Outer
  memset(&row,0,tableSet->GetRowSize());
  row.nrows      =        nrows; //  Zdc3CGFRunX25P10i_AuAu200_production_ReversedFullField.root +
  row.idx        =            2; //  Zdc3CGFRunX25P10i_LowLuminosity_2010_ReversedFullField.root
  row.npar       =            5; // chain->Draw("mu:y>>AuAu200RFI(15,2,5)","(i&&j&&i<=13)/(dmu**2)","profw")
  row.min        =  2.2;
  row.max        =  4.8;
  row.a[0]	 = -6.83864e-02;//
  row.a[1]	 =  3.16842e-02;//
  row.a[2]	 =  7.12736e-02;//
  row.a[3]	 = -3.55314e-02;//
  row.a[4]	 =  4.21282e-03;//
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
