TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdCharge",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.nrows      =         nrows;
  row.idx        =             1;
  row.npar       =            8;// dCharge3CGFHist938P04ij
  row.min        =          -.7;// versus log10(dE(keV))  
  row.max        =           4.;//                         
  row.a[0]	 = -1.86391e-01;//
  row.a[1]	 =  1.84754e-01;//
  row.a[2]	 =  1.00971e-01;//
  row.a[3]	 = -3.27362e-01;//
  row.a[4]	 =  2.59754e-01;//
  row.a[5]	 = -9.94755e-02;//
  row.a[6]	 =  1.89976e-02;//
  row.a[7]	 = -1.46278e-03;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.nrows      =        nrows;// 
  row.idx        =            2;
  row.npar       =            8;// dCharge3CGFHist938P04ij
  row.min        =          -.7;// versus log10(dE(keV))  
  row.max        =           4.;//                         
  row.a[0]	 =  1.07078e-02;//
  row.a[1]	 =  1.98412e-01;//
  row.a[2]	 = -3.78822e-02;//
  row.a[3]	 = -3.24504e-01;//
  row.a[4]	 =  3.92752e-01;//
  row.a[5]	 = -1.89501e-01;//
  row.a[6]	 =  4.12988e-02;//
  row.a[7]	 = -3.40089e-03;//
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
