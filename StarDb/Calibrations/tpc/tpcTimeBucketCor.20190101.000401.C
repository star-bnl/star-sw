TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcTimeBucketCor",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      =        nrows; // net/l404/data/fisyak/reco/2019/hijingAuAu200/StiCA/T0offset.root.T0.root
  row.idx        =            1; // BO_1->Fit("pol3","er","",3, 17)
  row.npar       =            4; // 
  row.min        =            3;
  row.max        =           17;
  row.a[0]       =  3.64055e-01; 
  row.a[1]       = -1.09063e-01;
  row.a[2]       =  1.05803e-02;
  row.a[3]       = -3.55103e-04;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx        =            2; // BI_1->Fit("pol1","er","",3, 18)
  row.npar       =            2; // 
  row.min        =            3;
  row.max        =           18;
  row.a[0]       =  2.79422e-01; 
  row.a[1]       = -2.61007e-02;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
