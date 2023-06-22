TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcLengthCorrectionMDN",6);
  Int_t nrows = 6;
  memset(&row,0,tableSet->GetRowSize());
  tableSet->AddAt(&row);// MuFit;	1	h2mdf("mu",5,1,20);
  tableSet->AddAt(&row);// SigmaFit;	2	h2mdf("sigma",5,1,20);
  tableSet->AddAt(&row);// MuFit;	3	h2mdf("mu",5,1,20);
  tableSet->AddAt(&row);// SigmaFit;	4	h2mdf("sigma",5,1,20);
  tableSet->AddAt(&row);// MuFit;	5	h2mdf("mu",5,1,20);
  tableSet->AddAt(&row);// SigmaFit;	6	h2mdf("sigma",5,1,20);
  return (TDataSet *)tableSet;
}
