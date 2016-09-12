TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.08417,  -0.07068,  -0.06242,  -0.05641,  -0.04766,
      -0.04227,  -0.05302,  -0.04162,  -0.03642,  -0.03386,
      -0.03655,  -0.04134,  -0.04387,   0.09434,   0.09928,
       0.09534,   0.09451,   0.09545,   0.09589,   0.09159,
       0.09105,   0.09326,   0.09185,   0.08517,   0.08096,
       0.07853,   0.07877,   0.07025,   0.06725,   0.06749,
       0.06551,   0.05181,   0.05535,   0.05090,   0.04918,
       0.03811,   0.02394,   0.01830,   0.00847,   0.01760,
       0.01542,   0.00945,   0.00276,  -0.01080,  -0.03150
  };
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcRowQ",45);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.nrows = 45;
  row.npar  =  1;
  for (Int_t i = 0; i < 45; i++) {
    row.idx  = i + 1;
    row.a[0] = rowsGain[i];
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
