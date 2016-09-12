TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.03341,  -0.01763,  -0.00721,   0.00345,   0.01446,
       0.02278,   0.01540,   0.02891,   0.03858,   0.04114,
       0.03845,   0.03366,   0.03113,   0.10277,   0.10768,
       0.10360,   0.10292,   0.10367,   0.10415,   0.09979,
       0.09901,   0.10065,   0.09984,   0.09345,   0.08976,
       0.08770,   0.08846,   0.08010,   0.07754,   0.07846,
       0.07719,   0.06328,   0.06802,   0.06402,   0.06281,
       0.05162,   0.03739,   0.03217,   0.02278,   0.03240,
       0.03071,   0.02502,   0.01873,   0.00544,  -0.01535
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
