TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52877; // +/- 7.54139e-06 cm/us All: East = 2.40025 +/- 0.00797366
  row.laserDriftVelocityWest	 =   5.52877; // +/- 7.54139e-06 cm/us All: West = 2.37872 +/- 0.00137184
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52877 +/- 7.54139e-06
  return (TDataSet *)tableSet;// West = 5.52881 +/- 7.64196e-06 East = 5.52726 +/- 4.66367e-05
};
