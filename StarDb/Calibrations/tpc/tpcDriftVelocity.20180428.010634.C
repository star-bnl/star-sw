TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 117032
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54032; // +/- 9.02471e-06 cm/us All: East = -0.630776 +/- 0.00637655
  row.laserDriftVelocityWest	 =   5.54032; // +/- 9.02471e-06 cm/us All: West = 0.361414 +/- 0.00165931
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54032 +/- 9.02471e-06
  return (TDataSet *)tableSet;// West = 5.53995 +/- 9.3338e-06 East = 5.54553 +/- 3.53614e-05
};
