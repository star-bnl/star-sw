TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55403; // +/- 8.80928e-06 cm/us All: East = 0.0801686 +/- 0.00202134
  row.laserDriftVelocityWest	 =   5.55403; // +/- 8.80928e-06 cm/us All: West = 0.342045 +/- 0.00253756
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55403 +/- 8.80928e-06
  return (TDataSet *)tableSet;// West = 5.55317 +/- 1.41919e-05 East = 5.55457 +/- 1.12359e-05
};
