TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54937; // +/- 5.31169e-06 cm/us All: East = 0.117338 +/- 0.00198951
  row.laserDriftVelocityWest	 =   5.54937; // +/- 5.31169e-06 cm/us All: West = 0.154237 +/- 0.00106651
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54937 +/- 5.31169e-06
  return (TDataSet *)tableSet;// West = 5.54933 +/- 6.03703e-06 East = 5.5495 +/- 1.11767e-05
};
