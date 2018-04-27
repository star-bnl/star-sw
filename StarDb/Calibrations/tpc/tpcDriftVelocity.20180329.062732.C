TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 88006
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51773; // +/- 8.58649e-06 cm/us All: East = -0.628139 +/- 0.00489665
  row.laserDriftVelocityWest	 =   5.51773; // +/- 8.58649e-06 cm/us All: West = 0.168579 +/- 0.00162957
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51773 +/- 8.58649e-06
  return (TDataSet *)tableSet;// West = 5.51733 +/- 9.01476e-06 East = 5.52171 +/- 2.8193e-05
};
