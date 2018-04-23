TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54671; // +/- 1.23094e-05 cm/us All: East = 0.271373 +/- 0.0261241
  row.laserDriftVelocityWest	 =   5.54671; // +/- 1.23094e-05 cm/us All: West = 0.179363 +/- 0.00222763
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54671 +/- 1.23094e-05
  return (TDataSet *)tableSet;// West = 5.54672 +/- 1.24327e-05 East = 5.5461 +/- 8.76483e-05
};
