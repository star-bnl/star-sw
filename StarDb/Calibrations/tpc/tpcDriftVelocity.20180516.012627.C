TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 135038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53738; // +/- 4.66958e-06 cm/us All: East = 0.285948 +/- 0.00190378
  row.laserDriftVelocityWest	 =   5.53738; // +/- 4.66958e-06 cm/us All: West = 0.177399 +/- 0.0009221
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53738 +/- 4.66958e-06
  return (TDataSet *)tableSet;// West = 5.53749 +/- 5.20668e-06 East = 5.53692 +/- 1.05564e-05
};
