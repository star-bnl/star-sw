TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54975; // +/- 6.21254e-06 cm/us All: East = 0.120709 +/- 0.00281917
  row.laserDriftVelocityWest	 =   5.54975; // +/- 6.21254e-06 cm/us All: West = 0.128011 +/- 0.00120161
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54975 +/- 6.21254e-06
  return (TDataSet *)tableSet;// West = 5.54976 +/- 6.75127e-06 East = 5.54968 +/- 1.5871e-05
};
