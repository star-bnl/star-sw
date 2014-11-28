TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Geometry/ftpc/.ftpcInnerCathode/ftpcInnerCathode Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: ftpcInnerCathode_st[0]--> ftpcInnerCathode_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_ftpcInnerCathode")) return 0;
ftpcInnerCathode_st row;
St_ftpcInnerCathode *tableSet = new St_ftpcInnerCathode("ftpcInnerCathode",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.offsetCathodeWest	 =     -0.035; // inner cathode offset in cm west ;
    row.offsetCathodeEast	 =      -0.06; // inner cathode offset in cm east ;
    row.angleOffsetWest	 =          0; // angle offset of inner cathode displ. in radians west ;
    row.angleOffsetEast	 =     0.2628; // angle offset of inner cathode displ. in radians east ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
