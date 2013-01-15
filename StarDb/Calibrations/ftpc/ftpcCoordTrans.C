TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/ftpc/.ftpcCoordTrans/ftpcCoordTrans Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: ftpcCoordTrans_st[0]--> ftpcCoordTrans_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_ftpcCoordTrans")) return 0;
ftpcCoordTrans_st row;
St_ftpcCoordTrans *tableSet = new St_ftpcCoordTrans("ftpcCoordTrans",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.observedVertexOffsetX[0]	 =       -0.1; // [0] = east, [1] = west ;
    row.observedVertexOffsetX[1]	 =     -0.049;
    row.observedVertexOffsetY[0]	 =      0.273; // [0] = east, [1] = west ;
    row.observedVertexOffsetY[1]	 =          0;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
