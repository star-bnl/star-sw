TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/sst/.sstDimensions/sstDimensions Allocated rows: 1  Used rows: 1  Row size: 68 bytes
//  Table: sstDimensions_st[0]--> sstDimensions_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_sstDimensions")) return 0;
sstDimensions_st row;
St_sstDimensions *tableSet = new St_sstDimensions("sstDimensions",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.wafersPerLadder	 =         16; // 16         ;
    row.a128PerSide	 =          6; // 6          ;
    row.stripPerSide	 =        768; // 768        ;
    row.stripPitch	 =     0.0095; // 0.0095 cm  ;
    row.stereoAngle	 =     0.0175; // 0.0175 rad ;
    row.waferHalfLength	 =       3.75; // 3.75 cm    ;
    row.waferHalfWidth	 =        2.1; // 2.1  cm    ;
    row.waferHalfThickness	 =      0.015; // 0.015 cm   ;
    row.waferHalfActLength	 =       3.65; // 3.65 cm    ;
    row.waferHalfActWidth	 =          2; // 2. cm      ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
