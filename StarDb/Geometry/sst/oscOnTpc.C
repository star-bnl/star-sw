TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/sst/.oscOnTpc/oscOnTpc Allocated rows: 1  Used rows: 1  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("oscOnTpc",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          1; // ;
    row.r00	 =          1; // ;
    row.r11	 =          1;
    row.r22	 =          1;
    memcpy(&row.comment,"Ideal",6);// 
    tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
