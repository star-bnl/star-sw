TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// TpcTimeGain Allocated rows: 1  Used rows: 1  Row size: 8 bytes
//  Table: TpcTimeGain_st[0]--> TpcTimeGain_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcTimeGain")) return 0;
TpcTimeGain_st row;
St_TpcTimeGain *tableSet = new St_TpcTimeGain("TpcTimeGain",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.ScaleFactor	 =   1.159649; // ;
    row.ErrorScaleFactor	 = 0.0007634182; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
