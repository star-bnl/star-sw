St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ttepars/tte_control Allocated rows: 1  Used rows: 1  Row size: 12 bytes
//  Table: tte_control_st[0]--> tte_control_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tte_control")) return 0;
tte_control_st row;
St_tte_control *tableSet = new St_tte_control("tte_control",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.fit	 =          1; // turn onn/off re-fitting...... ;
    row.pair	 =          0; // Turn on/off evaluation of fragments ;
    row.res	 =          0; // Turn on/off calculation of the residuals ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
