TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/RunLog/onl/.tpcRDOMasks/tpcRDOMasks Allocated rows: 12  Used rows: 12  Row size: 12 bytes
//  Table: tpcRDOMasks_st[0]--> tpcRDOMasks_st[11]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcRDOMasks")) return 0;
tpcRDOMasks_st row;
St_tpcRDOMasks *tableSet = new St_tpcRDOMasks("tpcRDOMasks",12);
//
 for (Int_t s = 1; s <= 24; s += 2) {
   memset(&row,0,tableSet->GetRowSize());
   row.runNumber	 =     0; // run number  ;
   row.sector	         =     s; // sector  ;
   row.mask	         = 65535; // enable mask  ;
   tableSet->AddAt(&row);
 }
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
