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
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =          1; // sector  ;
    row.mask	 =      16191; // enable mask  ; disable S01-R7-8 & S02-R7-8
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =          3; // sector  ;
    row.mask	 =      32767; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =          5; // sector  ;
    row.mask	 =      65535; // enable mask  ; disable 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ; 
    row.sector	 =          7; // sector  ;
    row.mask	 =      65535; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =          9; // sector  ;
    row.mask	 =      63487; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =         11; // sector  ;
    row.mask	 =      65535; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =         13; // sector  ;
    row.mask	 =        239; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =         15; // sector  ;
    row.mask	 =      65535; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =         17; // sector  ;
    row.mask	 =      65535; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =         19; // sector  ;
    row.mask	 =      64511; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =         21; // sector  ;
    row.mask	 =      65023; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21243008; // run number  ;
    row.sector	 =         23; // sector  ;
    row.mask	 =      65535; // enable mask  ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
