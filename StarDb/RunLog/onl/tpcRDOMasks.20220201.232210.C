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
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =          1; // sector  ;
    row.mask	 =      65535; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =          3; // sector  ;
    row.mask	 =      65405; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =          5; // sector  ;
    row.mask	 =      65535; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =          7; // sector  ;
    row.mask	 =      65535 & ~(((1 <<  4) + ( 1 << 5)) << 8); // disable RDO 5 and ^ in sector 8
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =          9; // sector  ;
    row.mask	 =      63487; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =         11; // sector  ;
    row.mask	 =      65533; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =         13; // sector  ;
    row.mask	 =      65535; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =         15; // sector  ;
    row.mask	 =      65531; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =         17; // sector  ;
    row.mask	 =      65526; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =         19; // sector  ;
    row.mask	 =      64503; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =         21; // sector  ;
    row.mask	 =      28671; // enable mask  ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   23032059; // run number  ;
    row.sector	 =         23; // sector  ;
    row.mask	 =      63231; // enable mask  ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
