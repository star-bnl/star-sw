St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/fclpars/zrow Allocated rows: 20  Used rows: 20  Row size: 4 bytes
//  Table: fcl_zrow_st[0]--> fcl_zrow_st[19]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_fcl_zrow")) return 0;
fcl_zrow_st row;
St_fcl_zrow *tableSet = new St_fcl_zrow("zrow",20);
//
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     162.75; // z-position of padrow ;
tableSet->AddAt(&row,0);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     171.25; // z-position of padrow ;
tableSet->AddAt(&row,1);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     184.05; // z-position of padrow ;
tableSet->AddAt(&row,2);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     192.55; // z-position of padrow ;
tableSet->AddAt(&row,3);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     205.35; // z-position of padrow ;
tableSet->AddAt(&row,4);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     213.85; // z-position of padrow ;
tableSet->AddAt(&row,5);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     226.65; // z-position of padrow ;
tableSet->AddAt(&row,6);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     235.15; // z-position of padrow ;
tableSet->AddAt(&row,7);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     247.95; // z-position of padrow ;
tableSet->AddAt(&row,8);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =     256.45; // z-position of padrow ;
tableSet->AddAt(&row,9);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -162.75; // z-position of padrow ;
tableSet->AddAt(&row,10);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -171.25; // z-position of padrow ;
tableSet->AddAt(&row,11);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -184.05; // z-position of padrow ;
tableSet->AddAt(&row,12);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -192.55; // z-position of padrow ;
tableSet->AddAt(&row,13);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -205.35; // z-position of padrow ;
tableSet->AddAt(&row,14);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -213.85; // z-position of padrow ;
tableSet->AddAt(&row,15);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -226.65; // z-position of padrow ;
tableSet->AddAt(&row,16);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -235.15; // z-position of padrow ;
tableSet->AddAt(&row,17);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -247.95; // z-position of padrow ;
tableSet->AddAt(&row,18);
memset(&row,0,tableSet->GetRowSize());
    row.z	 =    -256.45; // z-position of padrow ;
tableSet->AddAt(&row,19);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
