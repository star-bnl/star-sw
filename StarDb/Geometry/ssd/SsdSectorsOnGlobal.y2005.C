TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// SsdSectorsOnGlobal Allocated rows: 4  Used rows: 4  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[3]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_Survey")) return 0;
    Survey_st row[4] = {
      {1, 1,0,0, 0,1,0, 0,0,1, 0,0,0,.1,.1,.1,.1,.1,.1,"top sector"},
      {2, 1,0,0, 0,1,0, 0,0,1, 0,0,0,.1,.1,.1,.1,.1,.1,"right sector"},
      {3, 1,0,0, 0,1,0, 0,0,1, 0,0,0,.1,.1,.1,.1,.1,.1,"bottom sector"},
      {4, 1,0,0, 0,1,0, 0,0,1, 0,0,0,.1,.1,.1,.1,.1,.1,"left sector"}
    };
    St_Survey *tableSet = new St_Survey("SsdSectorsOnGlobal",4);
    for (Int_t i = 0; i < 4; i++) tableSet->AddAt(&row[i].Id, i);
    // ----------------- end of code ---------------
    return (TDataSet *)tableSet;
}
