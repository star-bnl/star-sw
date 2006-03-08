TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// SsdSectorsOnGlobal Allocated rows: 4  Used rows: 4  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[3]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_Survey")) return 0;
#if 0
  Survey_st row;
  St_Survey *tableSet = new St_Survey("SsdSectorsOnGlobal",4);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          1; // ;
    row.r00	 =   0.999838; // ;
    row.r01	 =      0.018; // ;
    row.r02	 =   0.000888; // ;
    row.r10	 =     -0.018; // ;
    row.r11	 =   0.999838; // ;
    row.r12	 =  -0.000461; // ;
    row.r20	 =  -0.000896; // ;
    row.r21	 =   0.000445; // ;
    row.r22	 =          1; // ;
    row.t0	 =    -0.3993; // ;
    row.t1	 =     0.0217; // ;
    row.t2	 =      -0.09; // ;
    row.sigmaRotX	 =        0.1; // ;
    row.sigmaRotY	 =        0.1; // ;
    row.sigmaRotZ	 =        0.1; // ;
    row.sigmaTrX	 =        0.1; // ;
    row.sigmaTrY	 =        0.1; // ;
    row.sigmaTrZ	 =        0.1; // ;
 memcpy(&row.comment,"\x20\x20\x20Nominal\x20errors\x20-\x20top\x20sector\x00\x01",32);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          2; // ;
    row.r00	 =   0.999838; // ;
    row.r01	 =      0.018; // ;
    row.r02	 =   0.000888; // ;
    row.r10	 =     -0.018; // ;
    row.r11	 =   0.999838; // ;
    row.r12	 =  -0.000461; // ;
    row.r20	 =  -0.000896; // ;
    row.r21	 =   0.000445; // ;
    row.r22	 =          1; // ;
    row.t0	 =    -0.3993; // ;
    row.t1	 =     0.0217; // ;
    row.t2	 =      -0.09; // ;
    row.sigmaRotX	 =        0.1; // ;
    row.sigmaRotY	 =        0.1; // ;
    row.sigmaRotZ	 =        0.1; // ;
    row.sigmaTrX	 =        0.1; // ;
    row.sigmaTrY	 =        0.1; // ;
    row.sigmaTrZ	 =        0.1; // ;
 memcpy(&row.comment,"\x20Nominal\x20errors\x20-\x20right\x20sector",30);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          3; // ;
    row.r00	 =  0.9998357; // ;
    row.r01	 =      0.018; // ;
    row.r02	 = 0.002387756; // ;
    row.r10	 = -0.01799929; // ;
    row.r11	 =   0.999838; // ;
    row.r12	 = -0.0004879995; // ;
    row.r20	 = -0.002395999; // ;
    row.r21	 =   0.000445; // ;
    row.r22	 =  0.9999977; // ;
    row.t0	 =    -0.3993; // ;
    row.t1	 =     0.0217; // ;
    row.t2	 =      -0.09; // ;
    row.sigmaRotX	 =        0.1; // ;
    row.sigmaRotY	 =        0.1; // ;
    row.sigmaRotZ	 =        0.1; // ;
    row.sigmaTrX	 =        0.1; // ;
    row.sigmaTrY	 =        0.1; // ;
    row.sigmaTrZ	 =        0.1; // ;
 memcpy(&row.comment,"Nominal\x20errors\x20-\x20bottom\x20sector",30);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          4; // ;
    row.r00	 =   0.999838; // ;
    row.r01	 =      0.018; // ;
    row.r02	 =   0.000888; // ;
    row.r10	 =     -0.018; // ;
    row.r11	 =   0.999838; // ;
    row.r12	 =  -0.000461; // ;
    row.r20	 =  -0.000896; // ;
    row.r21	 =   0.000445; // ;
    row.r22	 =          1; // ;
    row.t0	 =    -0.3993; // ;
    row.t1	 =     0.0217; // ;
    row.t2	 =      -0.09; // ;
    row.sigmaRotX	 =        0.1; // ;
    row.sigmaRotY	 =        0.1; // ;
    row.sigmaRotZ	 =        0.1; // ;
    row.sigmaTrX	 =        0.1; // ;
    row.sigmaTrY	 =        0.1; // ;
    row.sigmaTrZ	 =        0.1; // ;
    memcpy(&row.comment,"\x20\x20Nominal\x20errors\x20-\x20left\x20sector",30);// 
    tableSet->AddAt(&row);
#else
    Survey_st row[4] = {
      {1,.999838 ,.018,.000888,-.018,.999838,-.000461,-.000896,.000445,      1,-.3993,.0217,-.09,.1,.1,.1,.1,.1,.1,"top sector"},
      {2,.999838 ,.018,.000888,-.018,.999838,-.000461,-.000896,.000445,      1,-.3993,.0217,-.09,.1,.1,.1,.1,.1,.1,"right sector"},
      {3,.9998357,.018,.002388,-.018,.999838,-.000488,-.002396,.000445,.999998,-.3993,.0217,-.09,.1,.1,.1,.1,.1,.1,"bottom sector"},
      {4,.999838 ,.018,.000888,-.018,.999838,-.000461,-.000896,.000445,      1,-.3993,.0217,-.09,.1,.1,.1,.1,.1,.1,"left sector"}
    };
    St_Survey *tableSet = new St_Survey("SsdSectorsOnGlobal",4);
    for (Int_t i = 0; i < 4; i++) tableSet->AddAt(&row[i].Id, i);
#endif
    // ----------------- end of code ---------------
    return (TDataSet *)tableSet;
}
