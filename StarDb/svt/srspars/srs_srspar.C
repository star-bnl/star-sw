St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/srspars/srs_srspar Allocated rows: 1  Used rows: 1  Row size: 84 bytes
//  Table: srs_srspar_st[0]--> srs_srspar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_srs_srspar")) return 0;
srs_srspar_st row;
St_srs_srspar *tableSet = new St_srs_srspar("srs_srspar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.id_active[0]	 =          1; // id for active area on each layer ;
    row.id_active[1]	 =          1;
    row.id_active[2]	 =          1;
    row.id_active[3]	 =          1;
    row.id_active[4]	 =          1;
    row.id_active[5]	 =          1;
    row.id_active[6]	 =          4;
    row.id_active[7]	 =          0;
    row.id_active[8]	 =          0;
    row.id_active[9]	 =          0;
    row.init_ran	 =     111111; // if not 0 init seed with this each event ;
    row.merge	 =          0; // merge=1 when merging enabled ;
    row.method	 =          3; // how to copy hits to spt table ;
    row.nsca	 =          0; // number of sca channels ;
    row.bucket	 =          0; // space extent of time bucket ;
    row.enc	 =          0; // equivalent noise charge per bucket ;
    row.fsca	 =          25000000; // sca frequency ;
    row.pitch	 =          0.025; // distance between anodes (cm) ;
    row.shaper	 =          0; // gaussian responce of electronics (s) ;
    row.thickness	 =          0; // SVT wafer thickness ;
    row.vd	 =          675000; // drift velocity ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
