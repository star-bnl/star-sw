St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ems/control_toadc Allocated rows: 1  Used rows: 1  Row size: 224 bytes
//  Table: control_toadc_st[0]--> control_toadc_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_control_toadc")) return 0;
control_toadc_st row;
St_control_toadc *tableSet = new St_control_toadc("control_toadc",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.mode[0]	 =          2; // =2, convert with accounting photo statistics ;
    row.mode[1]	 =          2;
    row.mode[2]	 =          1;
    row.mode[3]	 =          1;
    row.mode[4]	 =          0;
    row.mode[5]	 =          0;
    row.mode[6]	 =          0;
    row.mode[7]	 =          0;
    row.zero_sup[0]	 =          0; // =0 no zero suppression, =1 do zero suppresion ;
    row.zero_sup[1]	 =          0;
    row.zero_sup[2]	 =          0;
    row.zero_sup[3]	 =          0;
    row.zero_sup[4]	 =          0;
    row.zero_sup[5]	 =          0;
    row.zero_sup[6]	 =          0;
    row.zero_sup[7]	 =          0;
    row.energy[0]	 =         60; // Max energy in BEMC tower (~60 GeV/c);
    row.energy[1]	 =         60;
    row.energy[2]	 =         25;
    row.energy[3]	 =         25;
    row.energy[4]	 =        100;
    row.energy[5]	 =          0;
    row.energy[6]	 =          0;
    row.energy[7]	 =          0;
    row.dep_mip[0]	 =     0.0198; // MIP deposit energy in Sc.layers at zero angle ;
    row.dep_mip[1]	 =  0.0009645;
    row.dep_mip[2]	 =          0;
    row.dep_mip[3]	 =          0;
    row.dep_mip[4]	 =          0;
    row.dep_mip[5]	 =          0;
    row.dep_mip[6]	 =          0;
    row.dep_mip[7]	 =          0;
    row.nphe_mip[0]	 =         63; // The number of PHE for MIP at zero angle ;
    row.nphe_mip[1]	 =          3;
    row.nphe_mip[2]	 =          0;
    row.nphe_mip[3]	 =          0;
    row.nphe_mip[4]	 =          0;
    row.nphe_mip[5]	 =          0;
    row.nphe_mip[6]	 =          0;
    row.nphe_mip[7]	 =          0;
    row.c1[0]	 =          0; // ( = sampfrac*bemc_adcbits) / energy   ;
    row.c1[1]	 =          0;
    row.c1[2]	 =          0;
    row.c1[3]	 =          0;
    row.c1[4]	 =          0;
    row.c1[5]	 =          0;
    row.c1[6]	 =          0;
    row.c1[7]	 =          0;
    row.c2[0]	 =          0; // ( = nphe_mip) / dep_mip) ;
    row.c2[1]	 =          0;
    row.c2[2]	 =          0;
    row.c2[3]	 =          0;
    row.c2[4]	 =          0;
    row.c2[5]	 =          0;
    row.c2[6]	 =          0;
    row.c2[7]	 =          0;
    row.c3[0]	 =          0; // (sampfrac*bems_adcbits*dep_mip) / (energy*nphe_mip);
    row.c3[1]	 =          0;
    row.c3[2]	 =          0;
    row.c3[3]	 =          0;
    row.c3[4]	 =          0;
    row.c3[5]	 =          0;
    row.c3[6]	 =          0;
    row.c3[7]	 =          0;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
