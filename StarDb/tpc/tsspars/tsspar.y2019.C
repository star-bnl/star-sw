St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tsspars/tsspar Allocated rows: 1  Used rows: 1  Row size: 240 bytes
//  Table: tss_tsspar_st[0]--> tss_tsspar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tss_tsspar")) return 0;
tss_tsspar_st row;
St_tss_tsspar *tableSet = new St_tss_tsspar("tsspar",1);
//
memset(&row,0,tableSet->GetRowSize());
 memcpy(&row.fileout,"\x04\x08o\xfc\x00",5);// output file for pixel data (none->table) 
    row.dynam	 =       1023; // adc dynamic range (adc counts; usu 1023) ;
    row.format	 =          0; // pixel data format ;
    row.max_itime	 =        512; // upper bound of time bucket ;
    row.max_pads	 =        184; // upper bound of pads ;
    row.max_row	 =         45; // upper bound of row (<=45) ;
    row.max_sect	 =         24; // upper bound of sector pair (<=12) ;
    row.min_itime	 =          0; // low bound of time bucket ;
    row.min_pads	 =          1; // lower bound of pads ;
    row.min_row	 =          1; // lower bound of row (>=1) ;
    row.min_sect	 =          1; // lower bound of sector pair (>=1) ;
    row.mode	 =          0; // mode of TPC simulation for diff. tests ;
    row.nele_laser	 =        300; // number of electrons from laser point;
    row.ngain	 =          1; // number of gain sampling (1-10) ;
    row.nseg	 =          1; // number of sub-segment within a G-vol ;
    row.ntime	 =        512; // number of time buckets (dimensionless) ;
    row.printout	 =        -10; // control the level of printout (0=no) ;
    row.tpc_half	 =          0; // half (1) or full (0) TPC volume ;
    row.reset	 =          1; // re-do setup: 0=no, anything else=yes ;
    row.ave_ion_pot	 =   2.85e-08; // Average Ion. Potential of a gas(Ar=26eV) ;
    row.bfield	 =          0; // magnetic field strength (Tesla) ;
    row.c_test	 =        0.1; // test capacitance value (pF) ;
    row.diff_long	 =      0.037; // long diff const of gas (cm/sqrt(cm)) ;
    row.diff_trans	 =    0.06336; // trans diff const of gas (cm/sqrt(cm)) ;
    row.gain_in	 =       3558; // 2503; gas gain:inner sector (dimensionless) ;
    row.gain_out	 =       1310; // 1315; gas gain:outer sector (dimensionless) ;
    row.prf_in	 =       0.25; // pad resp.func:inner sector (cm) ;
    row.prf_out	 =      0.395; // pad resp.func:outer sector (cm) ;
    row.sca_rms	 =        390; // SCA noise (random, not filtered) ;
    row.scale	 =        335; // number of electrons per ADC count (600) ;
    row.step_size	 =        0.5; // step size for subpadrow tracking ;
    row.tau	 =      0.055; // shaper resp. time const. (usec) ;
    row.threshold	 =          0.0; // adc threshold (adc counts but a real num ;
    row.time_offset	 =          0; // Wayne Betts time offsets ;
    row.v_test	 =          1; // voltage of test pulse (V) ;
    row.white_rms	 =        800; // rms of white noise (shaper filtered) ;
    row.wire_coupling_in	 =        0.533; // wire-to-pad coupling inner sector ;
    row.wire_coupling_out	 =        0.512; // wire-to-pad coupling outer sector ;
    row.x_laser	 =          0; // local x of laser point[cm] along row ;
    row.y_laser	 =      129.2; // local y of laser point[cm] across row ;
    row.z_laser	 =        100; // z drift length of pointlaser source(cm) ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
