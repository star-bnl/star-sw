St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/fsspars/fss_param Allocated rows: 1  Used rows: 1  Row size: 72 bytes
//  Table: fss_param_st[0]--> fss_param_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_fss_param")) return 0;
fss_param_st row;
St_fss_param *tableSet = new St_fss_param("fss_param",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.random_number_gen	 =          0; // ;
    row.adc_threshold	 =          5; // D=      5   minimum ADC value ;
    row.chamber_r_min	 =       7.73; // D=      8.0  inner radius [cm]    ;
    row.chamber_r_max	 =      30.05; // D=     32.0  outer radius [cm]    ;
    row.chamber_phi_min	 =         90; // D=      0.0  min. phi [deg]    ;
    row.chamber_phi_max	 =        150; // D=     40.0  max. phi [deg]    ;
    row.chamber_z_min	 =        150; // D=    150.0  zmin [cm]    ;
    row.chamber_cham_length	 =        120; // D=    120.0  float chamber length [cm];
    row.chamber_cath_voltage	 =     -10000; // D=   -10000.0 drift cathode voltage [V]    ;
    row.gas_attenuation	 =          0; // D=       0.0   attenuation [1/cm]    ;
    row.gas_gas_gain	 =       1200; // D=    2000.0   gas gain         ;
    row.gas_avg_ion_pot	 =         26; // D=      26.0   ave ion pot [eV]    ;
    row.readout_number_plane	 =         10; // D=   10        number of readout planes   ;
    row.readout_pad_pitch	 =       0.19; // D=   0.20      pad pitch [cm]    ;
    row.readout_pad_length	 =          2; // D=   2.0       pad length [cm]    ;
    row.readout_sigma_prf	 =       1500; // D=   600.0     pad-response-function sigma [um]    ;
    row.readout_shaper_time	 =        150; // D=   150.0     shaper time FWHM [ns]    ;
    row.readout_slice	 =        200; // D=   200.0     SCA time slice [ns]    ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
