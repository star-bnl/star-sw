St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/cal/ems_cal_control Allocated rows: 1  Used rows: 1  Row size: 52 bytes
//  Table: ems_cal_control_st[0]--> ems_cal_control_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ems_cal_control")) return 0;
ems_cal_control_st row;
St_ems_cal_control *tableSet = new St_ems_cal_control("ems_cal_control",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.det	 =          6; // Detector number. See emc_def.h;
    row.iseed	 =  537428200; // seed for random number gen. =0 use default value ;
    row.ped_channel	 =          0; // mean pedestal for fake_pedestal ;
    row.ped_var	 =          0; // ch to ch pedestal variation for fake_pedestal ;
    row.slope_fun	 =          1; // slope function # for fake_adcslope ;
    row.slope_ave	 =          1; // average slope for fake_adcslope ;
    row.slope_var	 =          0; // slope variation for fake_adcslope ;
    row.ped_width	 =          2; // pedestal width for energy_to_adc ;
    row.toy_sim_mode	 =          0; // =0 pedestal, =1 gaussian ;
    row.noise_level	 =       0.05; // noise level for toy_simulator mode=0 ;
    row.noise_width	 =          1; // noise width for toy_simulator mode=0 ;
    row.gaus_peak	 =        1.5; // peak for toy_simulator mode=1 ;
    row.gaus_width	 =        0.1; // width for toy_simulator mode=1 ;
    row.occupancy	 =        0.5; // hit occupancy toy_simulator mode=1 ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
