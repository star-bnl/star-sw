TDataSet *CreateTable()
	{ 
	if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
	St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",48);
	tpcAltroParams_st row;
/*
    Run = CURRENT; node =  onldb2.starp.bnl.gov; port = 3501
mysql -h  onldb2.starp.bnl.gov  -P 3501 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn)'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+---------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name            |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+---------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-05 16:52:11 |      22309003 |      22309008 | jmlPedAsPhys              |
|         3 |         1 |     0 |     0 |    40 |    0 |   400 |     1 | 2021-11-05 17:41:10 |      22309009 |      22360025 | pedestal_tcd_only         |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-05 17:43:31 |      22309010 |      22309010 | gatedGrid_tcd_only        |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-11 02:08:50 |      22314061 |      22358016 | laser_localclock          |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-11 03:36:58 |      22314069 |      22339050 | CosmicLocalClock_FieldOff |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-11 05:15:30 |      22315001 |      22350012 | CosmicLocalClock_FieldOn  |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-13 10:58:39 |      22317019 |      22342008 | pedAsPhys_tcd_only        |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-13 11:01:50 |      22317020 |      22322088 | pedAsPhys                 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-13 11:16:57 |      22317023 |      22317023 | pedAsPhys_4095_tcd_only   |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-11-17 19:48:49 |      22321057 |      22341031 | jaxCosmics                |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-12-02 17:40:49 |      22336040 |      22336040 | JaxPedAsPhys              |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-12-06 16:43:04 |      22340032 |      22340032 | jml_22_pp500_strawman     |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-12-06 21:04:02 |      22340054 |      22349003 | tune_pp500_2022           |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-12-14 07:52:52 |      22348018 |      22351032 | tuneVertex_2022           |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-12-15 08:17:02 |      22349011 |      22360018 | production_pp500_2022     |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-12-22 20:14:58 |      22356054 |      22356054 | dimuon_2022               |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+---------------------------+
*/
	row.N         =       6;
	row.Altro_thr =       3;
	row.Altro_seq =       2;
	row.Altro_K1  =   57158; //K1 coefficient of the TCF
	row.Altro_K2  =   21787; //K2 coefficient of the TCF
	row.Altro_K3  =   26699; //K3 coefficient of the TCF
	row.Altro_L1  =    7449; //L1 coefficient of the TCF
	row.Altro_L2  =   37911; //L2 coefficient of the TCF
	row.Altro_L3  =   58775; //L3 coefficient of the TCF

	cout << "[ALTRO] : Usin local configuration with row.Altro_thr = " << row.Altro_thr << endl; 

	for (Int_t i  = 0; i < 24; i++)
		{
		tableSet->AddAt(&row);
		}
	row.Altro_thr =       4; // iTPC, Tonko 12/12/2019
	cout << "[ALTRO] : Usin local configuration with row.Altro_thr = " << row.Altro_thr << endl; 

	for (Int_t i  = 0; i < 24; i++)
		{
		tableSet->AddAt(&row);
		}
	return (TDataSet *)tableSet;
	}
