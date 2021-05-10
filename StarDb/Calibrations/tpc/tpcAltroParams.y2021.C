TDataSet *CreateTable()
	{ 
	if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
	St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",48);
	tpcAltroParams_st row;
/*
    Run = CURRENT; node =  onldb2.starp.bnl.gov; port = 3501
mysql -h  onldb2.starp.bnl.gov  -P 3501 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn)'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name         |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------+
|         3 |         1 |     0 |     0 |    40 |    0 |   400 |     1 | 2021-01-28 00:34:57 |      22027009 |      22030030 | pedestal_tcd_only      |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-28 00:57:20 |      22027016 |      22030031 | laser_localclock       |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-28 01:15:44 |      22027019 |      22029014 | CosmicLocalClock       |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-29 21:04:13 |      22029019 |      22029041 | CosmicLocalClock_gmt_a |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-29 21:52:25 |      22029022 |      22030049 | CosmicLocalClock_gmt_b |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-29 21:59:01 |      22029023 |      22030038 | CosmicLocalClock_gmt_c |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-29 22:06:24 |      22029024 |      22030008 | CosmicLocalClock_gmt_d |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-30 01:44:22 |      22029036 |      22030010 | CosmicLocalClock_gmt_e |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------+
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
