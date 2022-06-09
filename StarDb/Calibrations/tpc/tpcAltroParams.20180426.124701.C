TDataSet *CreateTable()
	{ 
	if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
	St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
	tpcAltroParams_st row;
/*
    Run = CURRENT; node =  onldb2.starp.bnl.gov; port = 3501
    mysql -h  onldb2.starp.bnl.gov  -P 3501 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%roduction%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn)'
    +-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------+
    | ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name          |
    +-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------+
    |         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2010-01-02 15:24:50 |      11002051 |      11002104 | AuAu200_production_test | 
    |         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2010-01-03 02:46:48 |      11002120 |      11097111 | AuAu200_production      | 
    |         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2010-03-19 17:25:04 |      11078018 |      11098056 | AuAu62_production       | 
    |         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2010-04-09 07:51:16 |      11099002 |      11112023 | AuAu39_production       | 
    |         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2010-04-23 02:49:16 |      11112066 |      11140071 | AuAu7_production        | 
    +-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------+
*/
	row.N         =       6;
	row.Altro_thr =       4;
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
	return (TDataSet *)tableSet;
	}
