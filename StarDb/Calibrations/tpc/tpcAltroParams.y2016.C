TDataSet *CreateTable()	{ 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
/*
Run16, TPX;       mysql -h dbbak.starp.bnl.gov -P 3415 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+---------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                  |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+---------------------------------+
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2016-02-07 16:48:17 |      17038047 |      17130013 | AuAu_200_production_2016        |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2016-05-12 03:42:38 |      17132063 |      17141005 | dAu200_production_2016          |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2016-05-21 01:13:58 |      17141041 |      17148003 | dAu62_production_2016           |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2016-05-29 00:07:34 |      17149053 |      17160009 | dAu20_production_2016           |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2016-06-09 13:00:26 |      17161012 |      17169018 | dAu39_production_2016           |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2016-06-18 02:52:21 |      17169108 |      17179012 | AuAu200_production2_2016        |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2016-06-18 22:11:13 |      17170039 |      17170040 | AuAu200_production2_2016_MXQds7 |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2016-06-18 23:17:19 |      17170041 |      17170041 | AuAu200_production2_2016MXQds9  |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+---------------------------------+
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
  cout << "[ALTRO] : Usin local configuration with Inner row.Altro_thr = " << row.Altro_thr << " row.Altro_seq = " << row.Altro_seq << endl;
  for (Int_t i  = 0; i < 24; i++)		{
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
