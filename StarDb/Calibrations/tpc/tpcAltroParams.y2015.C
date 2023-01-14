TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  /*
Run15, TPX;       mysql -h dbbak.starp.bnl.gov -P 3414 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+--------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                 |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+--------------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-02-11 01:26:00 |      16041202 |      16091059 | production_pp200long_2015      |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-03-06 01:56:07 |      16064074 |      16093018 | production_pp200trans_2015     |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-03-14 05:40:15 |      16073003 |      16073003 | production_fms_pp200trans_2015 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-04-04 00:58:58 |      16093060 |      16117019 | production_pp200long2_2015     |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-04-16 15:54:54 |      16106024 |      16106033 | production_pp200long3_2015     |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-05-04 20:52:03 |      16124017 |      16161034 | production_pAu200_2015         |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-05-11 21:20:09 |      16131031 |      16132022 | production_pAu200_2015         |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-06-06 04:43:21 |      16156061 |      16156061 | production_pAu200_fms_2015     |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-06-09 06:26:48 |      16160012 |      16172040 | production_pAl200_2015         |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2015-06-12 18:13:43 |      16163037 |      16163044 | production_pAl200_2015_lowlumi |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+--------------------------------+
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
  cout << "[ALTRO] : Usin local configuration with Inner row.Altro_thr = " << row.Altro_thr << " row.Altro_seq = " << row.Altro_seq << endl;
  for (Int_t i  = 0; i < 24; i++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
