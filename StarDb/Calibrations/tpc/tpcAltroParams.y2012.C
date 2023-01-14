TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  /*
Run12, TPX;       mysql -h dbbak.starp.bnl.gov -P 3411 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name               |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-02-06 13:06:01 |      13037089 |      13037091 | pp200_production_2012_setup  |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-02-08 12:39:36 |      13039132 |      13072020 | pp200_production_2012        |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-02-12 15:19:56 |      13043025 |      13104049 | pp200_production_noemc_2012  |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-02-18 08:55:44 |      13049009 |      13054049 | pp200_production_fms_2012    |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-03-16 03:36:57 |      13075044 |      13109027 | pp500_production_2012        |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-03-23 15:32:59 |      13083040 |      13109013 | pp500_production_2012_noeemc |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-03-31 01:53:57 |      13090047 |      13109011 | pp500_production_fms_2012    |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-04-23 09:27:32 |      13114004 |      13136015 | UU_production_2012           |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-05-18 04:01:02 |      13139001 |      13177009 | cuAu_production_2012         |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-05-23 22:58:28 |      13144107 |      13173049 | cuAu_production_2012_noemc   |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2012-06-07 16:29:18 |      13159028 |      13159028 | cuA_production_2012          |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------------+
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
