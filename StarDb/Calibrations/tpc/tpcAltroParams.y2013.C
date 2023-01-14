TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  /*
Run13, TPX;       mysql -h dbbak.starp.bnl.gov -P 3412 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+--------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                 |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+--------------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2013-03-15 15:05:31 |      14074055 |      14161020 | pp500_production_2013          |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2013-03-24 19:34:14 |      14083049 |      14083050 | pp500_production_2013a         |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2013-03-28 10:16:42 |      14087025 |      14160015 | pp500_production_2013_noendcap |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2013-03-28 20:35:01 |      14087072 |      14095014 | pp500_production_fms_2013      |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2013-04-08 20:27:54 |      14098041 |      14119028 | pp500_production_fmsonly_2013  |
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
