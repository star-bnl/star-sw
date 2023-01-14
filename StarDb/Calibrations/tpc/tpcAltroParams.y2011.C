TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  /*
Run11, TPX;       mysql -h dbbak.starp.bnl.gov -P 3410 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-----------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                    |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-----------------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-02-02 14:49:41 |      12033048 |      12098031 | pp500_production_2011             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-02-26 06:06:13 |      12057005 |      12097006 | pp500_production_2011_fms         |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-02-26 06:10:46 |      12057006 |      12104078 | pp500_production_2011_noeemc      |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-04-09 14:38:05 |      12099028 |      12108020 | pp500_production_2011_long        |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-04-09 21:50:37 |      12099047 |      12099047 | pp500_production_2011_long_fms    |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-04-10 01:26:58 |      12099055 |      12107028 | pp500_production_2011_long_noeemc |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-04-19 06:27:14 |      12109004 |      12109005 | AuAu18_production                 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-04-21 16:17:27 |      12111025 |      12122019 | AuAu19_production                 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-04-26 18:24:51 |      12116059 |      12116062 | AuAu11_production                 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-05-06 05:10:08 |      12126018 |      12179051 | AuAu200_production_2011           |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2011-06-21 15:20:23 |      12172013 |      12179097 | AuAu27_production_2011            |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-----------------------------------+
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
