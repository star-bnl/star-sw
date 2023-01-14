TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  /*
Run14, TPX;       mysql -h dbbak.starp.bnl.gov -P 3413 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                           |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2014-02-14 21:24:00 |      15045069 |      15070021 | production_15GeV_2014                    |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2014-02-22 20:56:02 |      15053044 |      15063051 | production_15GeV_2014_hft                |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2014-03-15 19:03:09 |      15074070 |      15094020 | AuAu_200_production_2014                 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2014-03-29 18:50:28 |      15088044 |      15167007 | AuAu_200_production_high_2014            |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2014-04-05 02:40:50 |      15094068 |      15166047 | AuAu_200_production_low_2014             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2014-04-05 03:28:30 |      15094070 |      15167014 | AuAu_200_production_mid_2014             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2014-04-21 23:36:15 |      15111056 |      15133016 | AuAu200_production_high_2014_RDO14-4-off |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2014-06-19 22:57:20 |      15170064 |      15187006 | AuHe3_production_2014                    |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+------------------------------------------+
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
