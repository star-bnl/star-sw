TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  /*
Run8, TPC;        mysql -h dbbak.starp.bnl.gov -P 3407 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=0 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
| ALTRO_thr | ALTRO_seq | K1   | K2   | K3   | L1   | L2   | L3   | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name         |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-11-29 21:46:15 |       8333086 |       9027087 | production_dAu2008     |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-12-04 03:54:34 |       8337102 |       9021043 | production_mb2008      |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-12-07 16:05:05 |       8341053 |       8352084 | production_PMD2008     |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-12-17 23:43:42 |       8351105 |       8362076 | test_production_mb2008 |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-02-12 07:48:02 |       9043035 |       9070007 | ppProduction2008       |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-02-21 14:27:37 |       9052061 |       9052109 | ppProductionMinBias    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-02-22 18:33:09 |       9053068 |       9053075 | production2008setup    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-03-03 23:26:05 |       9063108 |       9064050 | ppProduction2008fast   |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-03-04 16:05:57 |       9064040 |       9069078 | ppProduction2008-2     |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
Run8, TPX;        mysql -h dbbak.starp.bnl.gov -P 3407 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
| ALTRO_thr | ALTRO_seq | K1   | K2   | K3   | L1   | L2   | L3   | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name         |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
|         2 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-12-01 07:03:59 |       8335008 |       9027087 | production_dAu2008     |
|         2 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-12-07 16:05:05 |       8341053 |       8352084 | production_PMD2008     |
|         2 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-12-07 22:55:59 |       8341107 |       9021043 | production_mb2008      |
|         2 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-12-17 23:43:42 |       8351105 |       8362076 | test_production_mb2008 |
|         2 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-02-12 07:37:35 |       9043033 |       9050052 | ppProduction2008       |
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-02-19 14:34:36 |       9050053 |       9070007 | ppProduction2008       |
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-02-21 14:27:37 |       9052061 |       9052061 | ppProductionMinBias    |
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-02-22 18:33:09 |       9053068 |       9053075 | production2008setup    |
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-03-03 23:26:05 |       9063108 |       9064050 | ppProduction2008fast   |
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2008-03-04 16:05:57 |       9064040 |       9069078 | ppProduction2008-2     |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
  */
  for (Int_t i  = 0; i < 24; i++) {
    memset(&row,0,tableSet->GetRowSize());
    row.N = -1; // TPC only
    row.Altro_thr =       1;
    row.Altro_seq =       2;
    if (i == 15) {
      row.N         =       0;
      row.Altro_thr =       3;
      row.Altro_seq =       1;
      cout << "[ALTRO] in secor " << i+1 << ": Usin local configuration with Inner row.Altro_thr = " << row.Altro_thr << " row.Altro_seq = " << row.Altro_seq << endl;
    }
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
