TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  memset(&row,0,tableSet->GetRowSize());
  /* 
Run4, TPC;        mysql -h dbbak.starp.bnl.gov -P 3403 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=0 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+----------------------+
| ALTRO_thr | ALTRO_seq | K1   | K2   | K3   | L1   | L2   | L3   | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name       |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+----------------------+
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-01-08 13:38:22 |       5008019 |       5077006 | productionMinBias    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-01-30 22:19:18 |       5030092 |       5033107 | productionMinBiasHT  |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-02-02 21:58:47 |       5033108 |       5083018 | productionCentral    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-02-02 21:59:25 |       5033109 |       5084025 | productionHigh       |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-02-02 22:00:27 |       5033110 |       5084015 | productionMid        |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-02-02 22:15:25 |       5033111 |       5084033 | productionLow        |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-02-26 17:16:36 |       5057048 |       5060018 | productionHalfHigh   |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-02-26 17:21:29 |       5057049 |       5062060 | productionHalfLow    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-03-25 19:11:57 |       5085017 |       5093014 | production62GeV      |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-04-26 23:06:02 |       5117052 |       5135068 | productionPP         |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-05-06 01:13:27 |       5126110 |       5135024 | productionPPnoEndcap |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2004-05-06 01:16:01 |       5126111 |       5135033 | productionPPnoBarrel |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+----------------------+
Run5, TPC;        mysql -h dbbak.starp.bnl.gov -P 3404 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=0 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+--------------------------+
| ALTRO_thr | ALTRO_seq | K1   | K2   | K3   | L1   | L2   | L3   | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name           |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+--------------------------+
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-01-15 02:59:49 |       6014088 |       6065045 | cuProductionMinBias      |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-01-30 04:42:50 |       6029067 |       6065058 | cuProductionHighTower    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-03-01 19:56:54 |       6060055 |       6060078 | cuProductionFPD          |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-03-10 07:17:44 |       6069003 |       6081062 | cu62productionMinBias    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-03-22 22:53:08 |       6081094 |       6083034 | cu22ProductionMinBias    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-04-08 21:47:49 |       6098011 |       6174068 | ppProductionMinBias      |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-04-19 01:19:37 |       6108047 |       6175020 | ppProduction             |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-06-03 04:22:45 |       6154006 |       6154045 | pp400Production          |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-06-14 19:48:57 |       6165030 |       6167088 | ppTransProduction        |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2005-06-14 21:30:54 |       6165042 |       6167065 | ppTransProductionMinBias |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+--------------------------+
Run6, TPC;        mysql -h dbbak.starp.bnl.gov -P 3405 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=0 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
| ALTRO_thr | ALTRO_seq | K1   | K2   | K3   | L1   | L2   | L3   | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name         |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-03-05 04:29:29 |       7063151 |       7064086 | OldPPProduction        |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-03-12 16:20:37 |       7071047 |       7096017 | ppProduction           |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-03-13 23:16:38 |       7072060 |       7072062 | ppProduction_pibero1   |
|         0 |         0 |    0 |    0 |    0 |    0 |  400 |    0 | 2006-03-24 06:01:44 |       7083006 |       7083009 | ppProduction           |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-04-07 04:43:31 |       7097009 |       7129065 | ppProductionTrans      |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-04-26 00:58:18 |       7115132 |       7127091 | ppProductionTransNoEMC |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-05-12 03:59:16 |       7131043 |       7156028 | ppProductionLong       |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-05-16 00:52:39 |       7135052 |       7154045 | ppProductionLongNoEmc  |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-05-19 17:47:08 |       7139023 |       7156039 | ppProductionJPsi       |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-06-07 23:49:16 |       7158004 |       7171011 | ppProduction62         |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2006-06-07 23:56:12 |       7158005 |       7171013 | ppProductionMB62       |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+------------------------+
Run7, TPC;        mysql -h dbbak.starp.bnl.gov -P 3406 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=0 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+-----------------------+
| ALTRO_thr | ALTRO_seq | K1   | K2   | K3   | L1   | L2   | L3   | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name        |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+-----------------------+
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-04-02 21:22:23 |       8092092 |       8093002 | 2007TestProduction    |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-04-04 05:38:54 |       8094008 |       8095163 | 2007Production1       |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-04-05 06:12:12 |       8095032 |       8177054 | 2007Production2       |
|         1 |         2 |    0 |    0 |   40 |    0 |  400 |    1 | 2007-04-06 21:22:26 |       8096107 |       8177059 | 2007ProductionMinBias |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+-----------------------+
   */
  row.N = -1; // TPC only
  for (Int_t i  = 0; i < 24; i++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
