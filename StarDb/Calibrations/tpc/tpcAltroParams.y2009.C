TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  /*
Run = Run9; node = dbbak.starp.bnl.gov; port = 3408
mysql -h dbbak.starp.bnl.gov  -P 3408 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%roduction%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn)'
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+--------------------------------+
| ALTRO_thr | ALTRO_seq | K1   | K2   | K3   | L1   | L2   | L3   | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                 |
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+--------------------------------+
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-03-07 10:05:09 |      10066057 |      10071041 | production2009_test            | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-03-12 07:08:37 |      10071029 |      10071054 | production2009_jeff            | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-03-19 20:52:43 |      10078049 |      10089004 | production2009_500GeV          | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-03-20 05:05:39 |      10079006 |      10079050 | production2009_500GeV_carl     | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-03-26 23:56:34 |      10085105 |      10089081 | production2009_500Gev_b        | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-03-27 02:36:04 |      10085131 |      10103043 | production2009_500Gev_c        | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-03-31 01:22:10 |      10089079 |      10089079 | production2009_500Gev_25       | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-04-10 03:16:29 |      10099190 |      10099191 | production2009_vpdsetup        | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-04-23 00:59:03 |      10112099 |      10125023 | production2009_200Gev_Hi       | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-04-23 03:56:47 |      10112121 |      10125027 | production2009_200Gev_Lo       | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-05-04 16:02:46 |      10124067 |      10181005 | production2009_200Gev_Single   | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-05-21 08:21:10 |      10141012 |      10179084 | production2009_200Gev_nocal    | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-05-30 03:16:30 |      10149086 |      10180034 | production2009_200Gev_noendcap | 
|         3 |         1 |    0 |    0 |   40 |    0 |  400 |    1 | 2009-06-28 20:54:58 |      10179069 |      10185023 | pp2pp_Production2009           | 
+-----------+-----------+------+------+------+------+------+------+---------------------+---------------+---------------+--------------------------------+
   */
  row.N         =       6;
  row.Altro_thr =       3;
  row.Altro_seq =       1;
  row.Altro_K1  =       0; //K1 coefficient of the TCF
  row.Altro_K2  =       0; //K2 coefficient of the TCF
  row.Altro_K3  =       0; //K3 coefficient of the TCF
  row.Altro_L1  =       0; //L1 coefficient of the TCF
  row.Altro_L2  =       0; //L2 coefficient of the TCF
  row.Altro_L3  =       0; //L3 coefficient of the TCF
  for (Int_t i  = 0; i < 24; i++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
