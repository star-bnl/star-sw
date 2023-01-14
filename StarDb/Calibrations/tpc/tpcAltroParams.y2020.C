TDataSet *CreateTable()	{ 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",48);
  tpcAltroParams_st row;
/*
Run20, TPX;       mysql -h dbbak.starp.bnl.gov -P 3419 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                      |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-12-08 10:09:05 |      20342006 |      21055017 | production_11p5GeV_2020             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-12-21 15:40:21 |      20355020 |      21045011 | production_5p75GeV_fixedTarget_2020 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-01-28 18:29:12 |      21028011 |      21029037 | production_31p2GeV_fixedTarget_2020 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-01-30 00:58:40 |      21029051 |      21032016 | production_9p8GeV_fixedTarget_2020  |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-01-31 01:21:12 |      21030030 |      21041013 | production_9p2GeV_2020              |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-02-01 18:10:44 |      21032040 |      21033017 | production_19p5GeV_fixedTarget_2020 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-02-02 16:04:09 |      21033026 |      21034013 | production_13p5GeV_fixedTarget_2020 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-02-04 05:35:18 |      21035003 |      21036013 | production_7p3GeV_fixedTarget_2020  |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-02-24 23:07:40 |      21055032 |      21080027 | production_9p2GeV_2020b             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-06-17 23:32:08 |      21169035 |      21245010 | production_9p2GeV_2020c             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-07-30 01:53:54 |      21211028 |      21258004 | production_26p5GeV_fixedTarget_2020 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-09-02 10:54:40 |      21246012 |      21255021 | production_7p7GeV_2020              |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
Run20, iTPC;      mysql -h dbbak.starp.bnl.gov -P 3419 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=31 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                      |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-12-08 10:09:05 |      20342006 |      21055017 | production_11p5GeV_2020             |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-12-21 15:40:21 |      20355020 |      21045011 | production_5p75GeV_fixedTarget_2020 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-01-28 18:29:12 |      21028011 |      21029037 | production_31p2GeV_fixedTarget_2020 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-01-30 00:58:40 |      21029051 |      21032016 | production_9p8GeV_fixedTarget_2020  |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-01-31 01:21:12 |      21030030 |      21041013 | production_9p2GeV_2020              |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-02-01 18:10:44 |      21032040 |      21033017 | production_19p5GeV_fixedTarget_2020 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-02-02 16:04:09 |      21033026 |      21034013 | production_13p5GeV_fixedTarget_2020 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-02-04 05:35:18 |      21035003 |      21036013 | production_7p3GeV_fixedTarget_2020  |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-02-24 23:07:40 |      21055032 |      21080027 | production_9p2GeV_2020b             |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-06-17 23:32:08 |      21169035 |      21245010 | production_9p2GeV_2020c             |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-07-30 01:53:54 |      21211028 |      21258004 | production_26p5GeV_fixedTarget_2020 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2020-09-02 10:54:40 |      21246012 |      21255021 | production_7p7GeV_2020              |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
*/
  memset(&row,0,tableSet->GetRowSize());
  row.N         =       6;
  row.Altro_thr =       3;
  row.Altro_seq =       2;
  row.Altro_K1  =   57158; //K1 coefficient of the TCF
  row.Altro_K2  =   21787; //K2 coefficient of the TCF
  row.Altro_K3  =   26699; //K3 coefficient of the TCF
  row.Altro_L1  =    7449; //L1 coefficient of the TCF
  row.Altro_L2  =   37911; //L2 coefficient of the TCF
  row.Altro_L3  =   58775; //L3 coefficient of the TCF
  
  cout << "[ALTRO] : Usin local configuration with Outer row.Altro_thr = " << row.Altro_thr << " row.Altro_seq = " << row.Altro_seq << endl; 

  for (Int_t i  = 0; i < 24; i++)    {
    tableSet->AddAt(&row);
  }
  row.Altro_thr =       3; //4; // iTPC, Tonko 12/12/2019
  row.Altro_seq =       1;
  cout << "[SAMPA] : Usin local configuration with Inner row.Altro_thr = " << row.Altro_thr << " row.Altro_seq = " << row.Altro_seq << endl; 
  
  for (Int_t i  = 0; i < 24; i++)		{
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
