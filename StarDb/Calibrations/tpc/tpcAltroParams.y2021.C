TDataSet *CreateTable()	{ 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",48);
  tpcAltroParams_st row;
/*
Run21, TPX;       mysql -h dbbak.starp.bnl.gov -P 3420 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                      |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-31 19:33:46 |      22031041 |      22121018 | production_7p7GeV_2021              |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-01 16:22:04 |      22121031 |      22179022 | production_3p85GeV_fixedTarget_2021 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-06 07:01:46 |      22126010 |      22126029 | production_44p5GeV_fixedTarget_2021 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-07 00:06:19 |      22126040 |      22128011 | production_100GeV_fixedTarget_2021  |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-07 03:16:39 |      22126045 |      22127018 | production_70GeV_fixedTarget_2021   |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-10 13:47:26 |      22130029 |      22136010 | production_OO_200GeV_2021           |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-13 08:19:27 |      22133002 |      22141016 | production_ps_OO_200GeV_2021        |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-14 14:47:28 |      22134016 |      22134017 | production_OO_fcsTiming             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-22 02:17:42 |      22141036 |      22144006 | production_FF_OO_200GeV_2021        |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-25 11:32:36 |      22145017 |      22158019 | production_17p3GeV_2021             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-06-04 02:30:45 |      22154036 |      22184019 | production_26p5GeV_fixedTarget_2021 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-06-30 02:28:49 |      22180043 |      22188007 | production_dAu200_2021              |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
Run21, iTPC;      mysql -h dbbak.starp.bnl.gov -P 3420 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=31 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                      |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-01-31 19:33:46 |      22031041 |      22121018 | production_7p7GeV_2021              |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-01 16:22:04 |      22121031 |      22179022 | production_3p85GeV_fixedTarget_2021 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-06 07:01:46 |      22126010 |      22126029 | production_44p5GeV_fixedTarget_2021 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-07 00:06:19 |      22126040 |      22128011 | production_100GeV_fixedTarget_2021  |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-07 03:16:39 |      22126045 |      22127018 | production_70GeV_fixedTarget_2021   |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-10 13:47:26 |      22130029 |      22136010 | production_OO_200GeV_2021           |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-13 08:19:27 |      22133002 |      22141016 | production_ps_OO_200GeV_2021        |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-14 14:47:28 |      22134016 |      22134017 | production_OO_fcsTiming             |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-22 02:17:42 |      22141036 |      22144006 | production_FF_OO_200GeV_2021        |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-05-25 11:32:36 |      22145017 |      22158019 | production_17p3GeV_2021             |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-06-04 02:30:45 |      22154036 |      22184019 | production_26p5GeV_fixedTarget_2021 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2021-06-30 02:28:49 |      22180043 |      22188007 | production_dAu200_2021              |
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

  for (Int_t i  = 0; i < 24; i++)		{
    tableSet->AddAt(&row);
  }
  row.Altro_thr =       3; // 4; // iTPC, Tonko 12/12/2019
  row.Altro_seq =       1;
  cout << "[SAMPA] : Usin local configuration with Inner row.Altro_thr = " << row.Altro_thr << " row.Altro_seq = " << row.Altro_seq << endl; 
  
  for (Int_t i  = 0; i < 24; i++)		{
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
