TDataSet *CreateTable()	{ 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",48);
  tpcAltroParams_st row;
/*
Run19, TPX;       mysql -h dbbak.starp.bnl.gov -P 3418 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                      |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-02-25 20:23:20 |      20056032 |      20093036 | production_19GeV_2019               |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-03-07 03:50:35 |      20065059 |      20075026 | production_19GeV_bbcveto_2019       |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-04-04 09:44:05 |      20094048 |      20154013 | production_14p5GeV_2019             |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-04-17 13:34:01 |      20107029 |      20169055 | production_7.3GeV_fixedTarget_2019  |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-05-30 05:33:13 |      20150002 |      20154012 | production_lzr_14p5GeV_2019         |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-03 22:36:55 |      20154047 |      20178014 | production_7p7GeV_2019              |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-04 00:04:33 |      20154050 |      20178002 | production_lzr_7p7GeV_2019          |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-07 17:48:34 |      20158042 |      20160027 | production_3p85GeV_fixedTarget_2019 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-11 15:51:50 |      20162023 |      20162031 | production_7p7GeV_2019_opentac      |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-28 08:19:31 |      20179015 |      20196017 | production_9p2GeV_2019              |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-29 03:37:29 |      20179040 |      20183025 | production_4p59GeV_fixedTarget_2019 |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-07-09 03:23:12 |      20189035 |      20190024 | production_31GeV_fixedTarget_2019   |
|         3 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-07-10 11:01:57 |      20191005 |      20193026 | production_AuAu200_2019             |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
Run19, iTPC;      mysql -h dbbak.starp.bnl.gov -P 3418 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=31 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                      |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-02-25 20:23:20 |      20056032 |      20093036 | production_19GeV_2019               |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-03-07 03:50:35 |      20065059 |      20075026 | production_19GeV_bbcveto_2019       |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-04-04 09:44:05 |      20094048 |      20154013 | production_14p5GeV_2019             |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-04-17 13:34:01 |      20107029 |      20169055 | production_7.3GeV_fixedTarget_2019  |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-05-30 05:33:13 |      20150002 |      20154012 | production_lzr_14p5GeV_2019         |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-03 22:36:55 |      20154047 |      20178014 | production_7p7GeV_2019              |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-04 00:04:33 |      20154050 |      20178002 | production_lzr_7p7GeV_2019          |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-07 17:48:34 |      20158042 |      20160027 | production_3p85GeV_fixedTarget_2019 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-11 15:51:50 |      20162023 |      20162031 | production_7p7GeV_2019_opentac      |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-28 08:19:31 |      20179015 |      20196017 | production_9p2GeV_2019              |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-06-29 03:37:29 |      20179040 |      20183025 | production_4p59GeV_fixedTarget_2019 |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-07-09 03:23:12 |      20189035 |      20190024 | production_31GeV_fixedTarget_2019   |
|         3 |         1 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2019-07-10 11:01:57 |      20191005 |      20193026 | production_AuAu200_2019             |
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
  row.Altro_thr =       4; // instead 3, iTPC, Tonko 12/12/2019, 
  row.Altro_seq =       2; // instead 1, confirmed by Jef 01/14/2023
  cout << "[SAMPA] : Usin local configuration with Inner row.Altro_thr = " << row.Altro_thr << " row.Altro_seq = " << row.Altro_seq << endl; 

  for (Int_t i  = 0; i < 24; i++)		{
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
