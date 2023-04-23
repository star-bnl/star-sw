TDataSet *CreateTable()	{ 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",48);
  tpcAltroParams_st row, rowiTPC;
/*
Run18, TPX;       mysql -h dbbak.starp.bnl.gov -P 3417 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                      |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2018-03-12 16:24:39 |      19071038 |      19129014 | production_isobar_2018              |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2018-05-11 00:05:33 |      19130071 |      19268002 | 27GeV_production_2018               |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2018-05-18 23:16:02 |      19138032 |      19148047 | production_27GeV_fixedTarget_2018   |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2018-05-31 07:14:46 |      19151029 |      19155022 | production_3p85GeV_fixedTarget_2018 |
|         4 |         2 | 57158 | 21787 | 26699 | 7449 | 37911 | 58775 | 2018-06-05 17:42:25 |      19156034 |      19169017 | production_26p5GeV_fixedTarget_2018 |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
Run18, iTPC;      mysql -h dbbak.starp.bnl.gov -P 3417 -e 'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=31 and a.idx_rn=b.idx_rn and b.glb_setup_name like "%production%" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn);'
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
| ALTRO_thr | ALTRO_seq | K1    | K2    | K3    | L1   | L2    | L3    | min(a.beginTIme)    | min(a.idx_rn) | max(a.idx_rn) | glb_setup_name                      |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
|         0 |         0 |     0 |     0 |     0 |    0 |     0 |     0 | 2018-03-14 13:04:46 |      19073049 |      19129014 | production_isobar_2018              |
|         0 |         0 |     0 |     0 |     0 |    0 |     0 |     0 | 2018-05-11 00:05:33 |      19130071 |      19156030 | 27GeV_production_2018               |
|         0 |         0 |     0 |     0 |     0 |    0 |     0 |     0 | 2018-05-18 23:16:02 |      19138032 |      19148047 | production_27GeV_fixedTarget_2018   |
|         0 |         0 |     0 |     0 |     0 |    0 |     0 |     0 | 2018-05-31 07:14:46 |      19151029 |      19155022 | production_3p85GeV_fixedTarget_2018 |
|         4 |         2 | 57158 | 21787 | 26699 | 7499 | 37911 | 58775 | 2018-06-05 14:31:53 |      19156031 |      19160011 | 27GeV_production_2018               |
|         4 |         2 | 57158 | 21787 | 26699 | 7499 | 37911 | 58775 | 2018-06-05 17:42:25 |      19156034 |      19159046 | production_26p5GeV_fixedTarget_2018 |
|         4 |         1 | 57158 | 21787 | 26699 | 7499 | 37911 | 58775 | 2018-06-09 10:12:03 |      19160012 |      19268002 | 27GeV_production_2018               |
|         4 |         1 | 57158 | 21787 | 26699 | 7499 | 37911 | 58775 | 2018-06-09 20:37:47 |      19160032 |      19169017 | production_26p5GeV_fixedTarget_2018 |
+-----------+-----------+-------+-------+-------+------+-------+-------+---------------------+---------------+---------------+-------------------------------------+
*/
  row.N         =       6;
  row.Altro_thr =       4;
  row.Altro_seq =       2;
  row.Altro_K1  =   57158; //K1 coefficient of the TCF
  row.Altro_K2  =   21787; //K2 coefficient of the TCF
  row.Altro_K3  =   26699; //K3 coefficient of the TCF
  row.Altro_L1  =    7449; //L1 coefficient of the TCF
  row.Altro_L2  =   37911; //L2 coefficient of the TCF
  row.Altro_L3  =   58775; //L3 coefficient of the TCF

  cout << "sector != 20 [ALTRO] : Usin local configuration with Inner row.Altro_thr = " << row.Altro_thr << " row.Altro_seq = " << row.Altro_seq << endl;
  rowiTPC.N         =       6;
  rowiTPC.Altro_thr =       4; // iTPC, Tonko 12/12/2019, 
  rowiTPC.Altro_seq =       2; // confirmed by Jef 01/14/2023
  rowiTPC.Altro_K1  =   57158; //K1 coefficient of the TCF
  rowiTPC.Altro_K2  =   21787; //K2 coefficient of the TCF
  rowiTPC.Altro_K3  =   26699; //K3 coefficient of the TCF
  rowiTPC.Altro_L1  =    7449; //L1 coefficient of the TCF
  rowiTPC.Altro_L2  =   37911; //L2 coefficient of the TCF
  rowiTPC.Altro_L3  =   58775; //L3 coefficient of the TCF

  for (Int_t i  = 0; i < 24; i++)		{
    tableSet->AddAt(&row);
  }
  for (Int_t i  = 0; i < 24; i++)		{
    if (i == 20 - 1) {// SAMPA in sector 20
      cout << "sector == 20 [SAMPA] : Usin local configuration with Inner row.Altro_thr = " << rowiTPC.Altro_thr << " row.Altro_seq = " << rowiTPC.Altro_seq << endl;
      tableSet->AddAt(&rowiTPC);
    } else {
      tableSet->AddAt(&row);
    }
  }
  return (TDataSet *)tableSet;
}
