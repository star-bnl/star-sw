#ifndef _RTSCNDDAQDETSETUP_H_
#define _RTSCNDDAQDETSETUP_H_

struct rtsCndDaqDetSetup
{
  //////////
  int idx_rn;
  int idx_det;
  //////////
  
  int ped_mode;
  int gain_mode;
  int analysis;
  int default_format;

  int asic_seq_hi;
  int asic_seq_lo;
  int asic_thr_hi;
  int asic_thr_lo;
  int time_bin_lo;
  int time_bin_hi;
  int clust_charge_lo;

  int raw_write;
  int cl_write;
  int cl_done;

  // New as of 2/18/09
  int res1;
  int res2;
  int res3;
  int res4;
  int res5;
  int res6;
  int res7;
  int res8;
  int res9;
  int res10;
};

#endif
