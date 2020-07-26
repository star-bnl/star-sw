#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg.h"

u_short fcs_trg_run_0();
u_short fcs_trg_run_201901();

u_int   fcs_trg_sim_adc[kMaxNS][kMaxDet][kMaxDep][kMaxCh] ;
float   fcs_trg_pt_correction[kMaxNS][kMaxDet][kMaxDep][kMaxCh];
float   fcs_trg_gain_correction[kMaxNS][kMaxDet][kMaxDep][kMaxCh];
u_short fcs_trg_pedestal[kMaxNS][kMaxDet][kMaxDep][kMaxCh] ;

int fcs_trgSelect=0;
int fcs_trgDebug=0;

u_short fcs_trg_run(int trgSelect, int debug){
       fcs_trgSelect=trgSelect;
       fcs_trgDebug=debug;
       u_short dsm_out=0;

       switch(fcs_trgSelect){
       case      0: dsm_out = fcs_trg_run_0();     break;   //0th version of ultimate algo
       case 201901: dsm_out = fcs_trg_run_201901(); break;  //1st version of run2019 algo
       }
       return dsm_out ;
}
