#include "PAM.h"
#include "tss_tsspar.h"
#include "raw_sec_m.h"
#include "raw_row.h"

#include "init_raw_table.h"

long  type_of_call init_raw_table_
  (TABLE_HEAD_ST         *tsspar_h,     TSS_TSSPAR_ST           *tsspar ,
    TABLE_HEAD_ST      *raw_sec_m_h,      RAW_SEC_M_ST        *raw_sec_m ,
    TABLE_HEAD_ST     *raw_row_in_h,        RAW_ROW_ST       *raw_row_in ,
    TABLE_HEAD_ST     *raw_row_out_h,        RAW_ROW_ST       *raw_row_out 
)
{
    int    isect,irow_in,irow_out;
    int    min_buck,   max_buck; 

    /* init the raw_sec_m table , calcualte min_buck, and max_buck */
    if(raw_sec_m_h->nok==0&&raw_sec_m_h->nok<raw_sec_m_h->maxlen+1){ 
        min_buck = tsspar[0].min_itime; 
        if(min_buck<1)min_buck = 1;
        max_buck = tsspar[0].max_itime; 
        if(max_buck>tsspar[0].ntime) max_buck = tsspar[0].ntime;

       /*  initialize the raw_sec_m table */
         
         for(isect=0;isect<24;isect++){       
            raw_sec_m[isect].SectorId = isect+1;
            raw_sec_m[isect].tfirst = min_buck;
            raw_sec_m[isect].tlast  = max_buck;
            raw_sec_m[isect].TimeRef = 'N' ;    
            raw_sec_m[isect].RowRefIn = 'N' ;   
            raw_sec_m[isect].RowRefOut = 'N' ;  
	    raw_sec_m_h->nok++; 
         }
  }

    for(irow_in=13;irow_in>0;irow_in--){
                 raw_row_in[raw_row_in_h->nok].RowId=irow_in;
                 raw_row_in[raw_row_in_h->nok].ipixel = 0;
                 raw_row_in[raw_row_in_h->nok].iseq =0;
                 raw_row_in[raw_row_in_h->nok].ipad = 0;
                 raw_row_in[raw_row_in_h->nok].npixel = 0;
                 raw_row_in[raw_row_in_h->nok].nseq =0;
                 raw_row_in[raw_row_in_h->nok].npad = 0;
                 raw_row_in[raw_row_in_h->nok].PadModBreak = 0;
                 raw_row_in[raw_row_in_h->nok].unused1 = 0;
                 raw_row_in[raw_row_in_h->nok].unused2 = 0;
                 raw_row_in[raw_row_in_h->nok].unused3 = 0;
                 raw_row_in[raw_row_in_h->nok].PadRef = 'l';
                 raw_row_in[raw_row_in_h->nok].PadFirst = 200;
                 raw_row_in_h->nok++; 
    }
    for(irow_out=45;irow_out>13;irow_out--){
                 raw_row_out[raw_row_out_h->nok].RowId=irow_out;
                 raw_row_out[raw_row_out_h->nok].ipixel = 0;
                 raw_row_out[raw_row_out_h->nok].iseq =0;
                 raw_row_out[raw_row_out_h->nok].ipad = 0;
                 raw_row_out[raw_row_out_h->nok].npixel = 0;
                 raw_row_out[raw_row_out_h->nok].nseq =0;
                 raw_row_out[raw_row_out_h->nok].npad = 0;
                 raw_row_out[raw_row_out_h->nok].PadModBreak = 0;
                 raw_row_out[raw_row_out_h->nok].unused1 = 0;
                 raw_row_out[raw_row_out_h->nok].unused2 = 0;
                 raw_row_out[raw_row_out_h->nok].unused3 = 0;
                 raw_row_out[raw_row_out_h->nok].PadRef = 'l';
                 raw_row_out[raw_row_out_h->nok].PadFirst = 200;
                 raw_row_out_h->nok++;
   }
 return STAFCV_OK;
}
           







