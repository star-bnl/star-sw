/* bring in the math library explicitly */
#include <math.h>

#include "PAM.h"
#include "tss_tsspar.h"
#include "tcl_sector_index.h"
#include "type_index.h"
#include "type_shortdata.h"
#include "type_structtbl.h"
#include "raw_sec_m.h"
#include "raw_row.h"
#include "raw_pad.h"
#include "raw_seq.h"

long  reformat_fill_row
   (  long sptr,
      long dptr,
     int sector,
     int row,
     long npad,
    TABLE_HEAD_ST       *indextbl_h,     TYPE_INDEX_ST         *indextbl ,
    TABLE_HEAD_ST      *shortdata_h, TYPE_SHORTDATA_ST        *shortdata ,
    TABLE_HEAD_ST      *structtbl_h, TYPE_STRUCTTBL_ST        *structtbl ,
    TABLE_HEAD_ST      *raw_sec_m_h,      RAW_SEC_M_ST        *raw_sec_m ,
    TABLE_HEAD_ST     *raw_row_io_h,        RAW_ROW_ST       *raw_row_io ,
    TABLE_HEAD_ST     *raw_pad_io_h,        RAW_PAD_ST       *raw_pad_io ,
    TABLE_HEAD_ST     *raw_seq_io_h,        RAW_SEQ_ST       *raw_seq_io ,
    TABLE_HEAD_ST  *pixel_data_io_h, TYPE_SHORTDATA_ST    *pixel_data_io 
)
{
    int    i, j,ij,ji;
    long   iptr,iptrow,irow;
    long   ipad;
    int    nseq, nseq0, nseq1, nbuck0, bucket;
    int    ibucket;
    long    first_pad_row,first_seq_row,first_pixel_row,first_pixel_pad;
    long   npixel_this_row,npixel_this_pad;
    float   datum;
  
   iptrow=-100;
   for(irow=0;irow<raw_row_io_h->maxlen;irow++){
       if(raw_row_io[irow].RowId==row)iptrow=irow;
    }
   if(iptrow>=0){
          raw_row_io[iptrow].ipixel = pixel_data_io_h->nok;
          raw_row_io[iptrow].iseq =raw_seq_io_h->nok;
          raw_row_io[iptrow].ipad = raw_pad_io_h->nok;
          raw_row_io[iptrow].npixel = 0;
          raw_row_io[iptrow].nseq =0;
          raw_row_io[iptrow].npad = npad;
          raw_row_io[iptrow].PadModBreak = 0;
          raw_row_io[iptrow].unused1 = 0;
          raw_row_io[iptrow].unused2 = 0;
          raw_row_io[iptrow].unused3 = 0;
          raw_row_io[iptrow].PadRef = 'l';
 
          first_pad_row=raw_pad_io_h->nok;
          first_seq_row=raw_seq_io_h->nok;
          first_pixel_row=pixel_data_io_h->nok;
          
           
          /* loop over pad if pads in this row have hits */
          for(i=0;i<npad;i++){
                    
                  ipad  = structtbl[sptr++].info;
	          nseq0 = structtbl[sptr++].info;
	          nseq1 = structtbl[sptr++].info;
                  first_pixel_pad=pixel_data_io_h->nok;

                  /* initialize the raw_pad_io */
                  if (raw_pad_io_h->nok<raw_pad_io_h->maxlen){                      
                      raw_pad_io[raw_pad_io_h->nok].SeqModBreak=nseq0;
                      raw_pad_io[raw_pad_io_h->nok].nseq=nseq0+nseq1;
                      raw_pad_io[raw_pad_io_h->nok].unused1=0;
                      raw_pad_io[raw_pad_io_h->nok].PadId = ipad;  
		      if(ipad< raw_row_io[iptrow].PadFirst)
                                 raw_row_io[iptrow].PadFirst=ipad; 
                      raw_pad_io[raw_pad_io_h->nok].SeqOffset = raw_seq_io_h->nok - first_seq_row;
                      npixel_this_row = pixel_data_io_h->nok-first_pixel_row;
                      raw_pad_io[raw_pad_io_h->nok].PadOffset = npixel_this_row%65536;
                      if( npixel_this_row>=65536)  raw_row_io[iptrow].PadModBreak = raw_pad_io_h->nok-1;
                      raw_pad_io_h->nok++;

		  }
                  else{   
                      printf("Error: reformat_new:reformat_fill_row: raw_pad_io table full\n");
                      return STAFCV_BAD;
		  }

	          if (sptr+2*(nseq0+nseq1) > structtbl_h->nok){
		       printf(" Error: reformat_new:reformat_fill_row: sptr will be incremented out of range!\n");
		       return STAFCV_BAD;
		  }
		  for (j=0; j<(nseq0+nseq1); j++){
		     if (j<nseq0)
                        bucket = structtbl[sptr++].info;
                     else
                        bucket = structtbl[sptr++].info+256;
		     nbuck0 = structtbl[sptr++].info;
 
		     /* fill the raw_seq_io */
                     if (raw_seq_io_h->nok<raw_seq_io_h->maxlen){
		       /*    raw_seq_io[raw_seq_io_h->nok].m = (bucket-1)%256;  */
                         raw_seq_io[raw_seq_io_h->nok].m = bucket%256;
                         raw_seq_io[raw_seq_io_h->nok].i = nbuck0; 
		         raw_seq_io_h->nok++; 
                    }
                     else
                         printf("Error: reformat_new:reformat_fill_row: raw_seq_io table full!\n");

                     /* loop over one sequence, and fill the pixel_data_io */
		     /*                      for(ibucket=0;ibucket<nbuck0+1;ibucket++){ */
                      for(ibucket=0;ibucket<nbuck0+1;ibucket++){
			datum = shortdata[dptr++].data;
			if(pixel_data_io_h->nok<pixel_data_io_h->maxlen){
                           pixel_data_io[pixel_data_io_h->nok].data=datum;
                           pixel_data_io_h->nok++;
			} 
                        else
                           printf("Error: reformat_new:reformat_fill_row: pixel_data_io table full!\n");
		      }  /* end loop for all pixels in one sequence */

		  }  /* end loop for all sequences at one pad */                    
  
	         /* fill raw_row_io */
                 npixel_this_pad = pixel_data_io_h->nok-first_pixel_pad;
                 raw_row_io[iptrow].npixel += npixel_this_pad;
                 raw_row_io[iptrow].nseq +=nseq0+nseq1;
		 } /* end loop for pads in this row */

           } 	/* end if for iptrow!=0 */

            return STAFCV_OK;
} 









