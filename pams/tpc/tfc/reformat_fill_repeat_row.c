/* bring in the math library explicitly */
#include <math.h>

#include "PAM.h"
#include "tcl_sector_index.h"
#include "type_index.h"
#include "type_shortdata.h"
#include "type_structtbl.h"
#include "raw_sec_m.h"
#include "raw_row.h"
#include "raw_pad.h"
#include "raw_seq.h"

RAW_PAD_ST raw_pad_temp[100];

long  reformat_fill_repeat_row
   ( long iRepeatRow, 
     long sptr,
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
    TABLE_HEAD_ST  *pixel_data_io_h, TYPE_SHORTDATA_ST    *pixel_data_io)
{
  /* define local variables */
    int    i, j,ij,ji,ik;
    long   iptr;
    long   ipad;
    int    nseq, nseq0, nseq1, nbuck0, bucket;
    int    ibucket;
    long    first_pad_row,first_seq_row,first_pixel_row;
    long   first_pixel_pad,first_pixel_seq;
    long   npixel_this_row,npixel_this_pad;
    float   datum;
    short  iRepeatPad;

     /* this row has been filled by input data from another readout board */
     printf("Information: there is a repeated row %d in %d sector\n",row, sector);
 
     first_pad_row= raw_row_io[iRepeatRow].ipad;
     first_seq_row=raw_row_io[iRepeatRow].iseq;
     first_pixel_row=raw_row_io[iRepeatRow].ipixel;
 
     /* loop over pad if pads in this row have hits */

      for(i=0;i<npad;i++){
                
          ipad  = structtbl[sptr++].info;
	  nseq0 = structtbl[sptr++].info;
	  nseq1 = structtbl[sptr++].info;
          first_pixel_pad=pixel_data_io_h->nok;

          /* find whether this pad ID is less than the last one in this row */
           iRepeatPad=-100;
           for(ji=raw_row_io[iRepeatRow].ipad+raw_row_io[iRepeatRow].npad;
                 ji>=raw_row_io[iRepeatRow].ipad;ji--){
                         if(ipad<raw_pad_io[ji].PadId)iRepeatPad=ji;
            }   
           /* check whether this pad should be the first pad in the row*/
             if(iRepeatPad< raw_row_io[iRepeatRow].PadFirst) {
                 raw_row_io[iRepeatRow].PadFirst=ipad;
	   } 

           /* if this pad is behind all the filled pad, it's easy */
           if( iRepeatPad<0){
                    
                  first_pixel_pad=pixel_data_io_h->nok;

                  /* initialize the raw_pad_io */
                  if (raw_pad_io_h->nok<raw_pad_io_h->maxlen){                      
                      raw_pad_io[raw_pad_io_h->nok].SeqModBreak=nseq0;
                      raw_pad_io[raw_pad_io_h->nok].nseq=nseq0+nseq1;
                      raw_pad_io[raw_pad_io_h->nok].unused1=0;
                      raw_pad_io[raw_pad_io_h->nok].PadId = ipad;  
		      if(ipad< raw_row_io[iRepeatRow].PadFirst)
                                 raw_row_io[iRepeatRow].PadFirst=ipad; 
                      raw_pad_io[raw_pad_io_h->nok].SeqOffset = raw_seq_io_h->nok - first_seq_row;
                      npixel_this_row = pixel_data_io_h->nok-first_pixel_row;
                      raw_pad_io[raw_pad_io_h->nok].PadOffset = npixel_this_row%65536;
                       if( npixel_this_row>=65536)  raw_row_io[iRepeatRow].PadModBreak = raw_pad_io_h->nok-1;
                     raw_pad_io_h->nok++;
		 }
                 else{   
                      printf("Error: reformat_new:reformat_fill_repeat_row: raw_pad_io table full\n");
                      return STAFCV_BAD;
		  }

	          if (sptr+2*(nseq0+nseq1) > structtbl_h->nok){
		       printf(" Error: reformat_new:reformat_fill_repeat_row: sptr will be incremented out of range!\n");
		       return STAFCV_BAD;
		  }
                  /* loop over all sequences in this pad */
		  for (j=0; j<(nseq0+nseq1); j++){

                     first_pixel_seq=pixel_data_io_h->nok;

		     if (j<nseq0)
                        bucket = structtbl[sptr++].info;
                     else
                        bucket = structtbl[sptr++].info+256;
		     nbuck0 = structtbl[sptr++].info;
 
		     /* fill the raw_seq_io */
                     if (raw_seq_io_h->nok<raw_seq_io_h->maxlen){
                         raw_seq_io[raw_seq_io_h->nok].m = bucket%256;
                         raw_seq_io[raw_seq_io_h->nok].i = nbuck0; 
		         raw_seq_io_h->nok++; 
                    }
                     else
                         printf("Error: reformat_new:reformat_fill_repeat_row: raw_seq_io table full\n");
                     /* loop over one sequence, and fill the pixel_data_io */
                      for(ibucket=0;ibucket<nbuck0+1;ibucket++){
			datum = shortdata[dptr++].data;
                        /* cut on the adc value to kick off the noise */
			/*                        if(datum>fmtpar[0].thresh){ */
  			    if(pixel_data_io_h->nok<pixel_data_io_h->maxlen){
                                pixel_data_io[pixel_data_io_h->nok].data=datum;
                                pixel_data_io_h->nok++;
			    } 
                            else
                                printf("Error: reformat_new:reformat_fill_repeat_row: pixel_data_io table full\n");
			    /*			} */
		      }  /* end loop for all pixels in one sequence */
		  }  /* end loop for all sequences at one pad */                    
     } /* iRepeatPad==0 */

     else {
            ik=0;
	   /* copy all the data with PadId>iRepeatPad to another variables from  raw_pad_io */
	    for(ij=iRepeatPad;ij<raw_pad_io_h->nok+1;ij++){
                    raw_pad_temp[ik].SeqModBreak=raw_pad_io[ij].SeqModBreak;
                    raw_pad_temp[ik].nseq=raw_pad_io[ij].nseq;
                    raw_pad_temp[ik].PadId=raw_pad_io[ij].PadId;
                    raw_pad_temp[ik].SeqOffset=raw_pad_io[ij].SeqOffset;
                    raw_pad_temp[ik].PadOffset=raw_pad_io[ij].PadOffset;
                    ik++;
	       }             
                  first_pixel_pad=pixel_data_io_h->nok;

                  /* fill this pad to  the raw_pad_io */
                  if (raw_pad_io_h->nok<raw_pad_io_h->maxlen){                      
                      raw_pad_io[iRepeatPad].SeqModBreak=nseq0;
                      raw_pad_io[iRepeatPad].nseq=nseq0+nseq1;
                      raw_pad_io[iRepeatPad].unused1=0;
                      raw_pad_io[iRepeatPad].PadId = ipad;  
		      if(ipad< raw_row_io[iRepeatRow].PadFirst)
                                 raw_row_io[iRepeatRow].PadFirst=ipad; 
                      raw_pad_io[iRepeatPad].SeqOffset = raw_seq_io_h->nok - first_seq_row;
                      npixel_this_row = pixel_data_io_h->nok-first_pixel_row;
                      raw_pad_io[iRepeatPad].PadOffset = npixel_this_row%65536;
		  }
                  else{   
                      printf("Error: reformat_new:reformat_fill_repeat_row: raw_pad_io table full\n");
                      return STAFCV_BAD;
		  }

	          if (sptr+2*(nseq0+nseq1) > structtbl_h->nok){
		       printf(" Error: reformat_new:reformat_fill_repeat_row: sptr will be incremented out of range!\n");
		       return STAFCV_BAD;
		  }
		  for (j=0; j<(nseq0+nseq1); j++){

                     first_pixel_seq=pixel_data_io_h->nok;

		     if (j<nseq0)
                        bucket = structtbl[sptr++].info;
                     else
                        bucket = structtbl[sptr++].info+256;
		     nbuck0 = structtbl[sptr++].info;
 
		     /* fill the raw_seq_io */
                     if (raw_seq_io_h->nok<raw_seq_io_h->maxlen){
                         raw_seq_io[raw_seq_io_h->nok].m = bucket%256; 
                         raw_seq_io[raw_seq_io_h->nok].i = nbuck0; 
		         raw_seq_io_h->nok++; 
                    }
                     else
                         printf("Error: reformat_new:reformat_fill_repeat_row: raw_seq_io table full");

                     /* loop over one sequence, and fill the pixel_data_io */
                      for(ibucket=0;ibucket<nbuck0+1;ibucket++){
			datum = shortdata[dptr++].data;
  			    if(pixel_data_io_h->nok<pixel_data_io_h->maxlen){
                                pixel_data_io[pixel_data_io_h->nok].data=datum;
                                pixel_data_io_h->nok++;
			    } 
                            else
                                printf("Error: reformat_new:reformat_fill_repeat_row: pixel_data_io table full\n");
			    /*			}*/
		      }  /* end loop for all pixels in one sequence */
		  }  /* end loop for all sequences at one pad */                    
                  /* copy data back from raw_pad_temp to raw_pad_io */  
                      raw_pad_io_h->nok++;
                      ik=0;
                      for(ij= iRepeatPad+1;  ij< raw_pad_io_h->nok;ij++){
                          raw_pad_io[ij].SeqModBreak=raw_pad_temp[ik].SeqModBreak;
                          raw_pad_io[ij].nseq=raw_pad_temp[ik].nseq;
                          raw_pad_io[ij].PadId=raw_pad_temp[ik].PadId;
                          raw_pad_io[ij].SeqOffset=raw_pad_temp[ik].SeqOffset;
                          raw_pad_io[ij].PadOffset=raw_pad_temp[ik].PadOffset;
                          ik++;
                      }                             
	      } /* end for if iRepeatPad */

	     /* fill raw_row_io */
            npixel_this_pad = pixel_data_io_h->nok-first_pixel_pad;
            raw_row_io[iRepeatRow].npixel += npixel_this_pad;
            raw_row_io[iRepeatRow].nseq +=nseq0+nseq1;
            raw_row_io[iRepeatRow].npad++;
	     /*    raw_row_io_h->nok++;   not increase it for repeated row*/ 
           } /* end loop for pads in this row */
            return STAFCV_OK;
}










