#include "reformat_new.h"

/* bring in the math library explicitly */
#include <math.h>

/* define the function */
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
    TABLE_HEAD_ST  *pixel_data_io_h, TYPE_SHORTDATA_ST    *pixel_data_io);

long  reformat_fill_repeat_row
   ( long irepeatrow, 
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
    TABLE_HEAD_ST  *pixel_data_io_h, TYPE_SHORTDATA_ST    *pixel_data_io);

/* begin the main programe */

long type_of_call reformat_new_
   (TABLE_HEAD_ST         *tsspar_h,     TSS_TSSPAR_ST           *tsspar ,
     TABLE_HEAD_ST         *tfc_sector_index_h,TCL_SECTOR_INDEX_ST  *tfc_sector_index, 
    TABLE_HEAD_ST       *indextbl_h,     TYPE_INDEX_ST         *indextbl ,
    TABLE_HEAD_ST      *shortdata_h, TYPE_SHORTDATA_ST        *shortdata ,
    TABLE_HEAD_ST      *structtbl_h, TYPE_STRUCTTBL_ST        *structtbl ,
    TABLE_HEAD_ST      *raw_sec_m_h,      RAW_SEC_M_ST        *raw_sec_m ,
    TABLE_HEAD_ST     *raw_row_in_h,        RAW_ROW_ST       *raw_row_in ,
    TABLE_HEAD_ST     *raw_pad_in_h,        RAW_PAD_ST       *raw_pad_in ,
    TABLE_HEAD_ST     *raw_seq_in_h,        RAW_SEQ_ST       *raw_seq_in ,
    TABLE_HEAD_ST  *pixel_data_in_h, TYPE_SHORTDATA_ST    *pixel_data_in ,
    TABLE_HEAD_ST    *raw_row_out_h,        RAW_ROW_ST      *raw_row_out ,
    TABLE_HEAD_ST    *raw_pad_out_h,        RAW_PAD_ST      *raw_pad_out ,
    TABLE_HEAD_ST    *raw_seq_out_h,        RAW_SEQ_ST      *raw_seq_out ,
    TABLE_HEAD_ST *pixel_data_out_h, TYPE_SHORTDATA_ST   *pixel_data_out 
)
{
  /* define local variables */
    int     rdoID, secID,ij;
    long  iptr, sptr, dptr;
    long  npad;
    int    row,current_sect,isect,ir,irow,err;
    int    sector, RDO,datatype;
    long min_buck,max_buck;
    long irepeatrow;

    /* Check all headers first */
    if(( indextbl_h->maxlen < 0 ) ||
       ( indextbl_h->nok > indextbl_h->maxlen ) ||
       ( shortdata_h->maxlen < 0 ) ||
       ( shortdata_h->nok > shortdata_h->maxlen ) ||
       ( structtbl_h->maxlen < 0 ) ||
       ( structtbl_h->nok > structtbl_h->maxlen ) ||
       ( raw_sec_m_h->nok < 0 ) ||
       ( raw_sec_m_h->nok >raw_sec_m_h->maxlen ) ||
       ( raw_row_in_h->nok < 0 ) ||
       ( raw_row_in_h->nok >raw_row_in_h->maxlen ) ||
       ( raw_pad_in_h->nok < 0 ) ||
       ( raw_pad_in_h->nok >raw_pad_in_h->maxlen ) ||
       ( raw_seq_in_h->nok < 0 ) ||
       ( raw_seq_in_h->nok >raw_seq_in_h->maxlen ) ||
       ( pixel_data_in_h->nok < 0 ) ||
       ( pixel_data_in_h->nok >pixel_data_in_h->maxlen )||
       ( raw_row_out_h->nok < 0 ) ||
       ( raw_row_out_h->nok >raw_row_out_h->maxlen ) ||
       ( raw_pad_out_h->nok < 0 ) ||
       ( raw_pad_out_h->nok >raw_pad_out_h->maxlen ) ||
       ( raw_seq_out_h->nok < 0 ) ||
       ( raw_seq_out_h->nok >raw_seq_out_h->maxlen ) ||
       ( pixel_data_out_h->nok < 0 ) ||
       ( pixel_data_out_h->nok >pixel_data_out_h->maxlen ) ){
	printf(" Error: reformat_new: Error in incoming table sizes!!!\n");
	return STAFCV_BAD;
       }

    /* fill raw_sec_m for this row */
   current_sect=indextbl[0].sector;
 
    if(current_sect==tfc_sector_index[0].CurrentSector && indextbl[3].rgm!=0){
          raw_sec_m[current_sect-1].RowRefIn = 'R' ;
          raw_sec_m[current_sect-1].RowRefOut = 'R' ;
     }

    /* fill the raw_row_in */
         for(iptr=0;iptr< indextbl_h->nok;iptr++){
	       sptr   = indextbl[iptr].struct_row;
	       dptr   = indextbl[iptr].data_row;
               sector = indextbl[iptr].sector;
	       RDO    = indextbl[iptr].rdo_loc;
               row    = indextbl[iptr].rgm;
	       npad  = structtbl[sptr++].info;
               datatype=indextbl[iptr].data_type;

       	        /* only for zero-suppressed data */             
                if(datatype==0||datatype==1){

  	            /* check the sector/RDO integrity */
	            if (RDO < 1 || RDO > 6){
	               printf(" Error: reformat_new: Invalid initial RDO number: %d\n", RDO);
	               return STAFCV_BAD;
 	            }
	            if (sector < 1 || sector > 24){
	               printf(" Error: reformat_new: Invalid initial sector number: %d\n",  sector);
      	               return STAFCV_BAD;
     	           }

	            /* Check the structure pointer integrity */
	            if (sptr > structtbl_h->nok)
	            printf(" Error: reformat_new: initial structure pointer out of range!\n" 
                            " sptr, structtbl_h->nok = %d, %d\n", sptr, structtbl_h->nok); 

   	            if(row<=13){
	            /* for the inner sector */
	                /* first check whether this row has been found before */
                        irepeatrow=-1;
	                for(ij=0;ij<raw_row_in_h->nok;ij++){
		             if(raw_row_in[ij].RowId==row&&raw_row_in[ij].npad!=0) irepeatrow=ij;
                        }

                        if(irepeatrow==-1)   /* fill raw tables directly */
                           err=reformat_fill_row(sptr,dptr,sector,row,npad,  indextbl_h,   indextbl,
                           shortdata_h,shortdata ,structtbl_h,structtbl,raw_sec_m_h,raw_sec_m ,
                           raw_row_in_h,raw_row_in ,raw_pad_in_h, raw_pad_in ,raw_seq_in_h, 
                           raw_seq_in ,pixel_data_in_h,pixel_data_in);
                        else     /* reorder the raw tables */
                           err=reformat_fill_repeat_row(irepeatrow,sptr,dptr,sector,row,npad,  indextbl_h, 
                           indextbl, shortdata_h,shortdata ,structtbl_h,structtbl,raw_sec_m_h,raw_sec_m ,
                           raw_row_in_h,raw_row_in ,raw_pad_in_h, raw_pad_in ,raw_seq_in_h, 
                           raw_seq_in ,pixel_data_in_h,pixel_data_in);
	             }
 	             else if(row>13&&row<=45){
	                    /* for the outer sector */

               	            /* first check whether this row has been found before */
                           irepeatrow=-1;
	                   for(ij=0;ij<raw_row_out_h->nok;ij++){
		                  if(raw_row_out[ij].RowId==row&&raw_row_out[ij].npad!=0) irepeatrow=ij;
                            }

                            if(irepeatrow==-1)   
                                err=reformat_fill_row(sptr,dptr,sector,row,npad, indextbl_h,   indextbl,
                                       shortdata_h,shortdata ,structtbl_h,structtbl,raw_sec_m_h,raw_sec_m ,
                                       raw_row_out_h,raw_row_out ,raw_pad_out_h, raw_pad_out ,raw_seq_out_h, 
                                       raw_seq_out ,pixel_data_out_h,pixel_data_out);
                             else 
                                  err=reformat_fill_repeat_row(irepeatrow,sptr,dptr,sector,row,npad,  
                                       indextbl_h, indextbl,shortdata_h,shortdata ,structtbl_h,structtbl,
                                       raw_sec_m_h,raw_sec_m ,raw_row_out_h,raw_row_out ,
                                       raw_pad_out_h, raw_pad_out ,raw_seq_out_h, 
                                       raw_seq_out ,pixel_data_out_h,pixel_data_out); 
			    /*                           if(err!=0)return STAFCV_BAD;
                            err=test_row_out(raw_sec_m_h,raw_sec_m ,
                                       raw_row_out_h,raw_row_out ,raw_pad_out_h, raw_pad_out ,raw_seq_out_h, 
                                       raw_seq_out ,pixel_data_out_h,pixel_data_out); */
	            }  /* end if for inner and outer sector */
         if(err!=STAFCV_OK)return STAFCV_BAD;
      }/* end if for datatype */
    } /* end loop for iptr */
    return STAFCV_OK;
}




