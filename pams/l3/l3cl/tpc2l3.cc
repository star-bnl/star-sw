/*:>-------------------------------------------------------------------
**: FILE:        tpc2l3.cc (StAF version)
**:
**: HISTORY:     last change - 09/16/98  cs 
**:              10/29/98 major bug, upper limit on pad loop corrected, py
**:  
**:<------------------------------------------------------------------*/

#include "PAM.h"
#include "tpc2l3.h"
#include "tpc2l3_inc.h"
#include <stdlib.h>


extern "C" long type_of_call tpc2l3_(
  TABLE_HEAD_ST     *raw_sec_m_h,        RAW_SEC_M_ST        *raw_sec_m,
  TABLE_HEAD_ST     *raw_row_in_h,       RAW_ROW_ST          *raw_row_in,
  TABLE_HEAD_ST     *raw_pad_in_h,       RAW_PAD_ST          *raw_pad_in,
  TABLE_HEAD_ST     *raw_seq_in_h,       RAW_SEQ_ST          *raw_seq_in,
  TABLE_HEAD_ST     *pixel_data_in_h,    TYPE_SHORTDATA_ST   *pixel_data_in,
  TABLE_HEAD_ST     *raw_row_out_h,      RAW_ROW_ST          *raw_row_out,
  TABLE_HEAD_ST     *raw_pad_out_h,      RAW_PAD_ST          *raw_pad_out,
  TABLE_HEAD_ST     *raw_seq_out_h,      RAW_SEQ_ST          *raw_seq_out,
  TABLE_HEAD_ST     *pixel_data_out_h,   TYPE_SHORTDATA_ST   *pixel_data_out,
  TABLE_HEAD_ST     *l3cl_pad_h,         L3CLPAD_ST          *l3cl_pad,
  TABLE_HEAD_ST     *l3cl_pixel_h,       TYPE_SHORTDATA_ST   *l3cl_pixel,
  TABLE_HEAD_ST     *para_h,             TPC2L3PARA_ST       *para,
  TABLE_HEAD_ST     *l3cl_para_h,        L3CLPARA_ST         *l3cl_para
                       )

{
/*:>--------------------------------------------------------------------
**: ROUTINE:     tpc2l3_
**: DESCRIPTION: Prepares data for l3 chain   
**:
**: 
**: AUTHOR:      cs  - Christof Struck, struck@star.physics.yale.edu
**: HISTORY:     10/26/98 py  check there is any input data           
**:
**: ARGUMENTS:
**:       IN:
**:        raw_sec_m
**:        raw_row_in / _out
**:        raw_pad_in / _out
**:        raw_seq_in / _out
**:        pixel_data_in / _out
**:        tpc2l3_para            <---- gives i960 no. and Start/EndRow
**:       OUT:
**:        l3cl_pad      \
**:        l3cl_pixel    - l3cl pixel format
**:        l3cl_para     - start/endrow for l3 clusterfinder
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

  int iRow, RowIndex;                         // count: iPad, iRow = 0...(MAX-1) !!!
  int iPad, iSeq, iPixel;                     // count: timebin    = 1...512
  int StartRow, EndRow;
  int SeqBegin, SeqEnd, SeqLength;
  int PixelIndex = 0;                         // Index for outgoing l3cl_pixel table
  // pointer to input data
  RAW_ROW_ST                  *Raw_Row_pointer;
  RAW_PAD_ST          *pad_p, *Raw_Pad_pointer;
  RAW_SEQ_ST          *seq_p, *Raw_Seq_pointer;
  TYPE_SHORTDATA_ST   *pix_p, *Pixel_pointer;
  // pointer to output
  L3CLPAD_ST          *out_pad_p = l3cl_pad;
  TYPE_SHORTDATA_ST   *out_pix_p = l3cl_pixel;
//
//    Check there are sequences coming in
//
  if ( raw_seq_in_h->nok < 1 ) return STAFCV_OK ;
//

  // initialize l3cl_pad and _pixel table
  l3cl_pad_h->nok = 0;
  l3cl_pixel_h->nok = 0;
  memset(l3cl_pad, 0, l3cl_pad_h->maxlen * sizeof(L3CLPAD_ST));
  memset(l3cl_pixel, 0, l3cl_pixel_h->maxlen * sizeof(TYPE_SHORTDATA_ST));

  // initialize Start- and EndRow for the given i960 id
  i960map( para->i960Number, &StartRow, &EndRow );
  // printf("start: %d, end: %d\n", StartRow, EndRow );

  // start/endrow for l3 clusterfinder (reverse order!!)
  l3cl_para_h->nok = 1;
  l3cl_para->StartRow = EndRow;
  l3cl_para->EndRow   = StartRow;

  // initialize RowIndex
  if ( StartRow < NPADROWS_IN )         // inner sector
      RowIndex = NPADROWS_IN - StartRow - 1;
  else                                  // outer sector
      RowIndex = NPADROWS_OUT + NPADROWS_IN - StartRow - 1;

  //
  // loop over padrows
  // note: raw_row table contains rows starting at NPADROWS (reverse order)
  //
  for ( iRow = StartRow; iRow >= EndRow; iRow--, RowIndex++ )
  {
      // initialize pointers to input tables
      //printf ( " \n Row %d ", iRow ) ;
      if ( StartRow < NPADROWS_IN )         // inner sector
      {
	  // initialize Raw_Row_pointer
	  Raw_Row_pointer = raw_row_in;
	  // first pad of row in raw_pad
	  Raw_Pad_pointer = raw_pad_in     + (Raw_Row_pointer+RowIndex)->ipad;
	  // first seq of first pad of row in raw_seq
	  Raw_Seq_pointer = raw_seq_in     + (Raw_Row_pointer+RowIndex)->iseq;
	  // first adc of first seq of pad of row in pixel_data
	  Pixel_pointer   = pixel_data_in  + (Raw_Row_pointer+RowIndex)->ipixel;
      }
      else                                  // outer sector
      {
	  Raw_Row_pointer = raw_row_out;
	  Raw_Pad_pointer = raw_pad_out    + (Raw_Row_pointer+RowIndex)->ipad;
	  Raw_Seq_pointer = raw_seq_out    + (Raw_Row_pointer+RowIndex)->iseq;
	  Pixel_pointer   = pixel_data_out + (Raw_Row_pointer+RowIndex)->ipixel;
      }

      pad_p = Raw_Pad_pointer; // running pointer for raw_pad table


      // loop over number of pads in this padrow
      int firstPad = ((Raw_Row_pointer+RowIndex)->PadFirst)-1 ;
      int lastPad  = firstPad +  (Raw_Row_pointer+RowIndex)->npad ; 
      for ( iPad = firstPad ; iPad < lastPad ; iPad++ )
      {
	  // loop over number of sequences in this pad
	  seq_p = Raw_Seq_pointer + pad_p->SeqOffset;
	  pix_p = Pixel_pointer   + pad_p->PadOffset;

	  //printf("\n nseq: %d,  pad: %d,  seqoff: %d", pad_p->nseq, pad_p->PadId, pad_p->SeqOffset );

	  for ( iSeq = 0; iSeq < pad_p->nseq; iSeq++ )
	  {
	      if ( iSeq >= pad_p->SeqModBreak )
	          SeqBegin = seq_p->m + 256;
	      else SeqBegin = seq_p->m;
	      SeqEnd = SeqBegin + seq_p->i;
	      SeqLength = seq_p->i + 1;
	      //printf("\n %d         : ", seq_p );
	      //printf("\n %d %d %d %d: ", iRow, iPad, SeqBegin, SeqEnd );

	      // write sequence to l3cl_pad
	      if ( l3cl_pad_h->nok >= l3cl_pad_h->maxlen )
	      {
		  printf("tpc2l3: l3cl_pad table too small !\n");
		  return STAFCV_BAD;
	      }
	      out_pad_p->row   = iRow+1;
	      out_pad_p->pad   = iPad+1;
	      out_pad_p->first = SeqBegin;
	      out_pad_p->last  = SeqEnd;
	      out_pad_p->jpix  = PixelIndex;
	      l3cl_pad_h->nok++;             // increase number of rows
	      out_pad_p++;                   // increase pointer to outgoing table

	      for ( iPixel = 0; iPixel < SeqLength; iPixel++)
	      {
		  // printf(" %d", pix_p->data);

		  // write adc values to l3cl_pixel
		  if ( l3cl_pixel_h->nok >= l3cl_pixel_h->maxlen )
		  {
		      printf("tpc2l3: l3cl_pixel table too small !\n");
		      return STAFCV_BAD;
		  }
		  out_pix_p->data = pix_p->data;
		  l3cl_pixel_h->nok++;       // increase number of rows
		  PixelIndex++;              // increase Index counter
		  out_pix_p++;             // increase pointer to outgoing table
		  pix_p++;
	      }
	      // printf("\n");
	      seq_p++;
	  }
	  pad_p++;
      }
  } // iRow

  return STAFCV_OK;
}

