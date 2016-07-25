Int_t Print(int sector,int nrows, raw_row_st *row, raw_pad_st *pad, raw_seq_st *seq, 
	    unsigned short *adc, unsigned short *indx)
{
  int i,j,k,l;
  int r,p,s;
  int offset;

  for(i=0;i<nrows;i++) {
    int pad_off = row[i].ipad;
    r = row[i].RowId;

    for(j=0;j<row[i].npad;j++) {
      int seq_off = (row[i].iseq + pad[pad_off + j].SeqOffset);
      p = pad[pad_off + j].PadId;

      offset = (row[i].ipixel +
                pad[pad_off + j].PadOffset);

      for(k=0;k<pad[pad_off + j].nseq;k++) {
        int tb = seq[seq_off+k].m + ((k>=pad[pad_off + j].SeqModBreak) ? 256 : 0);
        int n = seq[seq_off+k].i;

        s = k;

        if( (r>45) || (p>184) || (s>31) ||
            (r<1)  || (p<1)   || (s<0)) {
          printf("got an illegal sequence row=%i, pad=%i, seq=%i\n",r,p,s);
        }

        if(n==0) {
          printf("Got an illegal CPP of length 0\n");
        }
	printf("sector %i\trow %i\tpad %i\tseq %i\ttb %i\toffset %i\tlength %i", 
	       sector,r,p,s,tb,offset,n+1);
	for (l=0; l < n+1; l++) printf("\t%i:%i",adc[offset+l],indx[offset+l]);
	printf("\n");
        offset += n+1;
      } 
    }
  }

  return offset;
}
//________________________________________________________________________________
void RoyBT(TDataSet *rawData=0) {
  if (! rawData) return;
  TDataSet *sector = 0;
  TDataSetIter rawIter(rawData);
  while((sector = rawIter()) != NULL) {

    // The dataset tpc_sec_m is also in the "tpc_raw" dataset, don't use it...
    // Only use Datasets labeled "Sector_xx"

    if(!strstr(sector->GetName(),"Sector_")) continue;

    // Get the table structures...
    TDataSetIter sectorIter(sector);
    St_raw_row *Trow_in = (St_raw_row *)sectorIter.Find("raw_row_in");
    St_raw_row *Trow_out = (St_raw_row *)sectorIter.Find("raw_row_out");
    St_raw_pad *Tpad_in = (St_raw_pad *)sectorIter.Find("raw_pad_in");
    St_raw_pad *Tpad_out = (St_raw_pad *)sectorIter.Find("raw_pad_out");
    St_raw_seq *Tseq_in = (St_raw_seq *)sectorIter.Find("raw_seq_in");
    St_raw_seq *Tseq_out = (St_raw_seq *)sectorIter.Find("raw_seq_out");
    St_type_shortdata *Tadc_in = (St_type_shortdata *)sectorIter.Find("pixel_data_in");
    St_type_shortdata *Tadc_out = (St_type_shortdata *)sectorIter.Find("pixel_data_out");
    St_type_shortdata *Tindx_in = (St_type_shortdata *)sectorIter.Find("pixel_indx_in");
    St_type_shortdata *Tindx_out = (St_type_shortdata *)sectorIter.Find("pixel_indx_out");

    // Get the c arrays for this sector
    raw_row_st *row_in = Trow_in->GetTable();
    raw_row_st *row_out = Trow_out->GetTable();
    raw_pad_st *pad_in = Tpad_in->GetTable();
    raw_pad_st *pad_out = Tpad_out->GetTable();
    raw_seq_st *seq_in = Tseq_in->GetTable();
    raw_seq_st *seq_out = Tseq_out->GetTable();
    unsigned short *adc_in = (unsigned short *)Tadc_in->GetTable();
    unsigned short *adc_out = (unsigned short *)Tadc_out->GetTable();
    unsigned short *indx_in = (unsigned short *)Tindx_in->GetTable();
    unsigned short *indx_out = (unsigned short *)Tindx_out->GetTable();

    int sectorIdx = atoi(&(sector->GetName()[7]));

    Int_t sz = 0;
    
    sz += Print(sectorIdx,Trow_in->GetNRows(), row_in, pad_in, seq_in,adc_in, indx_in);
    sz += Print(sectorIdx,Trow_out->GetNRows(), row_out, pad_out, seq_out,adc_out,indx_out);
  }  
}
