#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <time.h>

#include <rtsLog.h>

// for TPX
#include <DAQ1000/ddl_struct.h>
#include <DAQ_TPX/tpxCore.h>
#include <TPC/rowlen.h>

#include "support.h"
#include "tpc23_fcf.h"

typedef unsigned long u_long ;

#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)

struct tpc23_fcf::row_pad_s_t tpc23_fcf::row_pad_s[RP_ROW_MAX][RP_PAD_MAX] ;

tpc23_fcf::tpc23_fcf()
{

	return ;
}

tpc23_fcf::~tpc23_fcf()
{

	return ;
}

void tpc23_fcf::run_start()
{
	for(int r=0;r<RP_ROW_MAX;r++) {
		for(int p=0;p<RP_PAD_MAX;p++) {
			row_pad[r][p].cou = 0 ;
		}
	}

}

void tpc23_fcf::init(int iid, int det_type)
{
	det = det_type ;
	id = iid ;

	if(id==0) {
		LOG(TERR,"Sizeof row_pad gains %d, row_pad %d",sizeof(row_pad_s),sizeof(row_pad)) ;
		for(int r=0;r<RP_ROW_MAX;r++) {
			for(int p=0;p<RP_PAD_MAX;p++) {
				row_pad_s[r][p].gain = 0x20 ;
			}
		}
	}

	for(int r=0;r<RP_ROW_MAX;r++) {
		for(int p=0;p<RP_PAD_MAX;p++) {
			row_pad[r][p].cou = 0 ;
		}
	}
	
}


// private
// using (det,sector,rdo) from the class
// and using the asic (ALTRO or FEE port) and its channel
// return row and also pad

u_char tpc23_fcf::flags_row_pad(int asic, int channel, int &row, int &pad)
{
	if(det==0) {	// TPX
		tpx_from_altro(rdo-1,asic,channel,row,pad) ;
		if(row==0 || row==255) return 0xFF ;
		if(pad==0 || pad==255) return 0xFF ;
		return row_pad_s[row][pad].flags ;
	}

	return 0xFF ;
}

//private
int tpc23_fcf::scan_tpx()
{
	int short_cou = 0 ;

	u_int *h = data_end ;


	while(likely(h>data_start)) {	// go backwards!!!!
	u_int hi, lo ;
	int ix = data_end - h ;

	lo = *h-- ;
	hi = *h-- ;

	int wc = ((hi&0x3F)<<4)|((lo&0xF0000)>>16) ;	// altro's word count
	
	int id = (lo&0xFF0) >> 4 ;	// altro id
	int ch = lo & 0xF ;		// channel 

#ifdef DEBUG
	printf("AID %d:%d - wc %d = %d = 0x%08X 0x%08X\n",id,ch,wc,ix,lo,hi) ;
#endif	
	while(wc%4) wc++ ;

	int row, pad ;
	if(unlikely(flags_row_pad(id,ch,row,pad))) {	// dead, missing
#ifdef DEBUG
		printf("    not: %d %d because %d %d\n",id,ch,row,pad) ;
#endif
		h -= wc/2 ;
		continue ;
	}
	
	u_short *d = row_pad[row][pad].d ;

	// reverse them
	ix = 0 ;
	for(int i=0;likely(i<wc);) {
		lo = *h-- ;	// low 20 bits
		hi = *h-- ;


		if(unlikely(ix==0)) {	// first shot to get rid of the alignment 0x2AA's
			u_short dd[4] ;


			dd[0] = (hi >> 10) & 0x3FF ;
			dd[1] = hi & 0x3FF ;
			dd[2] = (lo >> 10) & 0x3FF ;
			dd[3] = lo & 0x3FF ;

			int s = 0 ;
			if(dd[0]==0x2AA) {
				s = 1 ;
				if(dd[1]==0x2AA) {
					s = 2 ;
					if(dd[2]==0x2AA) {
						s = 3 ;
					}
				}
			}

			for(;s<4;s++) {
				d[ix++] = dd[s] ;
			}
		}
		else {
			d[ix++] = (hi >> 10) & 0x3FF ;
			d[ix++] = hi & 0x3FF ;
			d[ix++] = (lo >> 10) & 0x3FF ;
			d[ix++] = lo & 0x3FF ;
		}
		
		i += 4 ;
	}

	row_pad[row][pad].cou = ix ;	// save counter
	
//	printf("scan_tpx: row %d\n",row) ;

	short_cou += ix ;

#ifdef DEBUG
	for(int i=0;i<ix;i++) {
		printf("%3d = 0x%04X %u\n",i,d[i],d[i]) ;
	}
#endif	
	}


	return short_cou ;
}


// private
int tpc23_fcf::rdo_start_tpx(void *mem, int bytes)
{
	struct ddl_header *hdr ;
	struct ddl_trailer *trl ;

	hdr = (ddl_header *)mem ;

        // get stuff from the header...
        type = hdr->type & 0xF ; ;
        subtype = (hdr->type >> 4) & 0xF ;
        sector = (hdr->type >> 12) & 0x7F ;        // last bit might indicate an error!
	rdo = (hdr->type >> 8) & 0xF ;

	token = 0 ;
	trg_cmd = 0 ;
	daq_cmd = 0 ;

	data_start = 0 ;
        data_end = 0 ;

	// only now look for errors



        data_start = (u_int *)((char *)mem + sizeof(struct ddl_header)) ; // skip the header...

        // now lets move to the end...
        trl = (struct ddl_trailer *) ((char *)mem + bytes - sizeof(struct ddl_trailer)) ;

	u_int *l = (u_int *)trl - 1 ;	// move before the trailer

	int trg_cou = l[0] ;

	// move to the start of trigger data

	l -= trg_cou * (sizeof(trg_data)/4) ;

	trg_data *trg = (struct trg_data *)l ;

	token = 0 ;
	for(int i=0;i<trg_cou;i++) {
		switch(trg[i].csr & 0xFF000000) {
		case 0xFF000000 :
		case 0xDD000000 :
		case 0xEE000000 :
			break ;
		default :
			token = trg[i].data & 0xFFF ;
			daq_cmd = (trg[i].data>>12) & 0xF ;
			trg_cmd = (trg[i].data>>16) & 0xF ;
			break ;
		}
	}

	l -= 2 ;	// skip the 2 FEE_MASKS

	l-- ;		// point to the last data datum

	data_end = l ;

#ifdef DEBUG
	printf("=== S%02d:%d: type %d, subtype %d, T %d, trg_cmd %d, daq_cmd %d\n",
	    sector,rdo,type,subtype,token,trg_cmd,daq_cmd) ;
#endif

	return 0 ;
}


// This work on a per-rdo basis; preliminary scan into
// local format. Don't waste time so that we can free the
// RORC/TEF FIFO as soon as this routine returns.

int tpc23_fcf::stage1_rdo(void *mem, int bytes)
{
	int shorts = 512 ;	// some small number

	if(det==0) {
		rdo_start_tpx(mem,bytes) ;	// load the header, get token, trg, daq
		shorts = scan_tpx() ;
	}

	s1_store = 0 ;
	s1_words = shorts/2 ;

	return 0 ;
}

int tpc23_fcf::stage2_evt()
{
	for(int r=14;r<=20;r++) {
		stage1_row(r) ;
	}

	return 0 ;
}

// This finishes the event:
// TPX: after RDOs 3 & 4, or 5 & 6
// iTPC: after all 4 RDOs
int tpc23_fcf::stage1_row(int row)
{
	int r_len = 0 ;

	if(s1_store==0) {
		s1_store = (u_int *) malloc(s1_words*4) ;
	}

	if(det==0) {
		r_len = tpc_rowlen[row] ;
	}

	if(row==20) printf("row %d, rowlen %d\n",row,r_len) ;

	struct row_pad_t *rp = &row_pad[row][0] ;

	for(int p=1;p<=r_len;p++) {
		if(rp[p].cou==0) continue ;

		u_short *d1 = rp[p].d ;
		u_short *d1_stop = d1 + rp[p].cou ;

		while(d1<d1_stop) {
			int t1_cou, t1_lo, t1_hi ;

			t1_cou = *d1++ ;
			t1_hi = *d1++ ;

			t1_lo = t1_hi - t1_cou ;

			d1 += t1_cou - 2 ;

			if(row==20) printf("rp %d:%d: strip [%d:%d]\n",row,p,t1_lo,t1_hi) ;

			u_short *d2 = rp[p+1].d ;
			u_short *d2_stop = d2 + rp[p+1].cou ;

			while(d2<d2_stop) {
				int t2_cou, t2_lo, t2_hi ;

				t2_cou = *d2++ ;
				t2_hi = *d2++ ;

				t2_lo = t2_hi - t2_cou ;

				d2 += t2_cou - 2 ;

				if(t1_lo > t2_hi) continue ;
				else if(t2_lo > t1_hi) continue ;
				
				// merge them here!!!
			}
		}

	}


	return 0 ;
}

