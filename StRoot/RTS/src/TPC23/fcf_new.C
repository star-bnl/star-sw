#include <stdio.h>
#include <sys/types.h>


// this structure is kept per-thread, for the entire set of allowed rows

#define RP_ROW_MAX	(40+1)
#define RP_PAD_MAX	(120+1) 

#define FCF_ONEPAD	0x01

struct row_pad_t {
	// filled at configuration time aka once 
	u_short gain ;	// skip if 0
	u_short t0 ;
	u_char flags ;	// e.g. row-edge, deadpad-edge
	u_char c_tmp1 ;

	// count of sequences for this event
	u_char seq_cou ;	// needs to zapped to 0 before the next event
	u_char c_tmp2 ;

	struct {
		u_short blob_ix ;
		u_short tb_cou ;
		u_short tb_min ;
	} seq[32] ;

	u_short adc[512] ;
} row_pad[RP_ROW_MAX][RP_PAD_MAX] ;

struct blob_t {
	u_char p1 ;
	u_char p2 ;
	u_char flags ;
	u_char c_tmp1 ;	// for alignment

	u_short t1 ;
	u_short t2 ;

	u_short adc_cou ;	// for debugging
	u_short s_tmp1 ;	// for alignment
} blobs[1023] ;			// max per row?

int blob_finding(int row)
{
	row_pad_t *rpl ;

	int pad_max = 120 ;	// depends on row!
	int pad_min = 1 ;	// sane value

	int blob_cou = 0 ;

	// find the leftmost pad and count its sequences as the first blobs
	for(int pl=1;pl<=pad_max;pl++) {
		rpl = &row_pad[row][pl] ;
		
		if(rpl->seq_cou) {
			for(int sl=0;sl<rpl->seq_cou;sl++) {

				// and start a new blob for every sequence
				blobs[blob_cou].p1 = pl ;
				blobs[blob_cou].p2 = pl ;
				blobs[blob_cou].flags = rpl->flags | FCF_ONEPAD ;
				blobs[blob_cou].t1 = rpl->seq[sl].tb_min ;
				blobs[blob_cou].t2 = rpl->seq[sl].tb_min + rpl->seq[sl].tb_cou ;
				blobs[blob_cou].adc_cou = rpl->seq[sl].tb_cou ;

				rpl->seq[sl].blob_ix = blob_cou ;
				blob_cou++ ;
			}
			pad_min = pl ;
			break ;
		}
	}

		
	for(int pl=pad_min;pl<=(pad_max-1);pl++) {	// left pad

		rpl = &row_pad[row][pl] ;
		row_pad_t *rpr = &row_pad[row][pl+1] ;

		for(int sl=0;sl<rpl->seq_cou;sl++) {
			u_short tbl_min = rpl->seq[sl].tb_min ;
			u_short tbl_max = tbl_min + rpl->seq[sl].tb_cou ;

			for(int sr=0;sr<rpr->seq_cou;sr++) {
				u_short tbr_min = rpr->seq[sr].tb_min ;
				u_short tbr_max = tbr_min + rpr->seq[sr].tb_cou ;

				if(tbl_min>tbr_max) {	// case A
					rpr->seq[sr].blob_ix = blob_cou ;
					blob_cou++ ;
					continue ;
				}

				if(tbl_min>tbr_min) {	// case B
					rpr->seq[sr].blob_ix = rpl->seq[sl].blob_ix ;	
					continue ;
				}

				if(tbl_max>tbr_min) {	// case C
					rpr->seq[sr].blob_ix = rpl->seq[sl].blob_ix ;
					break ;
				}

				break ;	// case D
				

			}
		}
		
	}

	// I need to finish off the last pad here, I'm sure...

	return blob_cou ;
}

int main()
{
	printf("sizoef row_pad_t %ld, row_pad %ld\n",sizeof(struct row_pad_t),sizeof(row_pad)) ;



	return 0 ;
}


