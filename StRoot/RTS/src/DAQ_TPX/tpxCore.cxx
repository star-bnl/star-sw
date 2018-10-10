#include <stdio.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <rtsLog.h>

#include <TPX/tpx_altro_to_pad.h>
#include <DAQ1000/ddl_struct.h>
#include <TPC/fee_readout.h>


#include "tpxCore.h"
#include "tpx_fee_position.h"

// globally visible
struct tpx_rdo tpx_rdo[24][6] ;
struct tpx_rdo_dbg tpx_rdo_dbg[24][6] ;
int tpx_fee_check ;

// statics...
u_int expected_usercode[5] = {
//	0x1a830000,	// used in FY09
	0x0c500000,	// with the Xilinx disabled on powerup
	0x0283c6b3,	// FEE, 30Jan08
	0x02b9ab26,	// Bob, 02Jan08
	0x18a9352d,
//	0x00acf6c3	//06Sep08
//	0x00ad0581	//07Sep08
//	0x00AD9A39	//15Apr09
//	0x00ACEB76	//05Oct09
//	0x00ABF590	//07Oct11, DAQ10k: some RDOs had ALTRO timeouts
	0x00AD9D1D	//09Oct11, DAQ10k: maybe 1 clock delayed...
} ;

static inline u_int get10(u_int *l, u_int p) ;
static u_int *data_test(u_int *h, struct tpx_altro_struct *a, int log, u_int *first)  ;

//static int check_emul(u_int *a) ;

static struct {
	u_char altro ;
	u_char ch ;
	u_char rdo ;	// from 1 !!!
} tpx_pad_to_altro[46][183] ;



/*
	sectors count 1..24
	rows count 1..45 _but_ row 0 is reserved for non-connected channels
	pads count 1..182 ; pad 0 is never present

*/

/*
	RDO counts from 1
*/
void tpx_to_altro(int row, int pad, int &rdo, int &a, int &ch)
{
	static int first ;
	int ro, pa ;

//	LOG(DBG,"row %d, pad %d",row,pad) ;

	if(first == 0) {
		for(int r=0;r<6;r++) {
		for(int a=0;a<256;a++) {
		for(int ch=0;ch<16;ch++) {
			tpx_from_altro(r,a,ch,ro,pa) ;
			if(ro > 45) continue ;

			tpx_pad_to_altro[ro][pa].altro = a ;
			tpx_pad_to_altro[ro][pa].ch = ch ;
			tpx_pad_to_altro[ro][pa].rdo = r+1 ;	// rdo from 1
		}
		}
		}
		first = 1 ;
	}

//	LOG(DBG,"row %d, pad %d",row,pad) ;

	rdo = tpx_pad_to_altro[row][pad].rdo ;
	a = tpx_pad_to_altro[row][pad].altro ;
	ch = tpx_pad_to_altro[row][pad].ch ;

}

int *tpx_altro_to_row_override = 0 ;
int tpx_rdo_override = 0 ;
int tpx_fy16_map = 0 ;	// moved some FEEs from RDO1 to RDO2
int tpx_is_stgc = 0 ;

/*
	RDO counts from 0!
*/
void tpx_from_altro(int rdo, int a, int ch, int &row, int &pad)
{

	if(rdo<0 || rdo>5) {
		LOG(ERR,"RDO should count from 0") ;
		row = 255 ;
		pad = 255 ;
		return ;
	}

//LOG(WARN,"Change for sTGC") ;
#if 1
	if(tpx_fy16_map) {
		int remap = 0 ;

		switch(a) {
		case 52 :
		case 53 :
		case 54 :
		case 55 :
		case 56 :
		case 57 :
		case 58 :
		case 59 :
		case 60 :
		case 61 :
		case 66 :
		case 67 :
			remap = 1 ;
			break ;
		}

		if(remap) {
			if(rdo==0) {
				row = 255 ;	// those guys don't exist in RDO1 anymore!
				pad = 255 ;
				return ;

			} else if(rdo==1) {
				row = tpx_altro_to_pad[0][a][ch].row ;
				pad = tpx_altro_to_pad[0][a][ch].pad ;
				return ;

			}
		}

	}
	else {
//		LOG(WARN,"Not FY16 Map????") ;
	}
#endif

	row = tpx_altro_to_pad[rdo][a][ch].row ;
	pad = tpx_altro_to_pad[rdo][a][ch].pad ;

	if(tpx_altro_to_row_override) {
		if(tpx_rdo_override != (rdo+1)) {
			row = 255 ;
			pad = 255 ;
			return ;
		}

		row = tpx_altro_to_row_override[a&0xFE] ;

		if(a&1) pad = 1 + 16 + ch ;
		else pad = 1 + ch ;



//		LOG(TERR,"A %d: row %d, pad %d",a,row,pad) ;

		if(row<=0) {
			row = 255 ;
			pad = 255 ;
		}

		return ;
	}


#ifdef ESB_TEST_ETTIE
	// This is a hack for ETTIE's test!
	// Should not happen for normal TPX data so I will leave it in the formal code.
	if(rdo==1) {
		//printf("RDO %d: ALTRO %d:%d, row:pad %d:%d\n",rdo+1,a,ch,row,pad) ;
		if((a==252) || (a==253)) {
			row = a - 210 ;
			pad = ch + 1 ;
		}
	}
#endif
//#ifdef TEST_ETTIE
//	row = a - 210 ;
//	pad = ch + 1 ;
//#endif

#ifdef TEST_RDO
	if(rdo==5) {	// only 6th RDO can be a test!
		if((a>=50) && (a<=55)) {
			row = 255 ;
			pad = 255 ;
			return ;
		}
		if((a>=250) && (a<=255)) {
			a -= 200 ;

			row = tpx_altro_to_pad[rdo][a][ch].row ;
			pad = tpx_altro_to_pad[rdo][a][ch].pad ;
			return ;
		}
	}
#endif

	return ;
}

//used in tpxGain solely!
int tpx_altro_to_fee(int rdo, int a)
{
//	LOG(WARN,"tpx_altro_to_fee: %d %d (map %d)",rdo,a,tpx_fy16_map) ;

	rdo-- ;	// to start from 0

	a &= 0xFE ;	// make it even

	for(int i=0;i<36;i++) {
		int fee, altro ;

		if(tpx_fy16_map) {
			fee = tpx_fee_position[rdo][i] ;
		}
		else {
			fee = fee_position[rdo][i] ;
		}

		if(fee == 255) continue ;

		altro = (fee << 1) & 0xFF ;

		if(altro == a) return fee ;
	}

	return -1 ;
}

//used in tpxGain solely
u_char tpx_rdo_fees(int rdo, int cou)
{
//	LOG(WARN,"tpx_rdo_fees: %d %d (map %d)",rdo,cou,tpx_fy16_map) ;

	if(cou >= 36) return 255 ;

	if(tpx_fy16_map) return tpx_fee_position[rdo-1][cou] ;
	else return fee_position[rdo-1][cou] ;
}


#ifdef WHO_USUES_THIS
u_char tpx_altro_ch_to_fee(int a, int ch)
{
	if(a & 1) ch += 16 ;	// for odd altros, add 16

	for(int i=0;i<32;i++) {
		if(tpx_old_to_new_ch[i] == ch) return i ;
	}

	return 255 ;

}
#endif


/*
	Go through the event, check what you can
	and return the token.
	If we are called with do_log=1  _JUST_ do some
	checks and return the token with logging. Made to work as "get_token"
*/
int tpx_get_start(char *buff, u_int words, struct tpx_rdo_event *rdo, int do_log) 
{
	struct ddl_header *hdr ;
	struct ddl_trailer *trl ;


	u_int *l ;	// temporary pointer...


	
	hdr = (struct ddl_header *) buff ;

	rdo->data_err = 0 ;
	rdo->token = -ENOTSUP ;	// start as if error
	rdo->trg_cou = 0 ;
	rdo->rdo = 6 ;	// put bullshit values but not completelly bad!
	rdo->sector = 24 ;

	// the event needs to have at least the header and trailer!
	if(words < (2*sizeof(struct ddl_header))/4) {
		if(do_log) {
			LOG(ERR,"Event oddly small -- words %d",words) ;
		}
		return rdo->token ;
	}


	// get stuff from the header...
	rdo->type = hdr->type & 0xF ; ;
	rdo->subtype = (hdr->type >> 4) & 0xF ;
	rdo->sector = (hdr->type >> 12) & 0x7F ;	// last bit might indicate an error!
	rdo->rdo = (hdr->type >> 8) & 0xF ;



	rdo->data_end = 0 ;
	rdo->data_start = (u_int *)(buff + sizeof(struct ddl_header)) ;	// skip the header...

	rdo->l2_cmd = 0 ;

	// now lets move to the end...
	trl = (struct ddl_trailer *) (buff + 4*words - sizeof(struct ddl_trailer)) ;

	// NEW: for FY16 we moved some FEEs from RDO1 to RDO2
//	LOG(TERR,"Type 0x%08X",hdr->type) ;
	if((hdr->type != 0xFEED0301) && (hdr->type & 0x00100000)) {
		trl->type |= 0x00100000 ;
		tpx_fy16_map = 1 ; ;
		//LOG(TERR,"Setting FY16 map") ;
	}


//	LOG(TERR,"Header 0x%08X 0x%08X: type %d, subtype %d, sector %d, rdo %d",
//	    hdr->type,hdr->ev_cou,
//	    rdo->type,rdo->subtype,rdo->sector,rdo->rdo) ;

//	for(u_int i=0;i<words+8;i++) {
//		LOG(DBG,"%2d: 0x%08X",i,*((u_int *)buff + i)) ;
//	}



	if(trl->type != hdr->type) {
		if(hdr->type == 0xFEED0301) {	// HACK! Old factory type!
			if(do_log) LOG(WARN,"RDO %d: Old factory log?",rdo->rdo) ;
			rdo->data_start = (u_int *)(buff + 12) ;
			rdo->type = DDL_TYPE_LOG ;
		}
		else {
			if(do_log) {
				LOG(ERR,"RDO %d:%d: Header type 0x%08X and trailer type 0x%08X mismatch",rdo->sector,rdo->rdo,hdr->type,trl->type) ;

				u_int *d32 = (u_int *)(buff + 4*words) ;
				d32 -= 5 ;

				for(int i=0;i<10;i++) {
					LOG(WARN,"%d: 0x%08X",i,d32[i]) ;
				}
			}
			rdo->token = -EBADF ;
			return rdo->token ;
		}
	}



	switch(trl->fl_wc >> 28) {
	case 0 :		// all OK
		rdo->data_err = 0 ;
		break ;
	default :

		if(do_log) rdo->data_err = 1 ;	// HACK!
		break ;	// go on...
	}


	switch(rdo->type) {
	case DDL_TYPE_LOG :
	case DDL_TYPE_MSC :
		rdo->token = 4096 ;	// valid
		return rdo->token ;
	case DDL_TYPE_DTA :
		break ;	// later...
	default :
		if(do_log) {
			LOG(WARN,"Hm,I haven't coded this type %d", rdo->type) ;
			u_int *val = rdo->data_start ;
			for(int i=0;i<10;i++) {
				LOG(WARN,"\t%2d: 0x%08X [%u dec]",i,val[i],val[i]) ;
			}
		}
		rdo->token = -ENOTSUP ;
		return rdo->token ;

		break ;
	}



	l = (u_int *) trl - 1 ;		// move before the trailer...

	rdo->trg_cou = *l ;		// get the trigger count

	if(rdo->trg_cou > 124) {
		if(do_log) LOG(ERR,"Bad trg count %d>124, token %d",rdo->trg_cou, rdo->token) ;
//		rdo->trg_cou = 0 ;
//		rdo->token = -ENOTSUP ;
//		return rdo->token ;
	}
	
	// move to the start of trigger data
	l -= rdo->trg_cou * (sizeof(struct trg_data)/4) ;

	if(l < rdo->data_start) {
		if(do_log) LOG(ERR,"Bad trigger data!") ;
		rdo->data_err = 1 ;
		rdo->token =  -EBADF ;
		return rdo->token ;
	}

	rdo->trg = (struct trg_data *) l ;


	u_int rh  ;
	u_int rh_prompt = 0 ;
	rdo->token = 4097 ;	// in case I don't find a real token!

	for(u_int i=0;i<rdo->trg_cou;i++) {
		int rh_delta ;

		// fish out the token...
		switch(rdo->trg[i].csr & 0xFF000000) {
		case 0xFF000000 :	// fifo,skip
		case 0xDD000000 :	// FIFO, but selftrigger
			rh = rdo->trg[i].rhic_counter ;
			if(rh_prompt) {
				rh_delta = rh - rh_prompt ; 
			}
			else {
				rh_delta = (rh - tpx_rdo_dbg[rdo->sector-1][rdo->rdo-1].old_rhic) ;
			}
			break ;	
		case 0xEE000000 :	// emulated!
		default :		// real TCD prompt trigger
			rh_prompt = rdo->trg[i].rhic_counter ;
			rh_delta = rh_prompt - tpx_rdo_dbg[rdo->sector-1][rdo->rdo-1].old_rhic ;
			rdo->token = rdo->trg[i].data & 0xFFF ;
			break ;

		}

		if(do_log) {
		    if(rdo->rdo == 1) {
			 LOG(NOTE,"\tRDO %d: evt %d: trg %d: RHIC %u, CSR 0x%08X, data 0x%08X [t %d], bytes %u, delta %d",rdo->rdo,hdr->ev_cou,i,
		    		rdo->trg[i].rhic_counter,
		    		rdo->trg[i].csr,
		    		rdo->trg[i].data,
				rdo->trg[i].data&0xFFF,
		    		words*4,
		    		rh_delta) ;
		    }
		}

	}

	if(do_log) {
		if(rh_prompt) {
			tpx_rdo_dbg[rdo->sector-1][rdo->rdo-1].delta = rh_prompt - tpx_rdo_dbg[rdo->sector-1][rdo->rdo-1].old_rhic ;
			tpx_rdo_dbg[rdo->sector-1][rdo->rdo-1].old_rhic = rh_prompt ;
		}
		else {
			tpx_rdo_dbg[rdo->sector-1][rdo->rdo-1].delta = 0 ;
			// leave "old rhic" intact!

		}
		
	}

	if(do_log) LOG(DBG,"Event %d: sector %2d, rdo %d, type %d, subtype %d, token %d",
	    hdr->ev_cou,rdo->sector,rdo->rdo,rdo->type,rdo->subtype,rdo->token) ;

	l -= 2 ;	// points to FEE mask, will go away eventually since I never use it...

	//fee_mask[0] = l[0] ;
	//fee_mask[1] = l[1] ;
	
	if(do_log && (rdo->rdo==6)) {
		LOG(NOTE,"\t evt %d: %u %u",hdr->ev_cou,l[0],l[1]) ;
	}

	// 
	//if(rdo->subtype==DDL_DTA_EMUL) {
	//	check_emul(rdo->data_start) ;
	//	rdo->data_start = 0 ;	// mark as non-physics...
	//}

	if(rdo->data_err) LOG(ERR,"RDO %d: token %d, event %10u marked as in error [0x%X]",rdo->rdo,rdo->token,hdr->ev_cou,(trl->fl_wc >> 28)) ;

	if(do_log) LOG(DBG,"RDO %d: real data...",rdo->rdo) ;

	l-- ;	// and now points to the last datum

	rdo->data_end = l ;

	if((rdo->data_end - rdo->data_start) <= 0) {
		if(rdo->token != 4097) {
			if(do_log) LOG(ERR,"Bad RDO data: start 0x%X, end 0x%X, delta %d, tokenn %d!",rdo->data_start,rdo->data_end,rdo->data_end-rdo->data_start,rdo->token) ;
			rdo->token = -EBADF ;
		}
	}

	return rdo->token ;
}

	

/*
	This runs through the data pointer "now"
	backwards (in the altro data manner), checks
	for correctness.

	If the data is correct, unpacks it into 
	an altro_struct and returns the pointer 
	to the next altro in the data block.

	If the data is NOT correct, it scans
	bacwards until it finds a correct bank
	and repeats.

	If the "first" pointer is reached, it
	returns NULL.

*/
u_int *tpx_scan_to_next(u_int *now, u_int *first, struct tpx_altro_struct *a_struct)
{
	u_int *next_altro ;
	u_int *store = now ;
	int log_yes ;
	int was_log_yes ;
	int iter = 0 ;

	// I will log only if I'm told to
	log_yes = was_log_yes = a_struct->log_err ;

	a_struct->err = 0 ;	// clear error flag



	do {
		iter++ ;
		next_altro = data_test(now,a_struct,log_yes,first) ;	// returns pointer to next altro!

		// logic to print out on first error only....
		if(next_altro==0) {	// error in the bank!
			LOG(DBG,"Error in the bank at iter %d",iter) ;

			a_struct->err =  1 ;

			log_yes = 0 ;	// switch off further logging

			now-- ;		// decrement data buffer and try again...
	
		}
		else {		// data is OK
	
			if(was_log_yes && !log_yes) {	// we lost something before, turn on WARN
				LOG(NOTE,"    ...but found A%03d:%02d %d words earlier",a_struct->id,a_struct->ch,store-now) ;
	
			}

			if(((next_altro-first)<-1) || ((next_altro-first)>1000000)) {
				// I wan't to see this...
				LOG(ERR,"next_altro-first %d",next_altro-first) ;
			}


			//if(got_log && a_struct->log_err) {
			//	LOG(WARN,"Got it and now is: %d",a_struct->log_err) ;
			//}

			return next_altro ;	// pointer to next altro

		}

	
	} while((now-first)>=1) ;	// there must be at least 2 words!

	// can't be -- I wan't to see this
	LOG(ERR,"At end %d: now 0x%08X, next_altro 0x%08X, now-first 0x%08X, next-first 0x%08X",iter,
	    now,next_altro,now-first,next_altro-first) ;

	return 0 ;	// nothing found!

}

int tpx_use_rdo(char *rdobuff, int bytes, int (userfunc)(struct tpx_altro_struct *a, void *arg), void *arg)
{
	int t ;
	u_int *data_end ;
	tpx_rdo_event rdo ;
	tpx_altro_struct a ;
	int ret ;

	ret = 0 ;

	t = tpx_get_start(rdobuff, bytes/4, &rdo, 0) ;
	if(t<=0) {
		LOG(NOTE,"token %d, rdo %d: not an altro event",t,rdo.rdo) ;
		return ret ;
	}

	if(rdo.data_err) {
		LOG(ERR,"token %d, rdo %d: altro data error!",t,rdo.rdo) ;
		return -1 ;
	}

	a.what = TPX_ALTRO_DO_ADC ;
	a.rdo = rdo.rdo - 1;
	a.sector = rdo.sector ;
	a.t = t ;
	a.log_err = 0 ;

	LOG(DBG,"token %d, rdo %d: running through %d bytes...",t,rdo.rdo,bytes) ;

	data_end = rdo.data_end ;
	do {
		
		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;

		ret |= userfunc(&a, arg) ;
	} while((data_end > rdo.data_start) && data_end) ;

	return ret ;
}


/*
	Scans and uses one altro channels worth of data...

	h points to where the data is
	a->rb points to the input RB!

*/
static u_int *data_test(u_int *h, struct tpx_altro_struct *a, int log, u_int *first) 
{
  u_int hi, lo ;
  int wc ;
  int ret ;
  int delta ;
//  u_int *h_start = h ;

  ret = 0 ;

//  log = 1 ;

  a->count = 0 ;
  a->row = 0 ;	// unknown...
  a->pad = 0 ;

  delta = h - first ;
  if(delta < 1) {
	//if(log) LOG(ERR,"Startup offset bad %d",delta) ;
	return 0 ;
  }

  lo = *h-- ;
  hi = *h-- ;

  
  if((lo & 0xCFF00000) || (hi & 0xCFF00000)) {
    //if(log) LOG(WARN,"  Header words have junk: HI 0x%08X, LO 0x%08X",hi,lo) ;
    ret = -1 ;
  }


  // standard tests of the last ALTRO word...
  if((hi & 0xFFFC0) != 0xAAA80) {
    //if(log) LOG(WARN,"  Error HI in last ALTRO word: 0x%08X 0x%08X",hi,lo) ;
    ret = -2 ;

  }

  if((lo & 0x0F000) != 0x0A000) {
    //if(log) LOG(WARN,"  Error LO in last ALTRO word: 0x%08X 0x%08X",hi,lo) ;
    ret = -3 ;
  }


  wc = ((hi&0x3F)<<4) | ((lo&0xF0000)>>16) ;	// altro's word count



  a->id = (lo & 0xFF0)>>4 ;	// altro ID 0..255
  a->ch = lo & 0xF ;		// channel 0..15


  if((wc > 529) || (wc<0)) {	// for 512 tb + 15 pre + 2
    //if(log) LOG(WARN,"  Error in last ALTRO word: 0x%08X 0x%08X; bad WC %d",hi,lo,wc) ;
    ret = -4 ;
  }

//  log = 1 ;
//  if(log) {
//	LOG(TERR,"A%d:%d wc %d (0x%08X 0x%08X)",a->id,a->ch,wc,hi,lo) ;
//  }

  // we bomb out here if there was any error
  if(ret) {
	if(log) LOG(WARN,"RDO %d: T %d: A %d:%d(?) hdr[%d]",a->rdo+1,a->t,a->id,a->ch,ret) ;
	return 0 ;	// already error...
  }

  int rrow, ppad ;

  // FY19: special treatment for 
  if(tpx_is_stgc) {
	a->pad = a->ch ;
	a->row = a->id  ;

	goto real_tpx_skipped ;
  }


  for(int i=0;i<tpx_fee_override_cou;i++) {
	//LOG(TERR,"Checking sector %d:%d, rdo %d:%d, id %d:%d",a->sector,tpx_fee_override[i].sector,a->rdo,tpx_fee_override[i].rdo,a->id,tpx_fee_override[i].curr_altro) ;
	if(a->sector == tpx_fee_override[i].sector) {
	if(a->rdo == (tpx_fee_override[i].rdo-1)) {
	int fee = a->id & 0xFE ;	// kill last bit
	if(fee == tpx_fee_override[i].curr_altro) {
		int should = tpx_fee_override[i].orig_altro ;

		if(a->id & 1) {
			should = tpx_fee_override[i].orig_altro | 1 ;
		}
		else {
			should = tpx_fee_override[i].orig_altro ;
		}

		if(log) {
			//LOG(NOTE,"Sector %2d, RDO %d override: altro in data %3d but should be %3d!",a->sector,a->rdo+1,a->id,should) ;
		}


		a->id = should ;	// put the ID of the expected ALTRO!
	}
	}
	}
  }




  // get the row and pad; this is why we needed the rdo...
  tpx_from_altro(a->rdo, a->id, a->ch, rrow, ppad) ;

  a->row = rrow ;
  a->pad = ppad ;


  if((a->row > 45) || (a->pad > 182)) {
	if(log) LOG(ERR,"row:pad %d:%d illegal for altro %d:%d",a->row,a->pad,a->id,a->ch) ;
//LOG(WARN,"Change for sTGC: altro %d",a->id) ;
	if(tpx_rdo_override==0) return 0 ;
  }

  real_tpx_skipped:;


  if(wc == 0) return h ;	// empty channel...

  int l10 = wc ;

  while(l10 % 4) l10++ ;	// move until divisible by 4

  // l10 is minimally 4 for right now

  // sanity check
  delta = (h - l10/2) - first ;
  if(delta < -1) {
	if(log) LOG(ERR,"AID %d:%d: Bad offset %d, wrong wc %d", a->id, a->ch, delta, wc) ;
	return 0 ;
  }


  int p10 = 0 ;		// backward counter of the 10bit contributions

  // now we move through the padding, depending on the wc's last 2 bits...
  switch(wc&3) {
  case 0 :
    break ;

  case 1 :
    if(get10(h,p10) != 0x2AA) {
      //if(log) LOG(WARN,"  Bad 0x2AA 1:1") ;
      ret = -5 ;
    }
    p10++ ;

    if(get10(h,p10) != 0x2AA) {
      //if(log) LOG(WARN,"  Bad 0x2AA 1:2: 0x%X %d",get10(h,p10),wc) ;
      ret = -6 ;
    }
    p10++ ;

    if(get10(h,p10) != 0x2AA) {
      //if(log) LOG(WARN,"  Bad 0x2AA 1:3") ;
      ret = -7 ;
    }
    p10++ ;

    break ;

  case 2 :
    if(get10(h,p10) != 0x2AA) {
      //if(log) LOG(WARN,"  Bad 0x2AA 2:1") ;
      ret = -8 ;
    }
    p10++ ;

    if(get10(h,p10) != 0x2AA) {
      //if(log) LOG(WARN,"  Bad 0x2AA 2:2") ;
      ret = -9 ;
    }
    p10++ ;

    break ;

  case 3 :
    if(get10(h,p10) != 0x2AA) {
      //if(log) LOG(WARN,"  Bad 0x2AA 3:1") ;
      ret = -10 ;
    }
    p10++ ;

    break ;
  }




//  LOG(TERR,"AID %d:%d, wc %d: p10 %d, l10 %d, delta %d, next at %d",a->id,a->ch,wc,
//	p10,l10,h-first,(h-l10/2)-first) ;

#ifdef VEERY_PARANOID
  // check general form of _all_ the data -- nothing should be in the upper 12 bits
  for(int i=0;i<l10/4;i++) {
    if(h[-i] & 0xCFF00000) {
      // should not happen -- I want to see this!
      if(log) LOG(ERR,"  Bad form 0x%08X at %d",h[-i],i) ;	
      ret = -1 ;
      break ;
    }
  }
#endif

  if(ret) {
	if(log) LOG(WARN,"RDO %d: T %d: A %d:%d(?) hdr[%d]",a->rdo+1,a->t,a->id,a->ch,ret) ;
	return 0 ;	// already error...
  }




  // data check; we are in the data mode now...
  int tb_prev = 512 ;

  u_short *p_adc = a->adc ;
  u_short *p_tb = a->tb ;
  int tb_all = 0 ;

  while(p10 < l10) {
	int tb_cou = get10(h,p10++) ;
	int tb_last = get10(h,p10++) ;
	int tb_prev_last = tb_prev ;

	tb_cou -= 2 ;	// tb_cou & tb_last are included in the count, get rid of them...

	/*
	u_int tb_last 
		can't be greater than 511
		can't be less than 0
		can't be greater or equal to the end of the previous sequence

	
	int tb_cou
		can't be less than 0 
		can't be greater than 512
		can't be greater than the total word count!

	*/

	// do some sanity checks...
	if((tb_cou > wc) || (tb_cou <= 0) || (tb_cou > 512)) {
		//if(log) LOG(WARN,"  A%03d:%02d: Bad tb_cou %d > wc %d [tb_last %d]?",a->id,a->ch,tb_cou,wc,tb_last) ;
		ret = -1 ;
	}

	if((tb_last < 0) || (tb_last >= tb_prev)) {
		//if(log) LOG(WARN,"  A%03d:%02d: Bad tb_last %d => tb_prev %d?",a->id,a->ch,tb_last,tb_prev) ;
		ret = -2 ;
	}


	tb_all += tb_cou ;
	if(tb_all >= 512) {
		ret = -3 ;
		//if(log) LOG(WARN,"RDO %d: token %d: Altro %03d:%02d (?) tb_all [1] %d",a->rdo+1,a->t,a->id,a->ch,tb_all) ;
	}


	
	tb_prev = tb_last - tb_cou ;

	if(tb_prev < -1) {
		ret = -4 ;
		//if(log) LOG(WARN,"RDO %d: token %d: Altro %03d:%02d (?) tb_prev [2] %d",a->rdo+1,a->t,a->id,a->ch,tb_prev) ;
	}
	
	if(ret) {
		if(log) LOG(WARN,"RDO %d: T %d: A %d:%d(?) dta[%d]: pprev %d, prev %d, last %d, cou %d",a->rdo+1,a->t,a->id,a->ch,ret,
			    tb_prev_last,tb_prev,tb_last,tb_cou) ;
		return 0 ;
	}


	if(a->what & TPX_ALTRO_DO_ADC) {
		for(;tb_last > tb_prev; tb_last--) {
			*p_adc++ = get10(h, p10++) ;
			*p_tb++ = tb_last ;
		}
	}
	else {
		p10 += tb_cou ;
	}


	
  }





  if(ret) {
	if(log) LOG(ERR,"RDO %d: token %d: Altro %03d:%02d (?) data [2]",a->rdo+1,a->t,a->id,a->ch) ;
	return 0 ;	// already error...
  }
  else {
	a->count = p_adc - a->adc ;	// put count only if all OK
  }

  l10 /= 2 ;	// how many more words in this event...
			
  h -= l10 ;	// point now to the start of next altro...


//  LOG(TERR,"Returning delta %d",h-first) ;
//  note that -1 is the last return!

  return h ;	// return pointer to the start of the next altro!
}



#if 0
static int check_emul(u_int *a)
{
  int i ;
  u_int dta, should ;
  u_int errors ;

  errors = 0 ;
  for(i=0;i<200000;i++) {
    dta = *a++ ;

    should = ((i&0xFFFF)<<16)|(i&0xFFFF) ;

    if(dta != should) {
      errors++ ;
      LOG(ERR,"Mismatch %d: at %d: should 0x%08X, is 0x%08X",errors,i,should,dta) ;
      if(errors > 3) break ;
    }
  }

  if(errors) {
    return -1 ;
  }
	
  LOG(NOTE,"Emul evt checks OK...") ;
  return 0 ;

}
#endif

static inline u_int get10(u_int *l, u_int p)
{
  u_int ret ;

  l -= 2*(p/4) ;

  switch(p&3) {
  case 0 :
    ret = (l[-1] & 0xFFC00) >> 10 ;
    break ;
  case 1 :
    ret = (l[-1] & 0x3FF) ;
    break ;
  case 2 :
    ret = (l[0] & 0xFFC00) >> 10 ;
    break ;
  case 3 :
  default: 
    ret = l[0] & 0x3FF ;
    break ;
  }

  //printf("P %d, data 0x%X\n",p,ret) ;

  return ret ;
}


void tpx_analyze_log(int sector,int rdo, char *buff)
{
	int i ;
	char *start_ptr ;
	int len ;

	LOG(WARN,"tpx_analyze_log: deprecated!") ;

	len = strlen(buff) ;
	start_ptr = buff ;



	while(len) {
		for(i=0;i<len;i++) {
			if(start_ptr[i] == '\n') {
				start_ptr[i] = 0 ;
				if(strlen(start_ptr)) {

					LOG(INFO,"[Sec_%02d:RDO_%d] %s",sector,rdo,start_ptr) ;
				}
				start_ptr = start_ptr + i + 1 ;
				len = strlen(start_ptr) ;
				break ;
			}
		}
	}

	return ;

}

int tpx_show_status(int sector, int rb_mask, int *altro_list)
{
	struct tpx_rdo *rdo ;
	u_char altro[256] ;
	int err ;
	int rb ;

	err = 0 ;

	for(rb=0;rb<6;rb++) {

	if((1<<rb) & rb_mask) ;
	else continue ;

	rdo = &(tpx_rdo[sector-1][rb]) ;

	if(rdo->sector != sector || (rb+1) != rdo->rdo) {
		LOG(WARN,"msc: config for RDO %d: sector %d, rdo %d claims error",rb+1,rdo->sector & 0x7F,rdo->rdo) ;
		//err |= 1 ;
	}
	else {
		LOG(NOTE,"msc: config for RDO %d: sector %d, rdo %d",rb+1,rdo->sector,rdo->rdo) ;
	}

	LOG(NOTE,"msc: Remote %d, temp rdo %d, temp stratix %d",rdo->remote,rdo->temp_rdo,rdo->temp_stratix) ;
	LOG(NOTE,"msc: Compiled on %s",rdo->compilation_date) ;

	LOG(NOTE,"\t FPGAs: 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",
	    rdo->fpga_usercode[0],
	    rdo->fpga_usercode[1],
	    rdo->fpga_usercode[2],
	    rdo->fpga_usercode[3],
	    rdo->fpga_usercode[4]
	    ) ;

	for(int i=0;i<5;i++) {
		if((expected_usercode[i] != rdo->fpga_usercode[i]) && (rdo->fpga_usercode[i] != (u_int)(i+1))) {
			LOG(WARN,"msc: RDO %d: FPGA %d usercode is 0x%08X, expect 0x%08X!?",rdo->rdo,i,rdo->fpga_usercode[i],expected_usercode[i]) ;
		}
	}

	if(rdo->status_xilinx) {
		LOG(ERR,"msc: RDO %d: xilinx status: 0x%02X",rdo->rdo,rdo->status_xilinx) ;
		err |= 1 ;
	}
	else {
		LOG(NOTE,"RDO %d: xilinx status: 0x%02X",rdo->rdo,rdo->status_xilinx) ;
	}
	if(rdo->status_cpld) {
		LOG(ERR,"msc: RDO %d: CPLD status: 0x%02X",rdo->rdo,rdo->status_cpld) ;
		err |= 1 ;
	}
	else {
		LOG(NOTE,"RDO %d: CPLD status: 0x%02X",rdo->rdo,rdo->status_cpld) ;
	}


	memset(altro,0,sizeof(altro)) ;

	if(altro_list) {
		for(int a=0;a<256;a++) {
			altro[a] = altro_list[a] ;
		}
	}
	else {
		for(int a=0;a<256;a++) {
		for(int c=0;c<16;c++) {
			int row, pad ;

			tpx_from_altro(rb,a,c,row,pad) ;

			if(row <= 45) {
				altro[a] |= 0x1 ;	// need!
			}
		}
		}
	}

	// get list of bad FEEs and clear the "need" flag
	for(int i=0;i<tpx_odd_fee_count;i++) {
		if((tpx_odd_fee[i].sector != sector) || (tpx_odd_fee[i].rdo != (rb+1))) continue ;
		for(int j=0;j<2;j++) {
			for(int a=0;a<256;a++) {
				if(!altro[a]) continue ;

				if((tpx_odd_fee[i].altro_id_padplane + j) == a) {
					altro[a] = 0 ;
					LOG(WARN,"Marking ALTRO %3d (FEE %03d) as unneeded because it is marked bad in the gain file...",
					    a,tpx_odd_fee[i].tpc_fee_padplane) ;
				}
			}
		}

	}

	// get list of potentially overriden altros for this sector & rdo
	LOG(NOTE,"Checking override map [%d]",tpx_fee_override_cou) ;
	int over = 0 ;
	int in_fee[32], should_be[32] ;

	for(int i=0;i<tpx_fee_override_cou;i++) {
		LOG(NOTE,"Checking sector %d:%d, rdo %d:%d, id %d:%d",sector,tpx_fee_override[i].sector,rb+1,tpx_fee_override[i].rdo,
			tpx_fee_override[i].orig_altro,tpx_fee_override[i].curr_altro) ;

		if(sector == tpx_fee_override[i].sector) {
		if((rb+1) == tpx_fee_override[i].rdo) {
			in_fee[over] = tpx_fee_override[i].curr_altro ;
			should_be[over] = tpx_fee_override[i].orig_altro ;
			over++ ;
			LOG(WARN,"Expect to override wrong altro %3d with correct altro %3d!",tpx_fee_override[i].curr_altro,tpx_fee_override[i].orig_altro) ;


		}
		}

	}

	//new: check for the actual, correct, FEE complement
	int fcou ;

	fcou = 0 ;
	for(int b=0;b<3;b++) {
	for(int c=0;c<12;c++) {
		int altro = rdo->fee[b][c].id ;

//		LOG(TERR,"RDO %d: %2d: altro %3d, status %d",rdo->rdo,fcou,altro,rdo->fee[b][c].fee_status) ;


		//is the altro overriden?
		for(u_int i=0;i<sizeof(tpx_fee_override)/sizeof(tpx_fee_override[0]);i++) {
			int r = tpx_fee_override[i].rdo ;
			int s = tpx_fee_override[i].sector ;

			if(r==rdo->rdo && s==rdo->sector) ;
			else continue ;

			if(altro==tpx_fee_override[i].curr_altro) {
				altro = tpx_fee_override[i].orig_altro ;
				break ;
			}
		}

		//special cases
		if(rdo->sector==6 && rdo->rdo==3) {
			if(altro==200 || altro==214) {
				fcou++ ;
				continue ;
			}
		}


		int fee_expect = tpx_rdo_fees(rdo->rdo, fcou) ;
		int altro_expect = -1 ;

		if(fee_expect != 255) {
			altro_expect = (fee_expect << 1) & 0xFF ;
		}

		if(rdo->fee[b][c].fee_status) {
			if(altro_expect != altro) {
				LOG(WARN,"RDO %d: expect %d, got %d",rdo->rdo,altro_expect,altro) ;
			}
			
		}	
		else {
			if(altro_expect != -1) {
				LOG(WARN,"RDO %d: expect %d, got %d",rdo->rdo,altro_expect,altro) ;
			}
		}


		fcou++ ;
	}
	}


	fcou = 1 ;
	for(int b=0;b<3;b++) {
		for(int c=0;c<12;c++) {
			int ix = rdo->fee[b][c].id ;	// altro id!

			if((altro[ix] & 2)) {
				LOG(ERR,"Duplicate ALTRO %d status 0x%X at %d:%d",ix,altro[ix],b,c) ;
			}

			if(rdo->fee[b][c].fee_status) {	// found in the scan..

				altro[ix] |= 0x2 ;	// found
				altro[ix+1] |= 0x2 ;

				if(rdo->fee[b][c].jumpers != 3) {
					altro[ix] |= 4 ;
					altro[ix+1] |= 4 ;
				}

				if(rdo->fee[b][c].fee_status == 1) {	// normal; scan OK

				}
				else {
					altro[ix] |= 0x8 ;	// err!
					altro[ix+1] |= 0x8 ;

				}

				if(altro[ix] != 3) {	// somehing is not right so let's look at the possibilites...

					// is it overriden (jumpers should be non-3

					int overriden = 0 ;
					for(int i=0;i<over;i++) {
						if((in_fee[i] == ix) || ((in_fee[i]+1)==ix)) {
							LOG(WARN,"Sector %2d, RDO %d: %2d: FEE %3d (A%3d,%d) [port %d:%d:%d] = 0x%X, 0x%X -- overriden, should be A%3d",sector,rdo->rdo,fcou,
							    rdo->fee[b][c].pad_id,
							    rdo->fee[b][c].id,
							    rdo->fee[b][c].jumpers,
							    b,
							    rdo->fee[b][c].x_s>>4,
							    rdo->fee[b][c].x_s&1,
							    rdo->fee[b][c].fee_status,
							    altro[ix],
							    should_be[i]
					   		) ;
							overriden = 1 ;	// override error
							break ;
						}
					}
					
					// was it marked bad in the gain file?
					for(int i=0;i<tpx_odd_fee_count;i++) {
						if((tpx_odd_fee[i].altro_id_padplane == ix) || ((tpx_odd_fee[i].altro_id_padplane+1) == ix)) {
							
							LOG(WARN,"msc: Sector %2d, RDO %d: %2d: FEE %3d (A%3d,%d) [port %d:%d:%d] = 0x%X, 0x%X -- marked bad in gain file",sector,rdo->rdo,fcou,
							    rdo->fee[b][c].pad_id,
							    rdo->fee[b][c].id,
							    rdo->fee[b][c].jumpers,
							    b,
							    rdo->fee[b][c].x_s>>4,
							    rdo->fee[b][c].x_s&1,
							    rdo->fee[b][c].fee_status,
							    altro[ix]
							   ) ;

							overriden =  1 ;
							break ;
						}
					}

					if(overriden) ;
					else {
					LOG(ERR,"msc: Sector %2d, RDO %d: %2d: FEE %3d (A%3d,%d) [port %d:%d:%d] = 0x%X, 0x%X",
					    sector,rdo->rdo,fcou,
					    rdo->fee[b][c].pad_id,
					    rdo->fee[b][c].id,
					    rdo->fee[b][c].jumpers,
					    b,
					    rdo->fee[b][c].x_s>>4,
					    rdo->fee[b][c].x_s&1,
					    rdo->fee[b][c].fee_status,
					    altro[ix]
					   ) ;
					}
				}


				fcou++ ;
			}
		}
	}
	

	// now find missing ALTROs
	for(int a=0;a<256;a++) {
		if(altro[a] == 0) continue ;	// skip unneeded and not found...
		if(altro[a] == 3) continue ;	// needed and found; all ok...

		//I want the FEE for easier entry into the bad log...
		int fee = tpx_altro_to_fee(rb+1,a) ;

		if(altro[a] == 1) {	// needed but missing
			int overriden = 0 ;

			for(int i=0;i<over;i++) {
				if((a == should_be[i]) || (a == (should_be[i]+1))) {
					LOG(WARN,"Sector %2d, RDO %d: ALTRO %3d missing but was overriden: status 0x%X",sector,rb+1,a,altro[a]) ;
					overriden = 1 ;
					break ;
				}
			}

			if(overriden) continue ;


			LOG(ERR,"msc: Sector %2d, RDO %d: ALTRO %3d [FEE %3d] missing: status 0x%X",sector,rb+1,a,fee,altro[a]) ;
			
			err |= 2 ;
		}	
		else {
			LOG(WARN,"msc: Sector %2d, RDO %d: ALTRO %3d [FEE %3d] odd status: status 0x%X",sector,rb+1,a,fee,altro[a]) ;
			if((altro[a] & 0xB)==0xB) {	// needed, found & error
				err |= 2 ;
			}
		}
	}


	}

/*
	if(err == 2) {
		LOG(ERR,"Overriding FEE ID errors return until I fix this!!!") ;
		return 0 ;
	}
*/
	return err ;
}


// this is the real sector and real RDO (from 0)!
int tpx_analyze_msc(int sector,int rb, char *buff, int *altro_list)
{
//	struct tpx_rdo *rdo ;
//	rdo = (struct tpx_rdo *) buff ;

	memcpy(&(tpx_rdo[sector-1][rb]),buff,sizeof(struct tpx_rdo)) ;


	LOG(NOTE,"RDO %d: msc event, should be %d bytes",rb+1,sizeof(struct tpx_rdo)) ;

	return tpx_show_status(sector,1<<rb,altro_list) ;
}
