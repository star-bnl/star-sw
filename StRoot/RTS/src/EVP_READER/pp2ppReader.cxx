#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <evpReader.hh>
#include <evpSupport.h>
#include "pp2ppReader.h"


struct pp2pp_t pp2pp ;

// taken from ppSEQ.C
static unsigned char FromGrey[256] = {
//  0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    0,  1,  3,  2,  7,  6,  4,  5, 15, 14, 12, 13,  8,  9, 11, 10, 
   31, 30, 28, 29, 24, 25, 27, 26, 16, 17, 19, 18, 23, 22, 20, 21, //10
   63, 62, 60, 61, 56, 57, 59, 58, 48, 49, 51, 50, 55, 54, 52, 53, 
   32, 33, 35, 34, 39, 38, 36, 37, 47, 46, 44, 45, 40, 41, 43, 42, //30
  127,126,124,125,120,121,123,122,112,113,115,114,119,118,116,117,
   96, 97, 99, 98,103,102,100,101,111,110,108,109,104,105,107,106, //50
   64, 65, 67, 66, 71, 70, 68, 69, 79, 78, 76, 77, 72, 73, 75, 74,
   95, 94, 92, 93, 88, 89, 91, 90, 80, 81, 83, 82, 87, 86, 84, 85, //70
  255,254,252,253,248,249,251,250,240,241,243,242,247,246,244,245,
  224,225,227,226,231,230,228,229,239,238,236,237,232,233,235,234, //90
  192,193,195,194,199,198,196,197,207,206,204,205,200,201,203,202, 
  223,222,220,221,216,217,219,218,208,209,211,210,215,214,212,213, //B0
  128,129,131,130,135,134,132,133,143,142,140,141,136,137,139,138,
  159,158,156,157,152,153,155,154,144,145,147,146,151,150,148,149, //D0
  191,190,188,189,184,185,187,186,176,177,179,178,183,182,180,181,
  160,161,163,162,167,166,164,165,175,174,172,173,168,169,171,170  //F0
};

static int decodeData(int sec, void *data) ;

static int decodeEvent(int sec, unsigned char * data, u_int length);

static const char to_chains[4] = { 'A', 'B', 'C', 'D' } ;

int DAQpp2ppReader(char *m);

int pp2ppReader(char *m)
{
  static int oldrun=-1;
  static int oldbytes=-1;
  
  if(m == NULL) return EVP_DATA_ERR ;	// error
  
  evpReader *rrr = (evpReader *)m;
  if(!rrr->mem) {
    LOG(ERR, "No datap:   mem=0x%x sfs=0x%x",rrr->mem,rrr->sfs);
    return EVP_NO_DET;
  }

  if(oldrun == (int)rrr->event_number) {
    return oldbytes;
  };

  oldrun = rrr->event_number;
  oldbytes = DAQpp2ppReader(rrr->mem);

  return oldbytes;
}

int DAQpp2ppReader(char *m) 
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct DATAPX *datapx  ;
	struct PP2PPP *pp2ppp ;
	struct PP2PPR *voidp ;

	int off ;
	int len ;

	int sec ;


	pp2pp.channels = 0 ;
	pp2pp.max_channels = 1 ;
	pp2pp.mode = 0 ;

	memset(pp2pp.p_data,0,sizeof(pp2pp.p_data)) ;
	pp2pp.sec[0].len = pp2pp.sec[1].len = pp2pp.cam.len = 0 ;

	if(datap == NULL) return -1 ;	// error

	LOG(DBG,"In pp2pp reader...",0,0,0,0,0) ;

	len = (datap->det[EXT_ID].len) ;
	if(len == 0) return 0 ;	// not even a xtended det

	off = (datap->det[EXT_ID].off) ;

	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
		swap32(len) ;
		swap32(off) ;
	}

	datapx = (struct DATAPX *)(m + off*4) ;

	// verify bank
	if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
		return 0 ;
	}

	len = (datapx->det[PP_ID-10].len)  ;
	if(len == 0) return 0 ;

	off = (datapx->det[PP_ID-10].off) ;

	if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
		swap32(len) ;
		swap32(off) ;
	}


	pp2ppp = (struct PP2PPP *)((char *)datapx + off*4) ;

	// verify bank
	if(checkBank(pp2ppp->bh.bank_type, CHAR_PP2PPP) < 0) {
		return -1 ;
	}
	

	int do_swap ;

	if(pp2ppp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
		do_swap = 1 ;
	}
	else {
		do_swap = 0 ;
	}

	for(sec=0;sec<3;sec++) {	// 3 possible sectors aka detectors
		
		len = pp2ppp->sec[sec].len ;
		off = pp2ppp->sec[sec].off ;

		if(do_swap) {
			len = swap32(len) ;
			off = swap32(len) ;
		}

		LOG(NOTE,"PP2PP sector %d, len %d, offset %u",sec,len,off,0,0) ;

		if(pp2ppp->sec[sec].len == 0) continue ;

		
		voidp = (struct PP2PPR *)((char *)pp2ppp + off*4) ;
		if(checkBank((char *)voidp,CHAR_PP2PPR) < 0) return -1 ;

//		pp2pp.p_data[sec] = (void *)voidp ;

		if(decodeData(sec, voidp) < 0) return -1 ;	// error!
	}

	return 1 ;
}


static int decodeData(int sec, void *data)
{
	struct PP2PPR *ppr ;
	u_int token, seq, xing, type ;
	u_int length ;
	u_int do_swap ;
	u_int i ;


	ppr = (struct PP2PPR *) data ;

	if(ppr->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
		do_swap =1 ;
	}
	else {
		do_swap = 0 ;
	}

	token = ppr->bh.token ;
	seq = ppr->seq ;
	xing = ppr->xing ;
	type = ppr->type ;
	length = ppr->bh.length ;


	if(do_swap) {
		token = swap32(token) ;
		seq = swap32(seq) ;
		xing = swap32(xing) ;
		type = swap32(type) ;
		length = swap32(length) ;
	}


	LOG(DBG,"Length in PPR is %u",length,0,0,0,0) ;

	length = length - ((u_int)ppr->data - (u_int)data)/4 ;	// words of real data

	LOG(DBG,"Length in after calc is %u",length,0,0,0,0) ;

	pp2pp.p_data[sec] = (void *)ppr->data ;
	u_int *d32 = (u_int *)ppr->data ;


	if(sec == 2) {	// CAMAC!
		pp2pp.cam.len = length ;
		pp2pp.cam.seq = seq ;
		pp2pp.cam.xing = xing ;
		pp2pp.cam.type = type ;
		pp2pp.cam.token = token ;

		// make all values illegal
		memset(pp2pp.cam.d,0xFF,sizeof(pp2pp.cam.d)) ;

		u_int *last_datum = d32 + length ;

		u_int first_datum = *d32++ ;	// skip

		if(do_swap) swap32(first_datum) ;

		LOG(DBG,"First 0x%08X",first_datum,0,0,0,0) ;


		while(d32 < last_datum) {
			u_int type_len = *d32++ ;	
			
			if(do_swap) type_len = swap32(type_len) ;

			u_int words = (type_len & 0xFFFF) / 8 ;

			
			LOG(DBG,"type_len 0x%08X, words %d",type_len, words,0,0,0) ;
			

			for(i=0;i<words;i++) {
				u_int slot = *d32++ ;
				u_int datum = *d32++ ;
				u_int board, reg ;

				if(do_swap) {
					slot = swap32(slot) ;
					datum = swap32(datum) ;
				}

				board = slot >> 16 ;
				reg = slot & 0xFFFF ;

				// boards count from 1 but regs from 0!
				LOG(DBG,"Board %d, reg %d",board,reg,0,0,0) ;
				//printf("word %d: %d %d 0x%08X\n",i,board,reg,slot);
				//fflush(stdout) ;
				pp2pp.cam.d[board-1][reg] = datum ;

			}

			

		}		
	}
	else {	// silicon/sequencers

		pp2pp.sec[sec].len = length ;
		pp2pp.sec[sec].seq = seq ;
		pp2pp.sec[sec].xing = xing ;
		pp2pp.sec[sec].type = type ;
		pp2pp.sec[sec].token = token ;

		memset(pp2pp.sec[sec].err,0,sizeof(pp2pp.sec[sec].err)) ;
		memset(pp2pp.sec[sec].d,0,sizeof(pp2pp.sec[0].d)) ;

		if(decodeEvent(sec, (unsigned char *)d32, 4*length) < 0) return -1 ;
	}

	return 0 ;
}


// length in bytes
static int decodeEvent(int sec, unsigned char * d, u_int length) 
{
	u_int i, j, chlen, cptr ;
	int chainID, ppSEQID, chanID ;
	u_int realSeq ;
	int svx, old_svx, old_chan ;
	int error_on ;

	static u_char guard[MAXSVX][MAXCHAN] ;
	static u_char chains[MAXSEQ][MAXCHAIN] ;

	svx = 0x0FFFFFFFF ;	// laaarge
	old_chan = old_svx = -1 ;


	memset(chains,0,sizeof(chains)) ;

	chlen = 0 ;

	for(cptr = 0; cptr < length; cptr += ((chlen+1)/2)*4+4) {	

		chlen = d[cptr+2] | ((d[cptr+3] & 0xF) << 8); 

		chainID = d[cptr] & 0x3;
		realSeq = ppSEQID = (d[cptr] >> 2) & 0x3F;	// this counts from 1 so move to 0

		error_on = 0 ;

		if(chainID >= MAXCHAIN) {
			error_on = 1 ;
		}
		else if(sec == 0) {	// SEQID from 1..4
			if((ppSEQID < 1) || (ppSEQID > 4)) {
				error_on = 1 ;
			}
		} 
		else {
			if((ppSEQID < 5) || (ppSEQID > 8)) {
				error_on = 1 ;
			}
		}


		if(error_on) {
			// we'll mark the whole sector as bad!
			for(i=0;i<MAXSEQ;i++) {
				for(j=0;j<MAXCHAIN;j++) {
					pp2pp.sec[sec].err[i][j] = 1 ;	
				}
			}

			LOG(ERR,"Sector %d: incorrect SEQID %d or chain %c",sec+1,realSeq,to_chains[chainID],0,0) ;
			return 0 ;
		}

		// at this stage the seq id and the chain ID are correct so we can use the ppSEQID & chainID

		// start from 0
		if(sec == 0) ppSEQID -= 1 ;
		else ppSEQID -= 5 ;



		// check for chain overrun!
		if(chains[ppSEQID][chainID]) {	// error - chain overrun!
			pp2pp.sec[sec].err[ppSEQID][chainID] = 1 ;
			LOG(ERR,"Sector %d: overrun SEQID %d or chain %c",sec+1,realSeq,to_chains[chainID],0,0) ;
			continue ;
		}
		else {
			chains[ppSEQID][chainID] = 1 ;	// mark as used
		}
		

		error_on = 0 ;


		LOG(DBG,"SEQID %d, chain %c, chlen %d, 1 %d, 4 0x%02X",realSeq,to_chains[chainID],chlen,d[cptr+1],d[cptr+4]) ;

		// initalize the channel guard
		memset(guard,0,sizeof(guard)) ;

		old_svx = -1;
		old_chan = -1 ;

		//if(error_on) goto error_exit ;


		// get the first SVX
		svx = d[cptr+4] ;

		// checks: first value must be an SVX and must be 0!
		if(((svx & 0x80) == 0) || (svx & 0x7F)) {	// error
			LOG(ERR,"Sector %d: SEQID %d: chain %c: first SVX corrupt 0x%02X",sec+1,realSeq,to_chains[chainID],svx,0) ;
			pp2pp.sec[sec].err[ppSEQID][chainID] = 1 ;
		}



		for (i = 4; i < chlen*2 + 4; i += 2) {


			if (d[cptr+i] & 0x80) {		// this is an SVX ID
				svx = d[cptr+i] & 0x7F;

				LOG(DBG,"SVX 0x%02X:   0x%X 0x%X 0x%X 0x%X",svx,d[cptr+i],d[cptr+i+1],d[cptr+i+2],d[cptr+i+3]) ;

				if(old_svx >= 0) {	// zap old guard
					memset(guard[old_svx],1,sizeof(guard[old_svx])) ;				
				}

				old_chan = -1 ;

				if((old_svx+1) != svx) {
					if(svx > 5) {
						// known cases
						if((svx==7) && (realSeq==1) && (old_svx==2)) {
							LOG(NOTE,"Known bad SVX: Fixing SVX %d (expecting %d) at SEQID %d, fixing to %d",svx,old_svx+1,realSeq,old_svx+1,0) ;
						}
						else {
							LOG(WARN,"Fixing SVX %d (expecting %d) at SEQID %d, fixing to %d",svx,old_svx+1,realSeq,old_svx+1,0) ;
						}
						svx = old_svx+1 ;
					}
					else {
						LOG(NOTE,"Err: Unexpected SVX %d (expecting %d) at SEQID %d, chain %c",svx,old_svx+1,realSeq,to_chains[chainID],0) ;
						pp2pp.sec[sec].err[ppSEQID][chainID] = 1 ;
					}
				}
				old_svx = svx ;
				continue;
			}

			if(svx >= MAXSVX) {	// error!
				LOG(ERR,"Err: svx %d >= %d",svx,MAXSVX,0,0,0) ;
				pp2pp.sec[sec].err[ppSEQID][chainID] = 1 ;
				break ;

			}

			chanID = d[cptr+i] & 0x7F ;	// Always even

			// check rising order of channel
			if(chanID <= old_chan) {	// error!
				LOG(NOTE,"Err: SEQ %d, chain %c: chanID %d <= %d",realSeq, to_chains[chainID], chanID,old_chan,0) ;
				pp2pp.sec[sec].err[ppSEQID][chainID] = 1 ;
			}


			// check for overwrites
			if(guard[svx][chanID]) {
				LOG(NOTE,"Err: Data mangled! Would overwrite %d:%c:%d:%d (%d)- skipping",realSeq,to_chains[chainID],svx,chanID,FromGrey[d[cptr+i+1]]) ;
				pp2pp.sec[sec].err[ppSEQID][chainID] = 1 ;
			}
			else {	// all OK...
				// update guard
				if(old_chan >= 0) for(j=old_chan;j<=(u_int)chanID;j++) guard[svx][j] = 1 ;
				else guard[svx][chanID] = 1 ;

				pp2pp.sec[sec].d[ppSEQID][chainID][svx][chanID] = FromGrey[d[cptr+i+1]];


				LOG(DBG,"Data OK: ppSEQ=%2.2d Chain=%c SVX=%1.1d Channel=%3.3d: %u",realSeq,to_chains[chainID],svx,chanID,FromGrey[d[cptr+i+1]]) ;
			}

			old_chan = chanID ;


		}	// loop inside the chain
	


	}

	return 0 ;
}
