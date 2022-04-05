#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <rts.h>
#include <daqFormats.h>



#include "daq_tof.h" 


enum { ADCD = 0, TDCD, A2DD, SCAD };

const unsigned short gaindata[32] = {
  8, 15, 15, 15,
  15, 15, 15, 15,
  15, 15, 15, 15,
  15, 15, 15, 15,
  15, 15, 15, 15,
  15, 15, 15, 15,
  15, 15, 15, 15,
  15,  0,  0,  0 };

const float decal[5] = {0., 0.38748E-1, 0.332922E-4, 0.20618E-6, -0.2188E-8};
const float calib[7] = {0., 2.5928E-2, -7.602961E-7, 4.637791E-11,
                        -2.165394E-13,6.048144E-20, -7.293422E-25};
const float calib2[7] = {0., 2.5929192E-2, -2.1316967E-7, 7.9018692E-10,
                         4.2527777E-13, 1.330447E-16, 2.0241446E-20};
const float calibration[16] = {
  305.175E-6, 152.580E-6,  0, 76.290E-6,
  0,  38.150E-6, 19.073E-6, 0,
  9.536E-6, 4.786E-6, 0, 2.384E-6,
  1.192E-6, 596.046E-9, 0, 298.023E-9
};




int tof_reader(char *m, struct tof_t *tof, u_int driver) 
{
  struct DATAP *datap = (struct DATAP *)m ;
  struct TOFP *tofp ;
  struct TOFADCD *tofadcd;
  struct TOFTDCD *toftdcd;
  struct TOFA2DD *tofa2dd;
  struct TOFSCAD *tofscad;
  struct TOFDDLR *ddlr ;
  int do_swap  ;
  int data_words ;

  u_int blen ;
  int len, off ;

  tof->mode = 1 ;	
  tof->max_channels = 48+48+32+12 ;	// Tonko: this is old and stale...
  tof->channels = 0 ;

  memset(tof->ddl_words,0,sizeof(tof->ddl_words)) ;	// zap em first.

  if(datap == NULL) return 0 ;

  if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
	do_swap = 1 ;
  }
  else {
	do_swap = 0 ;
  }

  len = (datap->det[TOF_ID].len)  ;
  if(len == 0) return 0 ;

  off = (datap->det[TOF_ID].off) ;
  if(off == 0) return 0 ;


  off = qswap32(do_swap,off) ;
  len = qswap32(do_swap,len) ;
  

  len *= 4 ;	// turn it into bytes

  LOG(DBG,"TOF len %d, off %d",len,off,0,0,0);
	
  tofp = (struct TOFP *)((u_int *)m + off) ;

  if(checkBank(tofp->bh.bank_type,CHAR_TOFP) < 0) {
    return -1 ;
  }

  if(tofp->bh.token == 0) {
    LOG(DBG,"token 0 - skipping ...",0,0,0,0,0);
    return 0;
  }

  u_int fmt_version ;
  if(tofp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
	fmt_version = swap32(tofp->bh.format_ver) ;
  }
  else {
	fmt_version = tofp->bh.format_ver ;
  }

  int max_contrib ;
  if(fmt_version >= 0x50000) {	// new FY05 format
	LOG(NOTE,"TOF FY05+ data version 0x%X",fmt_version) ;
	max_contrib = 8 ;
  }
  else {
	LOG(DBG,"TOF pre-FY05 data version 0x%X",fmt_version) ;
	max_contrib = 4 ;
  }
  

 
  memset(tof->adc,0,sizeof(tof->adc));
  memset(tof->tdc,0,sizeof(tof->tdc));
  memset(tof->a2d,0,sizeof(tof->a2d));
  memset(tof->sca,0,sizeof(tof->sca));

  int bank_t;

  

  for(bank_t=0; bank_t<max_contrib; bank_t++) {
    if(tofp->type[bank_t].len <= 0) continue;
    switch (bank_t) {
    case ADCD:
	tofadcd = (struct TOFADCD *)((u_int *)tofp + l2h32(tofp->type[bank_t].off));
	if(checkBank(tofadcd->bh.bank_type,CHAR_TOFADCD) < 0) {
		return -1;
	}

	blen = l2h32(tofadcd->bh.length) ;
	blen -= sizeof(struct bankHeader)/4 ;

	LOG(DBG,"TOFADCD has %d items",blen,0,0,0,0) ;
	if(blen > sizeof(tof->adc)/sizeof(tof->adc[0])) {
		LOG(ERR,"TOFADCD too big [%d > %d] - skipping!",blen,sizeof(tof->adc)/sizeof(tof->adc[0]),0,0,0) ;
		break ;
	}

	for(u_int i=0;i<blen;i++) {
		tof->adc[i] = (( (l2h32(tofadcd->data[i])) & 0xffff0000)>>16);
		if(tof->adc[i] > 1024) tof->adc[i] = 1024;
		tof->channels++ ;
	}
	break;

    case TDCD:
	toftdcd = (struct TOFTDCD *)((u_int *)tofp + l2h32(tofp->type[bank_t].off));
	if(checkBank(toftdcd->bh.bank_type,CHAR_TOFTDCD) < 0) {
		return -1;
	}

	blen = l2h32(toftdcd->bh.length) ;
	blen -= sizeof(struct bankHeader)/4 ;

	LOG(DBG,"TOFTDCD has %d items",blen,0,0,0,0) ;
	if(blen > sizeof(tof->tdc)/sizeof(tof->tdc[0])) {
		LOG(ERR,"TOFTDCD too big [%d > %d] - skipping!",blen,sizeof(tof->tdc)/sizeof(tof->tdc[0]),0,0,0) ;
		break ;
	}

	for(u_int i=0;i<blen;i++) {
		tof->tdc[i] = (( (l2h32(toftdcd->data[i])) & 0xffff0000)>>16);
		if(tof->tdc[i] > 2048) tof->tdc[i] = 2048;
		// what's so special in channel 41??? Tonko
		if(i==41)tof->tdc[i] = 0;
		tof->channels++ ;
	}
	break;

    case A2DD: // 32 channels
	tofa2dd = (struct TOFA2DD *)((u_int *)tofp + l2h32(tofp->type[bank_t].off));
	if(checkBank(tofa2dd->bh.bank_type,CHAR_TOFA2DD) < 0) {
		return -1;
	}
	tof->a2d[0] = (int)((int) (l2h32(tofa2dd->data[0])) >>16)*calibration[gaindata[0]]/.001-273.;
	tof->channels++;
	for(int i=1;i<29;i++) {
		float V, uncorT;
		if( (i!=13) && (i!= 14) && (i!=15)) {
			V = short( (l2h32(tofa2dd->data[i])) >>16)*calibration[gaindata[i]]/.000001;
			if(V<0) {
				uncorT = calib2[0]+calib2[1]*V+calib2[2]*V*V+calib2[3]*V*V*V+calib2[4]*V*V*V*V+calib2[5]*V*V*V*V*V+calib2[6]*V*V*V*V*V*V;
			} else {
				uncorT = calib[0]+calib[1]*V+calib[2]*V*V+calib[3]*V*V*V+calib[4]*V*V*V*V+calib[5]*V*V*V*V*V+calib[6]*V*V*V*V*V*V;
			}
			tof->a2d[i] = uncorT+tof->a2d[0];
			if(tof->a2d[i] < 0) tof->a2d[i] = 0;
			if(tof->a2d[i] > 100) tof->a2d[i] = 100;
			tof->channels++ ;
		} else {
			tof->a2d[i] = 0;
			tof->channels++ ;
		}
	}
	// threshold channel
	// formula: count = 13.08*X mv +3280
	for(int i=29;i<32;i++) {
		tof->a2d[i] = ((short)((l2h32(tofa2dd->data[i]))>>16)-3280)/13.08;
		if(tof->a2d[i] < 0) tof->a2d[i] = 0;
		tof->channels++ ;
	}
	break;

    case SCAD: // 12 channels
	tofscad = (struct TOFSCAD *)((u_int *)tofp + l2h32(tofp->type[bank_t].off));
	if(checkBank(tofscad->bh.bank_type,CHAR_TOFSCAD) < 0) {
		return -1;
	}
	for(int i=0;i<12;i++) {
		tof->sca[i] = (( (l2h32(tofscad->data[i])) & 0xffffff00)>>8);
		tof->channels++ ;
	}
	break;

    default:  // new DDLR stuff
	ddlr = (struct TOFDDLR *)((u_int *)tofp + l2h32(tofp->type[bank_t].off)) ;
	if(checkBank(ddlr->bh.bank_type,CHAR_TOFDDLR) < 0) {
		return -1 ;
	}
	// do something now...
	LOG(DBG,"TOFDDLR %d: length %d words",bank_t-3,l2h32(ddlr->bh.length)-10) ;
	data_words = l2h32(ddlr->bh.length) - 10 ;
	
	tof->ddl_words[bank_t-4] = data_words ;

	u_int *data_word_p = (u_int *)ddlr->data ;
	for(int i=0;i<data_words;i++) {
		if(i<10000) {
			tof->ddl[bank_t-4][i] = l2h32(data_word_p[i]) ;
		}
		LOG(DBG,"DDLR %d: %3d: 0x%08X",bank_t-3,i,l2h32(data_word_p[i])) ;
	}

      break;
    }
  }
  return len ;

}

