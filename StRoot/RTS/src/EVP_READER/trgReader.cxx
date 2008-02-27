#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>


#include <daqFormats.h>
#include <rtsSystems.h>

#include <evpReader.hh>
#include <evpSupport.h>
#include <trgReader.h>



struct trg trg ;

int DAQtrgReader(char *m);

int trgReader(char *m)
{
  static int oldrun=-1;
  static int oldbytes=-1;
  
  if(m == NULL) return EVP_DATA_ERR ;	// error
  
  evpReader *rrr = (evpReader *)m;
  if(!rrr->mem) {
    LOG(DBG, "No datap:   mem=0x%x sfs=0x%x",rrr->mem,rrr->sfs);
    return EVP_NO_DET;
  }

  if(oldrun == (int)rrr->event_number) {
    return oldbytes;
  };

  oldrun = rrr->event_number;
  oldbytes = DAQtrgReader(rrr->mem);

  return oldbytes;
}

// reads the Triggers event descriptor from DATAP directly...
//
// obsolete!  doesn't work with sfs files...
//
int trgEvtReader(char *m) 
{
	struct EventDescriptor2001 *e ;
	struct DATAP *datap ;

	if(m == NULL) return 0 ;

	datap = (struct DATAP *) m ;

	e = &(datap->evtdes) ;


	return sizeof(datap->evtdes) ;
}

// read the Trigger RAW data
int DAQtrgReader(char *m)
{
	struct TRGP *trgp  ;
	struct TRGD *trgd ;
	struct DATAP *datap ;
	int len, off ;
	int trgp_banks ;

	int swapdatap=0;
	int swaptrgp=0;
	int swaptrgid=0;
	int swaptrgd=0;

	// zap all first
	memset(&trg,0,sizeof(trg)) ;

	trg.mode = 0 ;
	trg.max_channels = 240 * 128 * 6 ;
	trg.channels = 0 ;
	trg.trgc = NULL ;

	if(m == NULL) return EVP_DATA_ERR ;
	datap = (struct DATAP *) m ;
	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) 
	  swapdatap = 1;


	len = qswap32(swapdatap, datap->det[TRG_ID].len);
	if(len == 0) return EVP_NO_DET ;
	len *= 4 ;

	off = qswap32(swapdatap, datap->det[TRG_ID].off);
	if(off == 0) return EVP_NO_DET ;

	LOG(DBG,"Trg raw len %d (0x%x), off %d(0x%x)",len,len,off,off,0) ;

	trgp = (struct TRGP *)((u_int *)m+off) ;
	if(checkBank(trgp->bh.bank_type,"TRGP") < 0) {	// wrong bank!
		return EVP_DATA_ERR ;				
	}

	if(trgp->bh.byte_order != DAQ_RAW_FORMAT_ORDER)
	  swaptrgp = 1;

	if(trgp->bh.token == 0) {
		LOG(DBG,"Token 0 - skipping...",0,0,0,0,0) ;
		return EVP_NO_DET ;
	}

	// number of banks present in TGRP: 1 for TRGD only (old), 2 for TRGD & TRGID (new)
	trgp_banks = qswap32(swaptrgp, trgp->bh.length);

	trgp_banks = (trgp_banks - 10)/2 ;


	LOG(DBG,"TRGP bytes %d, TRGD off:len 0x%X:%d (banks %d)",
	    qswap32(swaptrgp, trgp->bh.length)*4,
	    qswap32(swaptrgp, trgp->trgData.off),
	    qswap32(swaptrgp, trgp->trgData.len),
	    qswap32(swaptrgp, trgp->trgData.len),
	    trgp_banks,0) ;

	// new: TRGID
	if(trgp_banks >= 2) {
	  if(trgp->trgId.len && trgp->trgId.off) {
	    int off = qswap32(swaptrgp, trgp->trgId.off);
	    struct TRGID *trgid = (struct TRGID *)((u_int)trgp + 4*off) ;

	    if(trgid->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swaptrgid = 1;
	    
	    if(checkBank(trgid->bh.bank_type,CHAR_TRGID) >= 0) {
	      int i ;
	  
	      for(i=0;i<32;i++) {
		if(evp_daqbits & (1 << i)) {
		  LOG(DBG,"TRGID %d: bit %2d is 0x%02X [%u dec]",i,i,
		      qswap32(swaptrgid, trgid->triggerId[i]),
		      qswap32(swaptrgid, trgid->triggerId[i]), 0);  
		  trg.offline_id[i] = qswap32(swaptrgid, trgid->triggerId[i]);
		 }
		
	      }
	    }
	  }
	}

	if(trgp->trgData.len == 0) return EVP_NO_DET ;
	if(trgp->trgData.off == 0) return EVP_NO_DET ;

	off = qswap32(swaptrgp, trgp->trgData.off);
	trgd = (struct TRGD *) ((u_int)trgp + 4*off) ;

	if(trgd->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swaptrgd = 1;
	
	// check misc. things
	if(checkBank(trgd->bh.bank_type,"TRGD") < 0) {
	  return EVP_DATA_ERR ;
	}
	
	LOG(DBG,"TRGD len %d",
	    qswap32(swaptrgd, trgd->bh.length),0,0,0) ;

	if(qswap32(swaptrgd, trgd->bh.length) == 10) return EVP_NO_DET ;	// no trigger data - just bh

	
	trg.trgc = (u_char *) trgd ;

	u_char trg_version = trgd->desc.TrgDataFmtVer ;


	switch(trg_version) {
	case 0x12 :	// pre Jan 2002
	case 0x13 :	// pre Jan 2002
		LOG(NOTE,"TRG: version 0x%02X supported...",trg_version,0,0,0,0) ;
		trgReader12((char *)trgd) ;
		break;
	case 0x20 :	// 2002-2003
		LOG(NOTE,"TRG: version 0x%02X supported...",trg_version,0,0,0,0) ;
		trgReader20((char *)trgd) ;
		break ;
	case 0x21 :	// 2003-2004
		LOG(NOTE,"TRG: version 0x%02X supported...",trg_version,0,0,0,0) ;
		trgReader21((char *)trgd) ;
		break ;
	case 0x22 :	// Nov 2004 - BBC extended
		LOG(NOTE,"TRG: version 0x%02X supported...",trg_version,0,0,0,0) ;
		trgReader22((char *)trgd) ;
		break ;
	case 0x30 :   // 30/31 same but for vpd order which was always wrong 
	case 0x31 :   // in 30
	  LOG(NOTE, "TRG: version 0x%02X supported", trg_version,0,0,0,0);
	  trgReader30((char *)trgd);
	  break;

	case 0x10 :
	  LOG(NOTE, "TRG: version 0x%02X supported, ", trg_version, 0,0,0,0);

	  trgReader10((char *)trgd);
	  break;
	
	default :
		LOG(ERR,"Can't read Trigger Format Version 0x%02X!",trg_version,0,0,0,0) ;
		return EVP_DATA_ERR ;
	}


	return len ;
}


