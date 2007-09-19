#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <arpa/inet.h>

#include "rtsLog.h"
#include "rtsSystemsEVP.h"
#define qswap16(test,x) ((test)?swap16(x):(x))
#define qswap32(test,x) ((test)?swap32(x):(x))
#include "daqFormats.h"

#include "evpSupport.h"
#include "emcReader.h"

struct emc_t emc ;

int emcReader(char *m) {
  struct DATAP *datap = (struct DATAP *)m ;
  struct DATAPX *datapx ;
  struct EMCP *emcp;
  struct EMCSECP *emcsecp ;
  struct EMCRBP *emcrbp ; 
  struct DUMMYDATA *emcadcr, *emcadcd ;
  char *p, *secp, *rbp, *adcr, *adcd ;
  u_int local_token, token ;
  
  int len, off ;
  int i, j, k ;
  int cou, cou2 ;
  int instance = 0 ;
  int type, id ;
  
  int bytes ;
  
  int swapdatap = 0;
  int swapdatapx = 0;
  int swapemcp = 0; 
  
  memset(emc.btow,0,sizeof(emc.btow));
  memset(emc.etow_pre,0,sizeof(emc.etow_pre));
  memset(emc.etow,0,sizeof(emc.etow));
  memset(emc.esmd_pre,0,sizeof(emc.esmd_pre));
  memset(emc.esmd,0,sizeof(emc.esmd));
  
  if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;
  
  emc.btow_max_ch = 4800  ;
  emc.bsmd_max_ch = 8*4800 ;
  emc.bpre_max_ch = 4800 ;	// unknown...
  
  emc.btow_ch = 0 ;
  emc.bsmd_ch = 0 ;
  emc.bpre_ch = 0 ;
  
  emc.btow_in = emc.bsmd_in = emc.bpre_in = 0 ;
  emc.etow_in = emc.esmd_in = 0 ;
  
  if(datap == NULL) return 0 ;
  
  bytes = 0 ;
  
  // let's first do the Barrel Tower
  for(type=0;type<2;type++) {	// 0 - Barrel, 1 - Endcap
    if(type==0) {
      id = BTOW_ID ;
      p = "EMCP" ;
      secp = "EMCSECP" ;
      rbp = "EMCRBP" ;
      adcr= "EMCADCR" ;
      adcd = "EMCADCD" ;
      
      len = qswap32(swapdatap, datap->det[id].len) * 4 ;
      if(len == 0) continue ;
      
      off = qswap32(swapdatap, datap->det[id].off) ;
      if(off == 0) continue ;
      
    } else {
      id = ETOW_ID ;
      p = "EECP" ;
      secp = "EECSECP" ;
      rbp = "EECRBP" ;
      adcr= "EECADCR" ;
      adcd = "EECADCD" ;
      
      // EEC is in DATAPX...
      len = qswap32(swapdatap, datap->det[EXT_ID].len) ;
      if(len == 0) continue ;	// not even a xtended det
      
      off = qswap32(swapdatap, datap->det[EXT_ID].off) ;
      if(off == 0) continue ;
      
      datapx = (struct DATAPX *)(m + off*4) ;
      
      // verify bank
      if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) continue ;
            
      if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatapx = 1;
      
      len = qswap32(swapdatapx, datapx->det[id-10].len) * 4 ;
      if(len == 0) continue ;
      
      off = qswap32(swapdatapx, datapx->det[id-10].off) ;
      if(off == 0) continue ;
      
      // override "m"
      m = (char *)datapx ;
    }
       
    // LOG(DBG,"EMC %s: bytes %d, off %d",id2char(id),len,off,0,0) ;
    
    bytes += len  ;	// save
    emcp = (struct EMCP *)((u_int *)m + off) ;
    
    if(checkBank(emcp->bh.bank_type,p) < 0) {
      return -1 ;
    }
     
    assert(emcp->bh.byte_order==0x04030201 || emcp->bh.byte_order==0x01020304); // If this
    // assert fails, the data is _VERY_ corrupted.  BIG PROBLEM.
    
    if(emcp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapemcp = 1; else swapemcp = 0;
    
    token = qswap32(swapemcp, emcp->bh.token) ;
    
    // let's see how many contributions (subdetectors) does this event have
    
    for(i=0;i<3;i++) {	// go through subdets
      len = qswap32(swapemcp, emcp->sec[i].len) ;
      if(len == 0) continue ;
      
      instance = i + 1 ;	// EMC subinstances star from 1...
      off = qswap32(swapemcp, emcp->sec[i].off) ;  
      emcsecp = (struct EMCSECP *)((u_int *)emcp + off) ;
      
      if(checkBank(emcsecp->bh.bank_type,secp) < 0) continue;

      cou = (b2h32(emcsecp->bh.length) - 10) / 2 ;	// contributions!
      if(cou<0||cou>999) cou = (l2h32(emcsecp->bh.length) - 10) / 2 ;
      assert(cou>=0&&cou<999);
               
      for(j=0;j<cou;j++) {
	len = b2h32(emcsecp->fiber[j].len) ;
	
	if(len == 0) continue ;
	
	off = l2h32(emcsecp->fiber[j].off) ; // new
	if(off<0||off>9999999) off = b2h32(emcsecp->fiber[j].off) ; // old
	
	emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;
	
	if(checkBank(emcrbp->bh.bank_type,rbp) < 0) continue;
	
	cou2 = (b2h32(emcrbp->bh.length) - 10) /2 ;
	if(cou2<0||cou2>999) cou2 = (l2h32(emcrbp->bh.length) - 10) /2 ;
	assert(cou2>0&&cou2<999);
		
	emcadcr = emcadcd = NULL ;
	
	for(k=0;k<cou2;k++) {
	  len = b2h32(emcrbp->banks[k].len) ;
	  
	  if(len == 0) continue ;
	  
	  off = l2h32(emcrbp->banks[k].off) ;
	  if(off<0||off>9999999) off = b2h32(emcrbp->banks[k].off) ;
	  
	  emcadcr = NULL ;
	  
	  switch(k) {
	  case 0 :	// Raw, ADCR
	    emcadcr = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
	    if(checkBank(emcadcr->bh.bank_type,adcr) < 0) {
	      continue ;
	    }
	    
	    break ;
	  case 1 :	// zero-suppressed...
	    //  This zero-suppressed part causes a segmentation fault.
	    //  I have no easy way to fix the problem -- no documentation and
	    //  buggy example code.  
	    //  According to Tonko's comment below, we only handle RAW data,
	    //  not this zero-suppressed data.  For all these reasons, 
	    //  I will just comment this part out.  Note the "continue" just below.
	    //  emcadcd = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
	    //  if(checkBank(emcadcd->bh.bank_type,adcd) < 0) {
	    //	continue ;
	    //  }
	    
	    break ;
	  default :
	    // LOG(ERR,"Unknown subbank %d in EMCRBP!",k,0,0,0,0) ;
	    continue ;
	  }
	  
	  if(emcadcr == NULL) continue;
	  
	  if((type==0) && (i == EMC_B_TOW)) {	// barrel tower
	    u_short *data ;
	    int l ;
	    
	    emc.btow_in = 1;
	    // get to the data: 40 bytes bank header, 4 bytes dummy,
	    // 128 bytes fiber header...
	    data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
	    
	    emc.btow_raw = data ;
	    data += 120 ;	// skip the preamble of 4X30 shorts
	    
	    for(l=0;l<4800;l++) {
	      emc.btow[l] = l2h16(*data++) ;
	      if(emc.btow[l] > 0) emc.btow_ch++ ;
	    }
	  } else if((type==0) && (i == EMC_B_SMD)) {	// barrel SMD
	    
	    u_short *data ;
	    int l ;
	    
	    emc.bsmd_in = 1;
	    // get to the data: 40 bytes bank header, 4 bytes dummy,
	    // 256 bytes fiber header...
	    data = (u_short *) ((u_int) emcadcr + 40 + 4 + 256) ; 
	    
	    emc.bsmd_cap[j] = *(u_char *)((u_int)emcadcr + 40 + 4 + 4*16) ;
	    for(l=0;l<4800;l++) {
	      emc.bsmd[j][l] = l2h16(*data++) ;
	      if(emc.bsmd[j][l] > 0) emc.bsmd_ch++ ;
	      //LOG(DBG,"BSMD %d: %d",l,emc.bsmd[j][l]) ;
	    }
	    
	  } else if((type=1) && (i == EMC_B_TOW)) { // endcap tower
	    u_short *data ;
	    u_int tlo, thi ;
	    int l,m ;
	    
	    emc.etow_in = 1;
	    // get to the data: 40 bytes bank header, 4 bytes dummy,
	    // 128 bytes fiber header...
	    // ...but first grab the token from the header...
	    data = (u_short *) ((u_int) emcadcr + 40 + 4 + 4) ;
	    thi = l2h16(*data) ;
	    data = (u_short *) ((u_int) emcadcr + 40 + 4 + 6) ;
	    tlo = l2h16(*data) ;
	    
	    local_token = thi * 256 + tlo ;
	    
	    if(token != local_token) { 
	      ;// do nothing here, make 1:1 copy
	    }
	    
	    data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
	    emc.etow_raw = data ;
	    
	    // get the preamble
	    for(m=0;m<ETOW_PRESIZE;m++) {
	      for(l=0;l<ETOW_MAXFEE;l++) {
		emc.etow_pre[l][m] = l2h16(*data++) ;;
	      }
	    }
	    
	    for(m=0;m<ETOW_DATSIZE;m++) {
	      for(l=0;l<ETOW_MAXFEE;l++) {
		emc.etow[l][m] = l2h16(*data++) ;
	      }
	    }
	    
	  }  else if((type==1) && (i == EMC_B_SMD)) {	// endcap SMD

	    // decide ESMD data format: 30 fibers in 2004, 48 later
	    int len2=l2h32(emcadcr->bh.length); // is 2983 for 30 fibers  or 4747 for 48 fibers
	    int mxESMDfiber=48;
	    if( len2 <3000)  mxESMDfiber=30;	   
	    assert(mxESMDfiber<=ESMD_MAXFEE); // not enough space in internal arrays 
	    
	    u_short *data ;
	    u_int tlo, thi ;
	    int l, m ;
	    
	    emc.esmd_in = 1;
	    
	    // get to the data: 40 bytes bank header, 4 bytes dummy,
	    // 128 bytes fiber header...
	    // ...but first grab the token from the header...
	    data = (u_short *) ((u_int) emcadcr + 40 + 4 + 4) ;
	    thi = l2h16(*data) ;
	    data = (u_short *) ((u_int) emcadcr + 40 + 4 + 6) ;
	    tlo = l2h16(*data) ;
	    
	    local_token = thi * 256 + tlo ;
	    
	    if(token != local_token) {
	      ; // do nothing here , make 1:1 copy
	    }
	    
	    data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
	    emc.esmd_raw = data ;
	    
	    // get the preamble
	    for(m=0;m<ESMD_PRESIZE;m++) {
	      for(l=0;l<mxESMDfiber;l++) {
		emc.esmd_pre[l][m] = l2h16(*data++) ;
	      }
	    }
	    
	    for(m=0;m<ESMD_DATSIZE;m++) {
	      for(l=0;l<mxESMDfiber;l++) {
		emc.esmd[l][m] = l2h16(*data++) ;
	      }
	    }
	  }
	}
      }
    }
  }
  
  
  return bytes ;
  
}

