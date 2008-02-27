#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <sys/types.h>

#include <evpReader.hh>
#include <rtsLog.h>
#include <daqFormats.h>
#include <rtsSystems.h>
#include <rts.h>
#include <evp.h>


#include <emcReader.h>

// maps received from Gerard on Mar 17,2006
int btow_crate_map[] = {
  0x12, 0x11, 0x10, 0x1e,
  0x1d, 0x1c, 0x1b, 0x1a,
  0x19, 0x18, 0x17, 0x16,
  0x15, 0x14, 0x13, 0x01,
  0x0f, 0x0e, 0x0d, 0x0c,
  0x0b, 0x0a, 0x09, 0x08,
  0x07, 0x06, 0x05, 0x04,
  0x03, 0x02
} ;

int etow_crate_map[] = {
  0x01, 0x02, 0x03, 0x04, 0x05, 0x06
};

int esmd_crate_map[] = {
  0x40, 0x41, 0x42, 0x43,
  0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x4a, 0x4b,
  0x4c, 0x4d, 0x4e, 0x4f,
  0x50, 0x51, 0x52, 0x53,
  0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5a, 0x5b,
  0x5c, 0x5d, 0x5e, 0x5f,
  0x60, 0x61, 0x62, 0x63,
  0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0x6a, 0x6b,
  0x6c, 0x6d, 0x6e, 0x6f
};

static int sanityCheckDet(int id, char *c_detp) ;
static int tpcCheck(char *mem, int type) ;
static int emcCheck(char *mem, int type) ;

static int tofCheck(char *mem) ;
static int pmdCheck(char *mem) ;
static int fpdCheck(char *mem) ;

static int trgCheck(char *mem) ;
static int l3Check(char *mem) ;

// there is grave danger when using threads!!!
static jmp_buf evp_jmp_env ;


static struct sanity_struct {
	// global event things...
	u_int evt_count ;	
	u_short token ;
	u_char trg_cmd ;
	u_char daq_cmd ;

	struct {
		u_int evt_count ;
	} etow, esmd, btow ;

} san ;

static void evp_sig_handler(int signum)
{
	LOG(DBG,"Caught signal %d...",signum) ;

	if((signum==SIGSEGV) || (signum==SIGBUS)) {
		siglongjmp(evp_jmp_env,signum) ;
	}
	else {
		LOG(WARN,"Caught signal %d",signum) ;
	}


	return ;
}

int is_bus_error(const char *addr)
{	
	static struct sigaction saved_act ;
	struct sigaction newact ;
	volatile char blah = 0 ;

	int ret = sigsetjmp(evp_jmp_env,1) ;
	if(ret) {	// bus error occured!
		sigaction(SIGSEGV,&saved_act,0) ;	// restore old
		LOG(WARN,"BUS ERROR at address 0x%08X: signal %d (ignore %d)",addr,ret,blah) ;
		return -1 ;		
	}


	// hook handler for bus error: BE CAREFUL with other PROGRAMS!
	newact.sa_handler = evp_sig_handler ;
	sigaction(SIGSEGV,&newact,&saved_act) ;	// save old one...

//	signal(SIGSEGV,evp_sig_handler) ;
//	signal(SIGBUS,evp_sig_handler) ;


//	LOG(DBG,"Checking address 0x%08X",addr) ;
	blah = *addr ;	// dummy read ;
	
	// all OK here
	sigaction(SIGSEGV,&saved_act,0) ;	// restore old...
	
//	LOG(DBG,"Value at 0x%08X is %d",addr,blah) ;

	return 0 ;
}

static void is_swap(void *vbh, u_int *dta)
{
	struct bankHeader *bh = (struct bankHeader *) vbh ;
	
	if(bh->byte_order != DAQ_RAW_FORMAT_ORDER) *dta = swap32(*dta) ;
}


static int chBank(const char *m, const char *what) 
{
	char bank[10] ;


	if(is_bus_error(m)) {
		LOG(ERR,"BUS Error at 0x%08X while expecting [%s]",m,what) ;
		return -1 ;
	}

	memcpy(bank,m,8) ;
	bank[8] = 0 ;

	if(memcmp(m,what,strlen(what)) != 0) {
		int i ;
		for(i=0;i<8;i++) {
			if(isprint(*(bank+i))) ;	// do nothing
			else *(bank+i) = '*' ;
		}
		LOG(CAUTION,"Wrong bank type: expecting [%s], is [%s]",what,bank) ;
		return -1 ;
	}
	else {
		LOG(DBG,"Bank Compare for [%s] OK...",what) ;
	}

	return 0 ;
}


// called at start run!
int sanityCheckStartRun(void)
{
	memset(&san,0,sizeof(san)) ;


	return 0 ;
}


int DAQsanityCheck(char *m);

int sanityCheck(char *m)
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
  oldbytes = DAQsanityCheck(rrr->mem);

  return oldbytes;
}


int DAQsanityCheck(char *c_datap) 
{
	u_int off, len ;
	int swp ;
	int i, j ;

	struct DATAP *datap = (struct DATAP *)c_datap ;

	if(chBank(datap->bh.bank_type,CHAR_DATAP)) return -1 ;

	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {	// need swaps...
		swp = 1 ;	// must swap DATAP & DATAPX entries...
	}
	else {
		swp = 0 ;
	}

	u_int bad = 0 ;	
	for(i=0;i<10;i++) {
		off = datap->det[i].off ;
		len = datap->det[i].len ;

		if(swp) {
			off = swap32(off) ;
			len = swap32(len) ;
		}

		if(off && len) {
			if(sanityCheckDet(i, c_datap + 4*off) < 0) bad |= (1<<i) ;
			else if(i==EXT_SYSTEM) {
				struct DATAPX *datapx = (struct DATAPX *)(c_datap + 4*off) ;
				for(j=0;j<22;j++) {
					off = datapx->det[j].off ;
					len = datapx->det[j].len ;

					if(swp) {
						off = swap32(off) ;
						len = swap32(len) ;
					}

					if(off && len) {
						if(sanityCheckDet(j+10, (char *)datapx + 4*off)) bad |= (1<<(j+10)) ;
					}
				}

			}
		}
	}

	if(bad) {
		LOG(ERR,"Bad detectors mask is 0x%08X",bad) ;
	}

	return bad ;

} 


static int sanityCheckDet(int id, char *c_detp)
{

	
	switch(id) {
	case TPC_SYSTEM :
		if(chBank(c_detp,CHAR_TPCP)) return -1 ;
		return tpcCheck(c_detp,id) ;
		break ;
	case SVT_SYSTEM :
		if(chBank(c_detp,CHAR_SVTP)) return -1 ;
		return tpcCheck(c_detp,id) ;
		break ;
	case TOF_SYSTEM :
		if(chBank(c_detp,CHAR_TOFP)) return -1 ;
		return tofCheck(c_detp) ;
		break ;
	case BTOW_SYSTEM :
		if(chBank(c_detp,CHAR_EMCP)) return -1 ;
		return emcCheck(c_detp,id) ;
		break ;
	case FPD_SYSTEM :
		if(chBank(c_detp,CHAR_FPDP)) return -1 ;
		return fpdCheck(c_detp) ;
		break ;
	case FTP_SYSTEM :
		if(chBank(c_detp,CHAR_FTPP)) return -1 ;
		return tpcCheck(c_detp,id) ;
		break ;
	case EXT_SYSTEM :
		if(chBank(c_detp,CHAR_DATAPX)) return -1 ;
		break ;
	case RIC_SYSTEM :
		if(chBank(c_detp,CHAR_RICP)) return -1 ;
		break ;
	case TRG_SYSTEM :
		if(chBank(c_detp,CHAR_TRGP)) return -1 ;
		return trgCheck(c_detp) ;
		break ;
	case L3_SYSTEM :
		if(chBank(c_detp,CHAR_L3_P)) return -1 ;
		return l3Check(c_detp) ;
		break ;
	case SC_SYSTEM :
		if(chBank(c_detp,CHAR_SCD)) return -1 ;
		break ;
	case EXT2_SYSTEM :
		if(chBank(c_detp,"DATAPX2")) return -1 ;
		break ;
	case PMD_SYSTEM :
		if(chBank(c_detp,CHAR_PMDP)) return -1 ;
		return pmdCheck(c_detp) ;
		break ;
	case SSD_SYSTEM :
		if(chBank(c_detp,CHAR_SSDP)) return -1 ;
		return tpcCheck(c_detp,id) ;
		break ;
	case ETOW_SYSTEM :
		if(chBank(c_detp,CHAR_EECP)) return -1 ;
		return emcCheck(c_detp, id) ;
		break ;
	case FP2_SYSTEM :
		if(chBank(c_detp,CHAR_FP2P)) return -1 ;
		break ;
	case TPX_ID:
	  //if(chBank(c_detp, "TPXD    ")) return -1;
	  break;
	default :
		LOG(WARN,"Don't know how to check RTS System %d - assuming OK",id) ;
		return 0 ;
	}


	return 0 ;
}

static char c_bh[4][20][20] = {
// TPC 
{
CHAR_TPCP,
CHAR_TPCSECP,
CHAR_TPCSECLP,
CHAR_TPCRBP,
CHAR_TPCRBCLP,
CHAR_TPCMZP,

CHAR_TPCADCD,	//6
CHAR_TPCSEQD,	
CHAR_TPCADCX,	//8
CHAR_TPCPADK,
CHAR_TPCCPPR,	//10
CHAR_TPCADCR,
CHAR_TPCMZCLD,	//12
CHAR_TPCCFGR,
CHAR_TPCPEDR,	// 14
CHAR_TPCRMSR,
CHAR_TPCGAINR,	// 16
CHAR_TPCBADR,	//17

},
// SVT
{
CHAR_SVTP,
CHAR_SVTSECP,
"NOEXIST",
CHAR_SVTRBP,
"NOEXIST",
CHAR_SVTMZP,

CHAR_SVTADCD,	//6
CHAR_SVTSEQD,	
CHAR_SVTADCX,	//8
CHAR_SVTANODK,
CHAR_SVTCPPR,	//10
CHAR_SVTADCR,
CHAR_SVTMZCLD,	//12
CHAR_SVTCFGR,
CHAR_SVTPEDR,	// 14
CHAR_SVTRMSR,
CHAR_SVTGAINR,	// 16
CHAR_SVTBADR,	//17

},
// FTP
{
CHAR_FTPP,
CHAR_FTPSECP,
"NOEXIST",
CHAR_FTPRBP,
"NOEXIST",
CHAR_FTPMZP,

CHAR_FTPADCD,	//6
CHAR_FTPSEQD,	
CHAR_FTPADCX,	//8
CHAR_FTPPADK,
CHAR_FTPCPPR,	//10
CHAR_FTPADCR,
CHAR_FTPMZCLD,	//12
CHAR_FTPCFGR,
CHAR_FTPPEDR,	// 14
CHAR_FTPRMSR,
CHAR_FTPGAINR,	// 16
CHAR_FTPBADR,	//17

},
// SSD
{
CHAR_SSDP,
CHAR_SSDSECP,
"NOEXIST",
CHAR_SSDRBP,
"NOEXIST",
CHAR_SSDMZP,

CHAR_SSDADCD,	//6
CHAR_SSDSEQD,	
CHAR_SSDADCX,	//8
CHAR_SSDPADK,
CHAR_SSDCPPR,	//10
CHAR_SSDADCR,
CHAR_SSDMZCLD,	//12
CHAR_SSDCFGR,
CHAR_SSDPEDR,	// 14
CHAR_SSDRMSR,
CHAR_SSDGAINR,	// 16
CHAR_SSDBADR,	//17

}
} ;


static int tpcCheck(char *c_tpcp, int type)
{
	u_int off, len ;
	int sec ;
	int secnum, rbnum ;
	int err = 0 ;

	struct TPCP *tpcp = (struct TPCP *) c_tpcp ;


	switch(type) {
	case TPC_ID :
		type = 0 ;
		break ;
	case SVT_ID :
		type = 1 ;
		break ;
	case FTP_ID :
		type = 2 ;
		break ;
	case SSD_ID :
		type = 3 ;
		break ;
	}

	LOG(DBG,"Checking type %d",type) ;

	len = tpcp->bh.length ;
	if(tpcp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) len = swap32(len) ;

	len -= sizeof(tpcp->bh)/4 ;

	secnum = len/2 ;

	LOG(DBG,"SECTORS %d",secnum) ;

	for(sec=0;sec<secnum;sec++) {
		off = tpcp->sb[sec].off ;
		len = tpcp->sb[sec].len ;

		if(tpcp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) off = swap32(off) ;

		LOG(DBG,"Checking sector %d",sec) ;

		if(off && len) {
			int rb ;

			struct TPCSECP *secp = (struct TPCSECP *) ((char *)tpcp + 4*off) ;

			if(chBank((char *)secp, c_bh[type][1])) {
				err = 1 ;
				continue ;
			}

			u_int f_num = secp->bh.format_number ;
			u_int w9 = secp->bh.w9 ;

			if(secp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
				f_num = swap32(f_num) ;
				w9 = swap32(w9) ;
			}

			if((f_num == 2) && w9) {	// SECLP
				struct TPCSECLP *seclp = (struct TPCSECLP *) ((char *)secp + 4*w9) ;

				if(chBank((char *)seclp, c_bh[type][2])) err = 1 ;
				else {
					len = seclp->bh.length ;
					if(seclp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) len = swap32(len) ;

					len -= sizeof(struct bankHeader)/4 ;
					rbnum = len/2 ;

					LOG(DBG,"SECLP rbnum %d",rbnum) ;

					for(rb=0;rb<rbnum;rb++) {
						int mz ;

						off = seclp->rb[rb].off ;
						len = seclp->rb[rb].len ;

						if(!(off && len)) continue ;

						if(seclp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) off = swap32(off) ;

						struct TPCRBCLP *rbclp = (struct TPCRBCLP *) ((char *)seclp+4*off) ;

						if(chBank((char *)rbclp, c_bh[type][4])) {
							err = 1 ;
							continue ;
						}

						for(mz=0;mz<3;mz++) {

							off = rbclp->mz[mz].off ;
							len = rbclp->mz[mz].len ;

							if(rbclp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) off = swap32(off) ;

							struct TPCMZCLD *mzcld = (struct TPCMZCLD *) ((char *)rbclp+4*off) ;

							if(chBank((char *)mzcld, c_bh[type][12])) {
								err = 1 ;
								continue ;
							}
						}
						

					}

				}

			}


			len = secp->bh.length ;
			if(secp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) len = swap32(len) ;

			len -= sizeof(struct bankHeader)/4 ;
			rbnum = len/2 ;

			LOG(DBG,"SECP rbnum %d",rbnum) ;

			for(rb=0;rb<rbnum;rb++) {
				off = secp->rb[rb].off ;
				len = secp->rb[rb].len ;

				if(secp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) off = swap32(off) ;

				LOG(DBG,"Checking RB %d",rb) ;

				if(off && len) {
					int mz ;

					struct TPCRBP *rbp = (struct TPCRBP *) ((char *)secp + 4*off) ;

					if(chBank((char *)rbp, c_bh[type][3])) {
						err = 1 ;
						continue ;
					}
					
					for(mz=0;mz<3;mz++) {
						off = rbp->mz[mz].off ;
						len = rbp->mz[mz].len ;


						if(rbp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) off = swap32(off) ;

						LOG(DBG,"Checking sec %d, RB %d, MZ %c",sec+1,rb+1,'A'+mz) ;

						if(off && len) {
							u_int j ;

							struct TPCMZP *mzp = (struct TPCMZP *) ((char *)rbp + 4*off) ;

							if(chBank((char *)mzp, c_bh[type][5])) {
								err = 1 ;
								continue ;
							}

							u_int banks = mzp->bh.length ;

							if(mzp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) banks = swap32(banks) ;

							banks = (banks - 10) / 2 ;	// get the number of banks present...
							
							if(banks > TPC_MZP_BANKS_NUM) {
								LOG(CAUTION,"Banks %d > expected %d!",banks, TPC_MZP_BANKS_NUM) ;
								err = 1 ;
								continue ;
							}

							for(j=0;j<banks;j++) {
								off = mzp->banks[j].off ;
								len = mzp->banks[j].len ;

								if(mzp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) off = swap32(off) ;


								if(off && len) {

									switch(j) {
									case TPC_ADCD :
										if(chBank((char *)mzp+4*off, c_bh[type][6])) {
											err = 1 ;
										}
										break ;
									case TPC_SEQD :
										if(chBank((char *)mzp+4*off, c_bh[type][7])) {
											err = 1 ;
										}
										break ;
									case TPC_ADCX :
										if(chBank((char *)mzp+4*off,c_bh[type][8])) {
											err =1 ;
										}
										break ;
									case TPC_PADK :
										if(chBank((char *)mzp+4*off,c_bh[type][9])) { 
											err=1 ;
										}
										break ;
									case TPC_CPPR :
										if(chBank((char *)mzp+4*off,c_bh[type][10])) {
											err = 1 ;
										}
										break ;
									case TPC_ADCR :
										if(chBank((char *)mzp+4*off,c_bh[type][11])) err =1 ;
										break ;
									case TPC_MZCLD :
										if(chBank((char *)mzp+4*off,c_bh[type][12])) err=1 ;
										break ;
									case TPC_CFGR :
										if(chBank((char *)mzp+4*off,c_bh[type][13])) err=1 ;
										break ;
									case TPC_PEDR :
										if(chBank((char *)mzp+4*off,c_bh[type][14])) err=1 ;
										break ;
									case TPC_RMSR :
										if(chBank((char *)mzp+4*off,c_bh[type][15])) err=1 ;
										break ;
									case TPC_GAINR :
										if(chBank((char *)mzp+4*off,c_bh[type][16])) err=1 ;
										break ;
									case TPC_BADR :
										if(chBank((char *)mzp+4*off,c_bh[type][17])) err=1 ;
										break ;
									default:
										if(chBank((char *)mzp + 4*off,"UNKOWN")) err = 1;
										break ;
									}
								}
							}		
						}							
					}
				}
			}
		}

	}
	if(err) return -1 ;
	return 0 ;
}

static char c_pmd[4][10] = {
	CHAR_PMDADCD,
	CHAR_PMDPEDR,
	CHAR_PMDRMSR,
	CHAR_PMDTHRR
} ;

static int pmdCheck(char *c_p)
{
	int err ;
	int sec, type ;
	u_int off, len ;

	err = 0 ;

	struct PMDP *pmdp = (struct PMDP *) c_p ;

	for(sec=0;sec<2;sec++) {
		off = pmdp->sec[sec].off ;
		len = pmdp->sec[sec].len ;

		if(off && len) {
			is_swap(pmdp, &off) ;

			struct PMDSECP *secp = (struct PMDSECP *)((char *)pmdp + 4*off) ;

			if(chBank((char *)secp, CHAR_PMDSECP)) {
				err = 1 ;
				continue ;
			}

			for(type=0;type<4;type++) {
				off = secp->type[type].off ;
				len = secp->type[type].len ;

				if(off && len) {

					is_swap(secp, &off) ;

					struct bankHeader *bh = (struct bankHeader *)((char *)secp + 4*off) ;

					if(chBank((char *)bh, c_pmd[type])) {
						err = 1 ;
					}
				}

			}
		}

	}

	if(err) return -1 ;
	return 0 ;
}


static char c_tof[8][10] = {
	CHAR_TOFADCD,
	CHAR_TOFTDCD,
	CHAR_TOFA2DD,
	CHAR_TOFSCAD,
	CHAR_TOFDDLR,
	CHAR_TOFDDLR,
	CHAR_TOFDDLR,
	CHAR_TOFDDLR
} ;

static int tofCheck(char *c_p)
{
	int err ;
	int i ;
	u_int off, len ;
	int max_types ;

	err = 0 ;

	struct TOFP *tofp = (struct TOFP *) c_p ;

	// the number of fiber depends on the format version!
	u_int ver = tofp->bh.format_ver ;
	is_swap(tofp,&ver) ;
	
	if(ver >= 0x50000) max_types = 8 ;
	else max_types = 4 ;

	LOG(DBG,"Version is 0x%X",ver) ;

	for(i=0;i<max_types;i++) {	// only 8 banks
		off = tofp->type[i].off ;
		len = tofp->type[i].len ;

		if(off && len) {
			is_swap(tofp, &off) ;

			struct bankHeader *bh = (struct bankHeader *)((char *)tofp + 4*off) ;
			if(chBank((char *)bh, c_tof[i])) {
				err = 1 ;
			}
		}
	}

	if(err) return -1 ;

	return 0 ;
}


static char c_fpd[FPDP_MAX_BANKS][10] = {
	CHAR_FPDADCD,
	CHAR_FPDTDCD,
	CHAR_FPDREGD,
	CHAR_FPDPEDR,
	CHAR_FPDSCL,
	CHAR_BBCDAT,
	CHAR_BBCPED,
	CHAR_BBCSCL
} ;

static int fpdCheck(char *c_p)
{
	int err ;
	int i ;
	u_int off, len ;

	err = 0 ;

	struct FPDP *fpdp = (struct FPDP *) c_p ;

	for(i=0;i<FPDP_MAX_BANKS;i++) {
		off = fpdp->type[i].off ;
		len = fpdp->type[i].len ;

		if(off && len) {
			is_swap(fpdp, &off) ;

			struct bankHeader *bh = (struct bankHeader *)((char *)fpdp + 4*off) ;
			if(chBank((char *)bh, c_fpd[i])) {
				err =1 ;
			}
		}
	}

	if(err) return -1 ;

	return 0 ;
}



static int emcCheck(char *c_p, int type)
{
	u_int sec, secnum ;
	u_int f, fbrnum ;
	u_int b, banknum ;
	u_int off, len ;
	char *c_secp ;
	char *c_rbp ;
	char *c_bank ;

	int err ;

	err = 0 ;

	if(type == BTOW_ID) {
		c_secp = CHAR_EMCSECP ;
		c_rbp = CHAR_EMCRBP ;
		c_bank = CHAR_EMCADCR ;
	}
	else {
		c_secp = CHAR_EECSECP ;
		c_rbp = CHAR_EECRBP ;
		c_bank = CHAR_EECADCR ;
	}

	struct EMCP *emcp = (struct EMCP *) c_p ;

	len = emcp->bh.length ;

	is_swap(emcp,&len) ;

	len -= sizeof(struct bankHeader)/4 ;
	secnum = len/2 ;

	if(secnum > 6) {
		LOG(CAUTION,"Sectors %d > expected %d!",secnum,6) ;
		return -1 ;
	}
	


	for(sec=0;sec<secnum;sec++) {
		off = emcp->sec[sec].off ;
		len = emcp->sec[sec].len ;

		is_swap(emcp, &off) ;

		if(off && len) {
			struct EMCSECP *secp = (struct EMCSECP *) ((char *)emcp + 4*off) ;

			if(chBank((char *)secp, c_secp)) {
				err = 1 ;
				continue ;
			}

			len = secp->bh.length ;
			is_swap(secp, &len) ;

			fbrnum = (len-10)/2 ;

			if(fbrnum > 12) {
				LOG(CAUTION,"EMC sector %d: fibers %d?",sec,fbrnum) ;
				err = 1 ;
				continue ;
			}

			for(f=0;f<fbrnum;f++) {
				off = secp->fiber[f].off ;
				len = secp->fiber[f].len ;

				if(off && len) {
					is_swap(secp,&off) ;

					struct EMCRBP *rbp = (struct EMCRBP *)((char *)secp + 4*off) ;
				
					if(chBank((char *)rbp, c_rbp)) {
						err = 1 ;
						continue ;
					}


					len = rbp->bh.length ;
					is_swap(rbp, &len) ;

					banknum = (len-10)/2 ;
					if(banknum > TPC_MZP_BANKS_NUM) {
						err = 1 ;
						LOG(CAUTION,"EMC sector %d, fiber %d: banknum is %d and not %d",sec,f,banknum,TPC_MZP_BANKS_NUM) ;
						continue ;
					}


					for(b=0;b<banknum;b++) {
						off = rbp->banks[b].off ;
						len = rbp->banks[b].len ;

						if(off && len) {
							is_swap(rbp, &off) ;

							struct DUMMYDATA *adcr = (struct DUMMYDATA *)((char *)rbp + 4*off) ;

							if(chBank((char *)adcr, c_bank)) {
								err = 1 ;
								continue ;
							}

							// add _specific_ header corruption checks here...
							if(b!=0) continue ; // I know of only the first bank - EMCADCR
								
							// ETOW
							if((type != BTOW_ID) && (sec==0)) {
								LOG(DBG,"ETOW") ;
								static u_int err_count ;

								static u_short last_dta[ETOW_MAXFEE][ETOW_PRESIZE] ;
								u_short dta[ETOW_MAXFEE][ETOW_PRESIZE] ;

								u_short *data ;
								u_int tlo, thi ;
								int l, m ;

								// increment etow count
								san.etow.evt_count++ ;

								// get the fiber local token and stuff it into "tlo"
								data = (u_short *)((u_int)adcr + 40 + 4 + 4) ;
								thi = l2h16(*data) ;
								data = (u_short *)((u_int)adcr + 40 + 4 + 6) ;
								tlo = l2h16(*data) ;
								
								tlo = thi * 256 + tlo ;	// fiber token

								data = (u_short *)((u_int)adcr + 40 + 4 + 128) ;


								// copy over the fiber headers...
								for(m=0;m<ETOW_PRESIZE;m++) {
									for(l=0;l<ETOW_MAXFEE;l++) {
										dta[l][m] = l2h16(*data++) ;
									}
								}


								if(san.etow.evt_count == 1) {	// reset on the first event of the run...
									err_count = 0 ;
									memcpy(last_dta,dta,sizeof(last_dta)) ;
								}

								u_int bad = 0 ;

								for(l=0;l<ETOW_MAXFEE;l++) {
									if((dta[l][0] != 0xFFF) && (dta[l][1] != 0xFFF)) {	// "good" fiber?
										if(dta[l][0] != 0x00A4) bad |= 1 << l ;
										if(dta[l][1] != 0) bad |= 1 << l ;
										if(dta[l][2] != tlo) bad |= 1 << l;
									
										int trg = (dta[l][3] >> 8) & 0xFF ;
										if((trg<4) || (trg>12)) bad |= 1 << l;

										if((dta[l][3] & 0xFF) != etow_crate_map[l]) bad |= 1 << l ;
										//if(dta[l][3] != ((trg << 8) | (l+1))) bad |= 1 << l;
									
									}
									else if((last_dta[l][0] != dta[l][0]) || (last_dta[l][1] != dta[l][1])) {
										// both are FFF but this was not the case for the prev. event!
										bad |= 1<<l ;
									}

								}
										
								// remember last...

								memcpy(last_dta,dta,sizeof(last_dta)) ;

								if(bad) err_count++ ;

								if(err_count < 100) {	// detailed log
									for(l=0;l<ETOW_MAXFEE;l++) {
										if(bad & (1<<l)) {
											LOG(CAUTION,"ETOW header corrupt (%u/%u) in TDC channel %2d [crate ID 0x%02X]: fiber token 0x%X: 0x%X 0x%X 0x%X 0x%X",
											    err_count,san.etow.evt_count,l,etow_crate_map[l],tlo,dta[l][0],dta[l][1],dta[l][2],dta[l][3]) ;
										}
										else {
											LOG(DBG,"ETOW header OK (%u/%u) in TDC channel %2d [crate ID 0x%02X]: fiber token 0x%X: 0x%X 0x%X 0x%X 0x%X",
											    err_count,san.etow.evt_count,l,etow_crate_map[l],tlo,dta[l][0],dta[l][1],dta[l][2],dta[l][3]) ;
										}
									}
								}
								else if(err_count == 100) {	// tell the operator...
									LOG(CAUTION,"ETOW has too many corrupt events (%u/%u) -- stopping detailed logging...!",
									    err_count,san.etow.evt_count) ;
								}
								else if((err_count % 1000)==0) { //
									LOG(CAUTION,"ETOW has many, many errors (%u/%u)...",
									    err_count,san.etow.evt_count) ;
								}

							}
							else if((type != BTOW_ID) && (sec==1)) {	// ESMD
								LOG(DBG,"ESMD") ;

								static u_int err_count ;

								static u_short last_dta[ESMD_MAXFEE][ESMD_PRESIZE] ;
								u_short dta[ESMD_MAXFEE][ESMD_PRESIZE] ;

								u_short *data ;
								u_int tlo, thi ;
								int l, m ;

								// increment esmd count
								san.esmd.evt_count++ ;

								// get the fiber local token and stuff it into "tlo"
								data = (u_short *)((u_int)adcr + 40 + 4 + 4) ;
								thi = l2h16(*data) ;
								data = (u_short *)((u_int)adcr + 40 + 4 + 6) ;
								tlo = l2h16(*data) ;
								
								tlo = thi * 256 + tlo ;	// fiber token

								data = (u_short *)((u_int)adcr + 40 + 4 + 128) ;

								

								// copy over the fiber headers...
								for(m=0;m<ESMD_PRESIZE;m++) {
									for(l=0;l<ESMD_MAXFEE;l++) {
										dta[l][m] = l2h16(*data++) ;
									}
								}


								if(san.esmd.evt_count == 1) {	// reset on the first event of the run...
									err_count = 0 ;
									memcpy(last_dta,dta,sizeof(last_dta)) ;
								}

								unsigned long long bad = 0 ;

								for(l=0;l<ESMD_MAXFEE;l++) {
									if((dta[l][0] != 0xFFF) && (dta[l][1] != 0xFFF)) {	// "good" fiber?
										

										if(dta[l][0] != 0x00C4) bad |= 1ll << (long long)l ;
										//if(dta[l][1] != 0x28) bad |= 1ll << (long long)l ;
										if(dta[l][1] != 0) bad |= 1ll << (long long)l ;
										if(dta[l][2] != tlo) bad |= 1ll << (long long)l;
									
										int trg = (dta[l][3] >> 8) & 0xFF ;
										if((trg<4) || (trg>12)) bad |= 1ll << (long long)l;

										if((dta[l][3] & 0xFF) != esmd_crate_map[l]) bad |= 1ll << (long long)l ;
										//if(dta[l][3] != ((trg << 8) | 0x40 | (l+0))) bad |= 1ll << (long long)l;

										
									
									}
									else if((last_dta[l][0] != dta[l][0]) || (last_dta[l][1] != dta[l][1])) {
										// both are FFF but this was not the case for the prev. event!
										bad |= 1ll << l ;

										
										
									}

								}
										

								if(bad) {
									err_count++ ;
								}

								if(err_count < 100) {	// detailed log
									for(l=0;l<ESMD_MAXFEE;l++) {
										if(bad & (1ll << l)) {
											LOG(CAUTION,"ESMD header corrupt (%u/%u) in TDC channel %2d [crate ID 0x%02X]: fiber token 0x%X: 0x%X 0x%X 0x%X 0x%X",
											    err_count,san.esmd.evt_count,l,esmd_crate_map[l],tlo,dta[l][0],dta[l][1],dta[l][2],dta[l][3]) ;
										}
										else {
											LOG(DBG,"ESMD header OK (%u/%u) in TDC channel %2d [crate ID 0x%02X]: fiber token 0x%X: 0x%X 0x%X 0x%X 0x%X",
											    err_count,san.esmd.evt_count,l,esmd_crate_map[l],tlo,dta[l][0],dta[l][1],dta[l][2],dta[l][3]) ;
										}
									}
								}
								else if(err_count == 100) {	// tell the operator...
									LOG(CAUTION,"ESMD has too many corrupt events (%u/%u) -- stopping detailed logging...!",
									    err_count,san.esmd.evt_count) ;
								}
								else if((err_count % 1000)==0) { //
									LOG(CAUTION,"ESMD has many, many errors (%u/%u)...",
									    err_count,san.esmd.evt_count) ;
								}

								// remember last...
								memcpy(last_dta,dta,sizeof(last_dta)) ;



							}
							else if((type == BTOW_ID) && (sec == 0)) {	// BTOW
								LOG(DBG,"BTOW") ;

								/* dead
								static int btow_fee_id[BTOW_MAXFEE] = {
									18,17,16,30,29,28,27,26,25,24,23,22,21,20,19,
									1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
								} ;
								*/

								static u_int err_count ;

								static u_short last_dta[BTOW_MAXFEE][BTOW_PRESIZE] ;
								u_short dta[BTOW_MAXFEE][BTOW_PRESIZE] ;

								u_short *data ;
								u_int tlo, thi ;
								int l, m ;

								// increment btow count
								san.btow.evt_count++ ;

								// get the fiber local token and stuff it into "tlo"
								data = (u_short *)((u_int)adcr + 40 + 4 + 4) ;
								thi = l2h16(*data) ;
								data = (u_short *)((u_int)adcr + 40 + 4 + 6) ;
								tlo = l2h16(*data) ;
								
								tlo = thi * 256 + tlo ;	// fiber token

								data = (u_short *)((u_int)adcr + 40 + 4 + 128) ;


								// copy over the fiber headers...
								for(m=0;m<BTOW_PRESIZE;m++) {
									for(l=0;l<BTOW_MAXFEE;l++) {
										dta[l][m] = l2h16(*data++) ;
									}
								}


								if(san.btow.evt_count == 1) {	// reset on the first event of the run...
									err_count = 0 ;
									memcpy(last_dta,dta,sizeof(last_dta)) ;
								}

								u_int bad = 0 ;

								
								for(l=0;l<BTOW_MAXFEE;l++) {
									if((dta[l][0] != 0xFFF) && (dta[l][1] != 0xFFF)) {	// "good" fiber?
										u_int rsn = 0 ;

										if(dta[l][0] != 0x00A4) {
											rsn |= 1 ;
											bad |= 1 << l ;
										}
										if(dta[l][1] != 0) {
											rsn |= 2 ;
											bad |= 1 << l ;
										}
										if(dta[l][2] != tlo) {
											bad |= 1 << l;
											rsn |= 4 ;
										}
										int trg = (dta[l][3] >> 8) & 0xFF ;
										if((trg<4) || (trg>12)) {
											bad |= 1 << l;
											rsn |= 8 ;
										}


										if((dta[l][3] & 0xFF) != btow_crate_map[l]) {
											bad |= 1 << l;
											rsn |= 0x10 ;
										}

										//if((dta[l][3] & 0xF00) != (trg << 8)) {
										//	bad |= 1 << l ;
										//	rsn |= 0x10 ;
										//}

										//if(dta[l][3] != ((trg << 8) | btow_fee_id[l])) {
										//	bad |= 1 << l;
										//	rsn |= 0x10 ;
										//}

										//printf("Really %d 0x%X    0x%X?\n",l+1,rsn,((trg << 8) | 0x10 | (l))) ;
									
									}
									else if((last_dta[l][0] != dta[l][0]) || (last_dta[l][1] != dta[l][1])) {
										// both are FFF but this was not the case for the prev. event!
										bad |= 1<<l ;
									}

									//printf("BTOW FEE %d: 0x%04X 0x%04X 0x%04X 0x%04X -> bad 0x%X\n",l+1,dta[l][0],dta[l][1],dta[l][2],dta[l][3],bad) ;

								}
										
								// remember last...

								memcpy(last_dta,dta,sizeof(last_dta)) ;

								if(bad) err_count++ ;

								if(err_count < 100) {	// detailed log
									for(l=0;l<BTOW_MAXFEE;l++) {
										if(bad & (1<<l)) {
											LOG(CAUTION,"BTOW header corrupt (%u/%u) in TDC channel %2d [crate ID 0x%02X]: fiber token 0x%X: 0x%X 0x%X 0x%X 0x%X",
											    err_count,san.btow.evt_count,l,btow_crate_map[l],tlo,dta[l][0],dta[l][1],dta[l][2],dta[l][3]) ;
										}
										else {
											LOG(DBG,"BTOW header OK (%u/%u) in TDC channel %2d [crate ID 0x%02X]: fiber token 0x%X: 0x%X 0x%X 0x%X 0x%X",
											    err_count,san.btow.evt_count,l,btow_crate_map[l],tlo,dta[l][0],dta[l][1],dta[l][2],dta[l][3]) ;
										}
									}
								}
								else if(bad && (err_count == 100)) {	// tell the operator...
									LOG(CAUTION,"BTOW has too many corrupt events (%u/%u) -- stopping detailed logging...!",
									    err_count,san.btow.evt_count) ;
								}
								else if(bad && ((err_count % 1000)==0)) { //
									LOG(CAUTION,"BTOW has many, many errors (%u/%u)...",
									    err_count,san.btow.evt_count) ;
								}
							}
							else if((type == BTOW_ID) && (sec >= 1) && (sec <=3)) {	// BSMD
								LOG(DBG,"BSMD %d sec") ;

							}
							else {
								LOG(WARN,"EMC type %d, sec %d UNKNOWN",type,sec) ;
							}

						}

					}
				}
			}
		}
	}

	if(err) return -1 ;
	return 0 ;
}


static int trgCheck(char *c_trgp)
{
	u_int off, len ;
	char *mem ;

	struct TRGP *trgp = (struct TRGP *) c_trgp ;

	off = trgp->trgData.off ;
	len = trgp->trgData.len ;
	if(trgp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
		off = swap32(off) ;
	}

	
	if(len && off) {
		mem = c_trgp + 4*off ;
		if(chBank(mem, CHAR_TRGD)) return -1 ;


		TrgTowerTrnfer *tran = (TrgTowerTrnfer *)(mem + sizeof(bankHeader));
		int bcv = swap32(tran->byteCount_Version) & 0xff;
		if(bcv == 0x10) {
		 
		  off = swap32(tran->OffsetBlock[TRG_INDEX].offset);
		  int *bcv2p = (int *)(mem + sizeof(bankHeader) + off);
		  int bcv2 = swap32(*bcv2p) & 0xff;
		  if(bcv2 != 0x32) {
		    LOG(ERR, "Trigger data version is %d rather than 0x32",bcv2);
		  }
		  else {
		    LOG(DBG, "Trigger versions: transfer: 0x%x,  data: 0x%x",bcv,bcv2);
		  }
		}
	}


	off = trgp->trgId.off ;
	len = trgp->trgId.len ;
	if(trgp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {
		off = swap32(off) ;
	}

	if(len && off) {
		mem = c_trgp + 4*off ;
		if(chBank(mem, CHAR_TRGID)) return -1 ;
	}

	return 0 ;
}

static int l3Check(char *c_l3p)
{
	if(chBank(c_l3p, CHAR_L3_P)) return -1 ;

	return 0 ;
}
