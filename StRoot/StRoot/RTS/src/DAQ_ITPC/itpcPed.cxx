#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>

#include <rtsLog.h>
#include <DAQ_ITPC/itpcCore.h>

#include "itpcPed.h"


itpcPed::itpcPed()
{
	memset(ped_p,0,sizeof(ped_p)) ;
	memset(padplane_id,0,sizeof(padplane_id)) ;

	pulser_peak_timebin = 90 ;	// as on the iFEE test stand in the lab!
	pulser_in_star = 0 ;
}


itpcPed::~itpcPed()
{
	for(int s=0;s<24;s++) {
	for(int r=0;r<4;r++) {
	for(int p=0;p<16;p++) {
	for(int c=0;c<64;c++) {
		if(ped_p[s][r][p][c]) {
			free(ped_p[s][r][p][c]) ;
			ped_p[s][r][p][c] = 0 ;
		}
	}
	}
	}
	}

}

int itpcPed::kill_non_phys()
{
	int cou = 0 ;

	for(int s=0;s<24;s++) {
	for(int r=0;r<4;r++) {
	for(int p=0;p<16;p++) {
	for(int c=0;c<64;c++) {
		if(ped_p[s][r][p][c]) ;
		else continue ;

		int row, pad, fee_id ;
		fee_id = padplane_id[s][r][p] ;


		itpc_ifee_to_rowpad(fee_id,c,row,pad) ;

		if(row==0) {
			cou++ ;
			LOG(NOTE,"Non-phys ch %d: %d %d %d %d: fee_id %d, rp %d:%d",cou,s+1,r+1,p+1,c,row,pad) ;

			ped_p[s][r][p][c]->c_ped = 1023.0 ;
		}

	}}}}


	return cou ;
}


// sector: 1 to 24
// rdo: 1 to 4
void itpcPed::init(int sector, int rdo, u_int mask)
{
	LOG(NOTE,"%s: sector %d, RDO %d, mask 0x%04X",__PRETTY_FUNCTION__,sector,rdo,mask) ;

	sector-- ;	// to start from 0
	rdo-- ;		// to start from 0

	//alloc per sector, per rdo
	for(int p=0;p<16;p++) {
	for(int c=0;c<64;c++) {
		if(ped_p[sector][rdo][p][c]) continue ;	// ...silently

		ped_p[sector][rdo][p][c] = (ped_t *)malloc(sizeof(ped_t)) ;
	}
	}

	
	fee_mask[sector][rdo] = mask ;

}

// Needed to get from port to (row:pad) and pin too
void itpcPed::set_padplane_id(int sector, int rdo, int port, int id)
{
	sector-- ;
	rdo-- ;
	port-- ;

	padplane_id[sector][rdo][port] = id ;
}

	
void itpcPed::clear()
{
	memset(evts,0,sizeof(evts)) ;

	for(int s=0;s<24;s++) {
	for(int r=0;r<4;r++) {
	for(int p=0;p<16;p++) {
	for(int c=0;c<64;c++) {
		if(ped_p[s][r][p][c]) {
			memset(ped_p[s][r][p][c],0,sizeof(ped_t)) ;
		}
	}
	}
	}
	}

	memset(fee_err,0,sizeof(fee_err)) ;

}

// sector, rdo, port, channel, timebin, value
void itpcPed::accum(int sector, int rdo, int port,int fee_id, int ch, int tb, int adc_val)
{
	ped_t *pt = ped_p[sector][rdo][port][ch] ;

	if(pt==0) return ;	// super-precaution

	if(evts[sector][rdo]<250) {
		evts[sector][rdo]++ ;
	}

	pt->ped[tb] += adc_val ;
	pt->rms[tb] += adc_val*adc_val ;
	pt->cou[tb]++ ;

	pt->c_ped += adc_val ;
	pt->c_rms += adc_val*adc_val ;
	pt->c_cou++ ;

}


void itpcPed::calc()
{
	for(int s=0;s<24;s++) {
	for(int r=0;r<4;r++) {
		if(ped_p[s][r][0][0]==0) continue ;

		for(int p=0;p<16;p++) {
			if(fee_mask[s][r] & (1<<p)) ;
			else continue ;

			for(int c=0;c<64;c++) {
				ped_t *pt = ped_p[s][r][p][c] ;

				if(pt->c_cou==0) {	// problem!!!
					pt->c_ped = 1023.0 ;
					pt->c_rms = 1023.0 ;
					continue ;
				}

				pt->c_ped /= pt->c_cou ;
				pt->c_rms /= pt->c_cou ;
				pt->c_rms = pt->c_rms - pt->c_ped*pt->c_ped ;

				if(pt->c_rms <= 0.0) pt->c_rms = 0.0 ;
				else pt->c_rms = sqrt(pt->c_rms) ;

				for(int t=0;t<512;t++) {
					if(pt->cou[t]==0) {
						pt->ped[t] = 1023.0 ;
						pt->rms[t] = 1022.0 ;
						continue ;
					}

					pt->ped[t] /= pt->cou[t] ;
					pt->rms[t] /= pt->cou[t] ;

					pt->rms[t] = pt->rms[t] - pt->ped[t]*pt->ped[t] ;
			
					if(pt->rms[t] <= 0.0) pt->rms[t] = 0.0 ;
					else pt->rms[t] = sqrt(pt->rms[t]) ;
				}
			}
		}
	}
	}


}


int itpcPed::from_cache(const char *fname, int sec1, int rdo1)
{
	FILE *f ;
	char fn[256] ;

	if(fname==0) {
		sprintf(fn,"/RTScache/itpc_pedestals_s%02d_r%d.txt",sec1,rdo1) ;
	}
	else strcpy(fn,fname) ;


	f = fopen(fn,"r") ;
	if(f==0) {
		LOG(ERR,"%s: [%s]",fn,strerror(errno)) ;
		return -1 ;
	}


	while(!feof(f)) {
		char buff[256] ;
		int sec, rdo, fee_port, fee_ch, tb, fee_id, dummy ;
		float fd, fped, frms ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		int ret = sscanf(buff,"%d %d %d %d %d %d %d %d %d %f %f %f %f",
			&sec,&rdo,&fee_port,&fee_ch,&fee_id,&dummy,&dummy,&dummy,&dummy,&fd,&fd,&fped,&frms) ;

		
		if(ret == 13) {	// first, global section

			sec-- ;
			rdo-- ;
			fee_port-- ;


			if(ped_p[sec][rdo][fee_port][fee_ch]) ;
			else continue ;

			padplane_id[sec][rdo][fee_port] = fee_id ;

			ped_p[sec][rdo][fee_port][fee_ch]->c_ped = fped ;
			ped_p[sec][rdo][fee_port][fee_ch]->c_rms = frms ;
		}
		else {	// per timebin
			ret = sscanf(buff,"%d %d %d %d %d %f %f",
				     &sec,&rdo,&fee_port,&fee_ch,&tb,&fped,&frms) ;

			if(ret != 7) {
				LOG(ERR,"WTF?") ;
				continue ;
			}

			sec-- ;
			rdo-- ;
			fee_port-- ;


			if(ped_p[sec][rdo][fee_port][fee_ch]) ;
			else continue ;

			ped_p[sec][rdo][fee_port][fee_ch]->ped[tb] = fped ;
			ped_p[sec][rdo][fee_port][fee_ch]->rms[tb] = frms ;



		}
		
	}

	fclose(f) ;

	return 0 ;
}

int itpcPed::sanity(int mode)
{
	u_int bad_cou =0 ;
	u_int good_cou =0 ;

	char fname[128] ;
	sprintf(fname,"/log/itpc/itpc_log_%02d.txt",sector_id) ;
	FILE *f = fopen(fname,"a") ;
	if(f==0) {
		LOG(ERR,"sanity: %s [%s]",fname,strerror(errno)) ;
	}
	else {
		time_t now = time(0) ;

		fprintf(f,"Run %08u, Sector %2d, Run-type %d. Date %s",run_number,sector_id,run_type,ctime(&now)) ;
	}

	if(mode) {
		LOG(INFO,"Using pulser peak timebin %d, in STAR %c",
		    pulser_peak_timebin, pulser_in_star?'Y':'N') ;
	}

	for(int s=0;s<24;s++) {
	for(int r=0;r<4;r++) {
		if(ped_p[s][r][0][0]==0) continue ;

		if(evts[s][r]<100) {	// no point in complaining if there's not enough events
			continue ;
		}

		for(int p=0;p<16;p++) {
			if(fee_mask[s][r] & (1<<p)) ;
			else continue ;

			for(int c=0;c<64;c++) {
				ped_t *pt = ped_p[s][r][p][c] ;


				int row, pad, fee_id ;

				fee_id = padplane_id[s][r][p] ;

				//if(p==4) fee_id = 54 ;
				//else if(p==11) fee_id = 55 ;
				//else fee_id = 46 ;

				itpc_ifee_to_rowpad(fee_id,c,row,pad) ;

				int pin ;
				itpc_rowpad_to_id(row,pad,fee_id,pin) ;

				//LOG(TERR,"%2d %d %2d %2d -1 %.3f %.3f :: FEE_ID %d,RP %d:%d, pin %d",s+1,r+1,p+1,c,pt->c_ped,pt->c_rms,fee_id,row,pad,pin) ;

	
				double m_ped = 0.0 ;
				double m_rms = 0.0 ;
				u_int m_cou = 0 ;

				for(int t=0;t<=20;t++) {
					m_ped += pt->ped[t] ;
					m_rms += pt->rms[t] ;
					m_cou++ ;
				}



				m_ped /= m_cou ;
				m_rms /= m_cou ;

				double pulser = pt->ped[pulser_peak_timebin] - m_ped ; 	// this is where the pulser on the FEE QA stand resides

				int bad = 0 ;

				if(row) {	// only for connected pads!
					if((m_ped<20)||(m_ped>150)) bad |= 1 ;
					if(m_rms < 0.5) bad |= 2 ;


					// only check for non-pulser events because the STAR pulser is very noisy
					if(mode==1 && pulser_in_star) {
						// The STAR TPC pulser introduces so much noise
						// it is not worth looking at it.
						//if(m_rms>3) bad |= 4 ;
						
					}
					else {
						if(m_rms>1.8) bad |= 4 ;
					}

					if(mode==1 && pulser < 200) bad |= 8 ;
				}

				if(bad) {
					bad_cou++ ;
					fee_err[s][r][p][c] |= bad ;
					
					if(f) {
						fprintf(f,"  Bad FEE Ch: RDO %d, Port #%d, Ch %d (Padplane %d, rp %d:%d): flag 0x%X: %.1f +- %.1f, %.1f\n",
							r+1,p+1,c,fee_id,row,pad,bad,m_ped,m_rms,pulser) ;
					}

					LOG(WARN,"Bad FEE Ch: RDO %d, Port #%d, Ch %d (Padplane %d, rp %d:%d): flag 0x%X: %.1f +- %.1f, %.1f",
					    r+1,p+1,c,fee_id,row,pad,bad,m_ped,m_rms,pulser) ;

					usleep(1000) ;	// to give time for the printout
				}
				else {
					good_cou++ ;
				}

			}
		}
	}
	}

	if(f) {
		fprintf(f,"Bad channels: %d/%d\n",bad_cou,bad_cou+good_cou) ;
		fclose(f) ;
	}

	LOG(INFO,"Bad channels: %d/%d",bad_cou,bad_cou+good_cou) ;


	return 0 ;
}


int itpcPed::to_cache(const char *fname, int sec1, int rdo1)
{
	FILE *outf ;
	int s_start, s_stop ;
	int r_start, r_stop ;

	s_start = 1 ;
	s_stop = 24 ;
	
	r_start = 1 ;
	r_stop = 4 ;

	if(fname) {
		outf = fopen(fname,"w") ;
		if(outf==0) {
			LOG(ERR,"%s: %s [%s]",__PRETTY_FUNCTION__,fname,strerror(errno)) ;
			return -1 ;
		}
	}
	else if(sec1 < 0) {
		outf = stdout ;
	}
	else {
		char fn[256] ;

		s_start = sec1 ;
		s_stop = sec1 ;

		r_start = rdo1 ;
		r_stop = rdo1 ;

		sprintf(fn,"/RTScache/itpc_pedestals_s%02d_r%d.txt",sec1,rdo1) ;

		outf = fopen(fn,"w") ;
		if(outf==0) {
			LOG(ERR,"%s: %s [%s]",__PRETTY_FUNCTION__,fn,strerror(errno)) ;
			return -1 ;

		}
	}

		
	for(int sx=s_start;sx<=s_stop;sx++) {
		int s = sx - 1 ;
	for(int rx=r_start;rx<=r_stop;rx++) {
		int r = rx - 1 ;

		if(ped_p[s][r][0][0]==0) continue ;



		for(int p=0;p<16;p++) {
			if(fee_mask[s][r] & (1<<p)) ;
			else continue ;

			for(int c=0;c<64;c++) {
				ped_t *pt = ped_p[s][r][p][c] ;


				int row, pad, fee_id ;

				fee_id = padplane_id[s][r][p] ;

				//if(p==4) fee_id = 54 ;
				//else if(p==11) fee_id = 55 ;
				//else fee_id = 46 ;

				itpc_ifee_to_rowpad(fee_id,c,row,pad) ;

				int pin ;
				itpc_rowpad_to_id(row,pad,fee_id,pin) ;

				//LOG(TERR,"%2d %d %2d %2d -1 %.3f %.3f :: FEE_ID %d,RP %d:%d, pin %d",s+1,r+1,p+1,c,pt->c_ped,pt->c_rms,fee_id,row,pad,pin) ;

	
				double m_ped = 0.0 ;
				double m_rms = 0.0 ;
				u_int m_cou = 0 ;

				for(int t=0;t<=20;t++) {
					m_ped += pt->ped[t] ;
					m_rms += pt->rms[t] ;
					m_cou++ ;
				}

				m_ped /= m_cou ;
				m_rms /= m_cou ;


				fprintf(outf,"%2d %d %2d %2d %2d %2d %2d %3d %3d %.3f %.3f %.3f %.3f \n",s+1,r+1,p+1,c,fee_id,pin,row,pad,-1,pt->c_ped,pt->c_rms, m_ped, m_rms) ;

				// NOTE HACK!!!!
				pt->c_ped = m_ped ;
				pt->c_rms = m_rms ;

			}
		}
	}
	}




	for(int sx=s_start;sx<=s_stop;sx++) {
		int s = sx - 1 ;
	for(int rx=r_start;rx<=r_stop;rx++) {
		int r = rx - 1 ;

		if(ped_p[s][r][0][0]==0) continue ;



		for(int p=0;p<16;p++) {
			if(fee_mask[s][r] & (1<<p)) ;
			else continue ;

			for(int c=0;c<64;c++) {
				ped_t *pt = ped_p[s][r][p][c] ;

				for(int t=0;t<512;t++) {
					double ped = pt->ped[t] ;

					//if(t==185 || t==186) printf("YADA %f %f %f\n",ped,pt->c_ped,pt->rms[t]) ;

					if(run_type==5) {
						ped -= pt->c_ped ;

						if(ped<2.0) ped = 0.0 ;	// kill low lying pulses

						if(ped==0.0) continue ;

						if(pt->rms[t]>1000.0) continue ;	
					}

					fprintf(outf,"%2d %d %2d %2d %3d %.3f %.3f\n",s+1,r+1,p+1,c,t,ped,pt->rms[t]) ;
				}
			}
		}
	}
	}


	if(outf != stdout) fclose(outf) ;

	return 0 ;
}
