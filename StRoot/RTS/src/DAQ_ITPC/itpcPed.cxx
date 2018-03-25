#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include <rtsLog.h>
#include <DAQ_ITPC/itpcCore.h>

#include "itpcPed.h"


itpcPed::itpcPed()
{
	memset(ped_p,0,sizeof(ped_p)) ;
	memset(padplane_id,0,sizeof(padplane_id)) ;
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

// sector: 1 to 24
// rdo: 1 to 4
void itpcPed::init(int sector, int rdo, u_int mask)
{
	LOG(TERR,"%s: sector %d, RDO %d, mask 0x%04X",__PRETTY_FUNCTION__,sector,rdo,mask) ;

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

}

// sector, rdo, port, channel, timebin, value
void itpcPed::accum(int sector, int rdo, int port,int fee_id, int ch, int tb, int adc_val)
{
	ped_t *pt = ped_p[sector][rdo][port][ch] ;
	
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
					continue ;
				}

				pt->c_ped /= pt->c_cou ;
				pt->c_rms /= pt->c_cou ;
				pt->c_rms = sqrt(pt->c_rms - pt->c_ped*pt->c_ped) ;

				for(int t=0;t<512;t++) {
					if(pt->cou[t]==0) continue ;

					pt->ped[t] /= pt->cou[t] ;
					pt->rms[t] /= pt->cou[t] ;
					pt->rms[t] = sqrt(pt->rms[t] - pt->ped[t]*pt->ped[t]) ;
				}
			}
		}
	}
	}


}


int itpcPed::from_cache(const char *fname)
{
	FILE *f = fopen(fname,"r") ;
	if(f==0) {
		LOG(ERR,"%s: [%s]",fname,strerror(errno)) ;
		return -1 ;
	}


	while(!feof(f)) {
		char buff[256] ;
		int sec, rdo, fee_port, fee_ch, dummy ;
		float fd, fped, frms ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		int ret = sscanf(buff,"%d %d %d %d %d %d %d %d %d %f %f %f %f",
			&sec,&rdo,&fee_port,&fee_ch,&dummy,&dummy,&dummy,&dummy,&dummy,&fd,&fd,&fped,&frms) ;

		if(ret != 13) {
			//LOG(TERR,"ret %d",ret) ;
			continue ;
		}

		sec-- ;
		rdo-- ;
		fee_port-- ;

		if(ped_p[sec][rdo][fee_port][fee_ch]) ;
		else continue ;

		ped_p[sec][rdo][fee_port][fee_ch]->c_ped = fped ;
		ped_p[sec][rdo][fee_port][fee_ch]->c_rms = frms ;
		
	}
	return 0 ;
}


int itpcPed::to_cache(const char *fname)
{
	FILE *outf ;

	if(fname) {
		outf = fopen(fname,"w") ;
		if(outf==0) {
			LOG(ERR,"%s: %s [%s]",__PRETTY_FUNCTION__,fname,strerror(errno)) ;
			outf = stdout ;
		}
	}
	else outf = stdout ;

	for(int s=0;s<24;s++) {
	for(int r=0;r<4;r++) {
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
					m_rms += pt->ped[t] * pt->ped[t] ;
					m_cou++ ;
				}

				m_ped /= m_cou ;
				m_rms /= m_cou ;
				m_rms = sqrt(m_rms - m_ped*m_ped) ;

				fprintf(outf,"%2d %d %2d %2d %2d %2d %2d %3d %3d %.3f %.3f %.3f %.3f \n",s+1,r+1,p+1,c,fee_id,pin,row,pad,-1,pt->c_ped,pt->c_rms, m_ped, m_rms) ;
			}
		}
	}
	}




	for(int s=0;s<24;s++) {
	for(int r=0;r<4;r++) {
		if(ped_p[s][r][0][0]==0) continue ;



		for(int p=0;p<16;p++) {
			if(fee_mask[s][r] & (1<<p)) ;
			else continue ;

			for(int c=0;c<64;c++) {
				ped_t *pt = ped_p[s][r][p][c] ;

				for(int t=0;t<512;t++) {
					fprintf(outf,"%2d %d %2d %2d %3d %.3f %.3f\n",s+1,r+1,p+1,c,t,pt->ped[t],pt->rms[t]) ;
				}
			}
		}
	}
	}


	if(outf != stdout) fclose(outf) ;

	return 0 ;
}
