#ifndef _DAQ_FTP_H_
#define _DAQ_FTP_H_

struct ftp_t {
	int channels ;
	int mode ;	// 0 normal, 1 pedestals/RMSs
	int max_channels ;

	// how many valid timebins in this pad
	u_char counts[2][10][960] ;
	// up to 256 valid timebins (count is in counts)
	u_char timebin[2][10][960][256] ;

	// up to 256 valid adcs (same count as above...)
	u_char adc[2][10][960][256] ;

} ;

#ifndef DAQ_FTP_DATA_STRUCTURE
#include <stdio.h>
#include <DAQ_READER/daq_det.h>

class daq_ftp : public daq_det {
private:
	class daq_dta *handle_legacy() ;

	class daq_dta *legacy ;	// "legacy" bank

	static const char *help_string ;
protected:


public:
	daq_ftp(daqReader *rts_caller=0) ;
	~daq_ftp() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;

#endif   //  DAQ_FTP_DATA_STRUCTURE
#endif	// _DAQ_FTP_H_
