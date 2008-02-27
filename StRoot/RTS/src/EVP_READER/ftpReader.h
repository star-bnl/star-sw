#ifndef _FTP_READER_H_
#define _FTP_READER_H_

#include <sys/types.h>

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

extern struct ftp_t ftp ;
extern int ftpReader(char *mem) ;


#endif
