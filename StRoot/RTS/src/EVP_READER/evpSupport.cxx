#include <sys/types.h>
#include <string.h>
#include <ctype.h>

#include <rtsLog.h>

#include <evpSupport.h>

extern int is_bus_error(const char *addr) ;

int checkBank(char *m, char *what) ;


int checkBank(char *m, char *what)
{
	char bank[10];
	

	LOG(DBG,"Checking bank for [%s]...",(uint)what,0,0,0,0) ;

	if(is_bus_error(m)) {
		LOG(CAUTION,"BUS ERROR while checking for bank type [%s]!",what) ;
		return -1 ;
	}

	memcpy(bank,m,8) ;
	bank[9] = 0 ;

	if(memcmp(m,what,strlen(what)) != 0) {
		int i ;
		for(i=0;i<8;i++) {
			if(isalnum(*(bank+i))) ;	// do nothing
			else *(bank+i) = '*' ;
		}
		LOG(CAUTION,"Wrong bank type: expecting [%s], is [%s]",(uint)what,(uint)bank,0,0,0) ;
		return -1 ;
	}
	else {
		LOG(DBG,"Bank Compare for [%s] OK...",(uint)what,0,0,0,0) ;
	}

	return 0 ;
}


