#ifndef _DAQ_ALGO_H_
#define _DAQ_ALGO_H_


class daq_dta ;

class daq_algo {

public:
	daq_algo(const char *aname="any") {
		algo_name = aname ;

		evt_num = 0 ;
		
		def_sector = -1 ;
		def_rdo = -1 ;

		last_loaded = 0 ;
	}

	virtual ~daq_algo() {} ;

	const char *algo_name ;


	// misc variables in use...
	u_int evt_num ;
	time_t last_loaded ;

	int def_sector ;
	int def_rdo ;

	int min_sec, max_sec ;
	int min_rdo, max_rdo ;

	static const int MAX_SECTOR = 1 ;
	static const int MAX_RDO = 1 ;

	virtual int Make(daq_dta *dd) { return 0 ; } ;
	virtual int Make(void *rdo, char *outbuff=0, int max_bytes=0) { return 0 ; } ;
	virtual int InitRun(int run=0) { evt_num = 0 ; return 0 ; }

	virtual int FinishRun(int orun=0) { return 0 ; } ;
	
	// helpers
	void set_defaults(int sec, int rdo)
	{
		def_sector = sec ;
		def_rdo = rdo ;

		check_defaults() ;
	}

	void check_defaults()
	{
		if(def_sector <= 0) {
			min_sec = 1 ;
			max_sec = MAX_SECTOR ;
		}
		else {
			min_sec = max_sec = def_sector ;
		}

		if(def_rdo <= 0) {
			min_rdo = 1 ;
			max_rdo = MAX_RDO ;
		}
		else {
			min_rdo = max_rdo = def_rdo ;
		}
	}

	int check_file(const char *fname) const {


		struct stat ss ;

		int ret = stat(fname,&ss) ;
		if(ret < 0) {	
			LOG(ERR,"%s: stat(%s) failed [%s]",algo_name,fname,strerror(errno)) ;
			return 1 ;	// reloaddd
		}

		if(ss.st_mtime > last_loaded) {
			return 1 ;
		}

		return 0 ;	// no load necessary

	}

	// these typically exist:

	// Online (typically)
	virtual int from_file(const char *fname=0) { return 0 ; } ;
	virtual int to_file(const char *fname=0) { return 0 ; } ;
	virtual int to_sfs(char *out, int max_bytes) { return 0 ; } ;	

	// Offline:
	virtual int from_db(void *db=0) { return 0 ; } ;
	virtual int to_store(daq_dta *dta) { return 0 ; } ;

} ;


#endif
