#ifndef _DAQ_DET_H_
#define _DAQ_DET_H_

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#include <RTS_READER/rts_reader.h>

// the m_Modes are done such that the default value of 0
// will do whatever default for Offline:
// ADC data for most
// CLD data for TPX/TPC

#define m_Mode_DAQ_PED          (1<<0)          // calculate pedestals in Make
#define m_Mode_DAQ_GAIN         (1<<1)          // calculate gains in Make
#define m_Mode_DAQ_FCF_RAW      (1<<2)          // run cluster finder
#define m_Mode_DAQ_ADC		(1<<3)          // Make outputs ADC
#define m_Mode_DAQ_RAW		(1<<4)		// Make outputs RAW (used in realtime only!)
#define m_Mode_DAQ_FCF		(1<<5)		// Make(): runs floating point FCF

#define m_Mode_DAQ_RT		(1<<31)		// working in realtime! Under ESB! do not use in Offline!

class daq_dta ;
class detHandler ;
struct daq_trg_word ;

class daq_det : public St_Maker {
protected:
	u_int file_ix ;
	u_int evt_ix ;

	int present ;
	int legacy ;

	int run_num ;
	u_int evt_num ;	// in the run!

	int def_sector ;
	int def_rdo ;


	static const int MAX_SEC = 0 ;
	static const int MAX_RDO = 0 ;

private:

public:
	daq_det(const char *dname="internal", rts_reader *rts_caller=0) {
		name = dname ;
		rts_id = -1 ;		// this should be correctly overriden in the member

		memset(mydet,0,sizeof(mydet)) ;


		SetMode(0) ;

		file_ix = 0 ;
		evt_ix = 0 ;



		caller = rts_caller ;

		multi_mask = 0 ;

		present = 0 ;
		legacy = 0 ;	// assume not legacy

		run_num = -1 ;	// unknown at startup...
		evt_num = 0 ;
		
		def_sector = def_rdo = -1 ;	// assume ALL

		in_buffer = 0 ;
		out_buffer = 0 ;
		in_bytes = 0 ;
		out_bytes = 0 ;

		return ;
	}



	virtual ~daq_det() {

		if(rts_id == -123) {	// I'm the special dispatcher!
			for(int i=0;i<32;i++) {
				if(mydet[i]) {
					delete mydet[i] ;
				}
				mydet[i] = 0 ;
			}
		}

		return ;
	}

	virtual int Make() {
		evt_num++ ;
		if(rts_id == -123) {
			for(int i=0;i<32;i++) {
				if(multi_mask & (1<<i)) {
					assert(mydet[i]) ;
					mydet[i]->Make() ;
				}
			}
		}

		return 0 ;
	}

	virtual int Init() {
		if(rts_id == -123) {
			for(int i=0;i<32;i++) {
				if(multi_mask & (1<<i)) {
					assert(mydet[i]) ;
					mydet[i]->Init() ;
				}
			}

			multi_mask = 0 ;
		}
		return 0 ;
	}

	virtual int InitRun(int run_number) {
		run_num = run_number ;
		evt_num = 0 ;

		if(rts_id == -123) {
			for(int i=0;i<32;i++) {
				if(multi_mask & (1<<i)) {
					assert(mydet[i]) ;
					mydet[i]->InitRun(run_number) ;
				}
			}

			multi_mask = 0 ;
		}
		return 0 ;
	}

	virtual int FinishRun(int old_run_number) {
		if(rts_id == -123) {
			for(int i=0;i<32;i++) {
				if(multi_mask & (1<<i)) {
					assert(mydet[i]) ;
					mydet[i]->FinishRun(old_run_number) ;
				}
			}

			multi_mask = 0 ;
		}
		return 0 ;
	}

	virtual void help() const {	
		if(rts_id == -123) {
			for(int i=0;i<32;i++) {
				if(multi_mask & (1<<i)) {
					assert(mydet[i]) ;
					mydet[i]->help() ;
				}
			}
		}
		else {
			printf("%s\nNo help written for %s\n",GetCVS(),name) ;
		}
	}


	virtual int presence() ;

	// needs to be overriden!
	virtual int get_token(char *buff, int buff_bytes) ;

	// needs to be overriden!
	virtual int get_l2(char *buff, int buff_bytes, daq_trg_word *trg, int prompt=0) ;

	// needs to be overriden!
	virtual daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	virtual void set_defaults(int sec, int rdo) { 
		def_sector = sec; 
		def_rdo = rdo ; 
		return ; 
	} ;

	virtual const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}


	// used for the container only and not subclases!
	daq_det *mydet[32] ;
	u_int multi_mask ;

	rts_reader *caller ;
	const char *name ;
	int rts_id ;

	u_int event_mode ;
	char *in_buffer ;
	int in_bytes ;
	char *out_buffer ;
	int out_bytes ;
	
#ifdef __RTS_ROOT__
	Class_Def(daq_det,0)
#endif

} ;



#endif
