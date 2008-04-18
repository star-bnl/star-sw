#ifndef _DAQ_DET_H_
#define _DAQ_DET_H_


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




// forward declarations
class daq_dta ;
class rts_reader ;


class daq_det {
protected:
	u_int file_ix ;
	u_int evt_ix ;

	int present ;	// in this event...
	int legacy ;	// currently unused...




	int run_num ;	// of this run
	u_int evt_num ;	// in the run!

	int def_sector ;
	int def_rdo ;

	int m_Mode ;	// processing mode

	// statics
	static const int MAX_SEC = 0 ;
	static const int MAX_RDO = 0 ;

	static int endianess ;	// of the executing machine -- 0 is little, 1 is big

private:

public:


	daq_det(const char *name = "internal", rts_reader *caller = 0) ;
	virtual ~daq_det() ;

	virtual int Init() ;
	virtual int InitRun(int run) ;
	virtual int Make() ;
	virtual int FinishRun(int old_run) ;



	// needs to be overriden!
	virtual daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	// needs to be overriden!
	virtual daq_dta  *put(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;


	virtual int presence() ;

	virtual void help() const ;

	virtual int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;
	virtual int get_token(char *buff, int buff_bytes) ;


	virtual void SetMode(int mode=0) { 
		m_Mode=mode ;
	} ;
	virtual int GetMode() { 
		return m_Mode ; 
	} ;

	virtual void set_defaults(int sec, int rdo) { 
		def_sector = sec; 
		def_rdo = rdo ; 
		return ; 
	} ;

	virtual const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $: $Id: daq_det.h,v 1.5 2008/04/18 17:27:31 tonko Exp $: built "__DATE__" "__TIME__ ; 
		return cvs;
	}



	rts_reader *caller ;
	const char *name ;
	int rts_id ;

	u_int event_mode ;

	char *in_buffer ;
	int in_bytes ;

	char *out_buffer ;
	int out_bytes ;


	// used for the container only and not subclases!
	daq_det *mydet[32] ;
	u_int multi_mask ;

} ;



#endif
