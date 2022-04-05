#ifndef _DAQ_DET_H_
#define _DAQ_DET_H_


// the m_Modes are done such that the default value of 0
// will do whatever default for Offline:
// ADC data for most
// CLD data for TPX/TPC

#include <sys/types.h> // for u_int

#define m_Mode_DAQ_PED          (1<<0)          // calculate pedestals in Make
#define m_Mode_DAQ_GAIN         (1<<1)          // calculate gains in Make
#define m_Mode_DAQ_FCF_RAW      (1<<2)          // run cluster finder
#define m_Mode_DAQ_ADC		(1<<3)          // Make outputs ADC
#define m_Mode_DAQ_RAW		(1<<4)		// Make outputs RAW (used in realtime only!)
#define m_Mode_DAQ_FCF		(1<<5)		// Make(): runs floating point FCF

#define m_Mode_DAQ_RT		(1<<31)		// working in realtime! Under ESB! do not use in Offline!


#define m_Debug_DAQ_PRINT	(1<<0)
#define m_Debug_DAQ_CHECK	(1<<1) 


#define DET_PRESENT_SFS		(1<<0)
#define DET_PRESENT_DATAP	(1<<1)
#define DET_PRESENT_TRG		(1<<2)

// forward declarations
class daq_dta ;
class daqReader ;
class daq_det ;

// helpers
extern int checkBank(char *in, char *expect) ;
extern int *legacyDetp(int rts_id, char *datap) ;


class daq_det_factory
{
public:
	daq_det_factory() {} ;
	virtual ~daq_det_factory() {} ;

	static daq_det *make_det(int wh) ;
protected:
	static daq_det_factory *det_factories[48] ;	// for real dets
	static daq_det_factory *pseudo_factories[48] ;	// for pseudo/internal dets such as whole EMC
	virtual daq_det *create() = 0 ;
} ;


class daq_det {
protected:

	u_char present ;	// in this event: bitmask: 1 in DATAP; 2 in SFS


	int run_num ;	// of this run
	u_int evt_num ;	// in the run!

	int def_sector ;	
	int def_rdo ;

	u_int m_Mode ;	// processing mod bitmask: see m_Mode_DAQ_xxx above

	u_int m_Debug ; // prints, header checks etc.

	// statics
	static const int MAX_SEC = 0 ;
	static const int MAX_RDO = 0 ;

	static int endianess ;	// of the executing machine -- 0 is little, 1 is big


	virtual int presence() ;	// is this DET (not only the bank!) present in this event?
	daqReader *caller ;

	const char *sfs_name ;	// name in sfs bank (if any) i.e. "tpx"



private:

public:


	daq_det(daqReader *caller = 0) ;
	virtual ~daq_det() ;

	virtual int Init() ;
	virtual int InitRun(int run) ;
	virtual int Make() ;
	virtual int FinishRun(int old_run) ;



	// needs to be overriden!
	virtual daq_dta  *get(const char *bank="*",int sec=-1, int row=-1, int pad=-1, void *p1=0, void *p2=0) ;
	virtual int bad_sanity() ;

	// needs to be overriden!
	virtual daq_dta  *put(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;




	virtual void help() const ;


	virtual int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;
	virtual int get_token(char *buff, int buff_bytes) ;


	virtual void SetMode(u_int mode=0) { 
		m_Mode=mode ;
	} ;
	virtual u_int GetMode() { 
		return m_Mode ; 
	} ;

	virtual void set_defaults(int sec, int rdo) { 
		def_sector = sec; 
		def_rdo = rdo ; 
		return ; 
	} ;

	virtual const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $: $Id: daq_det.h,v 1.11 2018/06/21 13:48:44 tonko Exp $: built " __DATE__ " " __TIME__ ; 
		return cvs;
	} ;


	virtual unsigned int get_global_event_num() ;

	void managed_by(class daqReader *c) ;

	

	const char *name ;	// detector's name i.e. "SVT"

	u_int event_mode ;

	char *in_buffer ;
	int in_bytes ;

	char *out_buffer ;
	int out_bytes ;

	int rts_id ;		// RTS_ID

} ;



#endif
