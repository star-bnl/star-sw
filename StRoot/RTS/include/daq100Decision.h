#ifndef _DAQ100_DECISION_H_
#define _DAQ100_DECISION_H_

#include <daqModes.h>

/* 
	The function returns a bitfield:

	Bit1	Has FCF
	Bit0	Has Raw
*/

extern inline int daq100Decision(u_int t, u_int daq_cmd, u_int run_type, u_int cl_run, u_int zero_wr)
{
	int fmt, proc ;

	proc = 0 ;
	fmt = 0 ;

	if(daq_cmd & DAQCMD_CL_RUN) {	// run CL unconditionally!
		proc = 1 ;
	}
	else {
		if(cl_run == 0) proc = 0 ;
		else if((t % cl_run)==0) proc = 1 ;
	}

	if(daq_cmd & DAQCMD_FMT_ONLY) {	// run RAW unconditionally
		fmt = 1 ;
	}
	else {
		if(zero_wr == 0) fmt = 0 ;
		else if((t % zero_wr)==0) fmt = 1 ;
	}			


	// overrides
	switch(run_type) {
/*
	case RUN_TYPE_PULSER :
	case RUN_TYPE_LASER :
		fmt = 1 ;	// always format; FCF as usual
		break ;
*/
	case RUN_TYPE_CONFIG :
	case RUN_TYPE_GAIN :
	case RUN_TYPE_PED :
		proc = 0 ;	// never proc
		fmt = 1 ;	// always format
		break ;
	case RUN_TYPE_DEBUG :
		proc = 0 ;	// never any...
		fmt = 0 ;
		break ;
	}

	// always!
	if(t == 0) {
		proc = 0 ;
		fmt = 1 ;
	}

	return (proc << 1) | fmt ;

}

#endif
