#include <string.h>

#include <rtsLog.h>
#include <daq_dta_structs.h>

size_t daq_dta_dict(const char *det, const char *bank)
{
	
	if((det==0) || (bank==0)) return 0 ;	// tough luck...


	/************ TPX ****************/
	if(!strcasecmp(det,"tpx")) {
		if(!strcasecmp(bank,"adc")) {
			return sizeof(struct daq_adc_tb) ;
		}
		if(strcasecmp(bank,"cld")) {
			return sizeof(struct daq_cld) ;
		}

	}
		
	/************** TOF ****************/
	if(!strcasecmp(det,"tof")) {
		if(!strcasecmp(bank,"raw")) {
			return	4 ;	// by definition!
		}
	}

	/************** PP2PP ****************/
	if(!strcasecmp(det,"pp2pp")) {
		if(!strcasecmp(bank,"raw")) {
			return	1 ;	// by definition
		}
	}

			
	/*************** ESMD ***************/
	if(!strcasecmp(det,"esmd")) {
		if(!strcasecmp(bank,"adc")) {
			return	sizeof(unsigned short (*)[192]) ;
		}
		if(!strcasecmp(bank,"raw")) {
			return 2 ;	// by definition
		}
		if(!strcasecmp(bank,"preamble")) {
			return sizeof(unsigned short (*)[4]) ;
		}
	}

	LOG(ERR,"detector \"%s\", bank \"%s\" - none found",det,bank) ;
	return 0 ;	// found no match

}
