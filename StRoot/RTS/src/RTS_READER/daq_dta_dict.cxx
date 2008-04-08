#include <string.h>

#include <rtsLog.h>

const char *daq_dta_dict(const char *det, const char *bank)
{
	
	if((det==0) || (bank==0)) return 0 ;	// tough luck...


	/************ TPX ****************/
	if(!strcasecmp(det,"tpx")) {
		if(!strcasecmp(bank,"adc")) {
			return "struct daq_adc_tb { \
				unsigned short adc ; \
				unsigned short tb ; \
			}" ;
		}
		if(strcasecmp(bank,"cld")) {
			return "struct daq_cld { \
				float tb ;	\
				float pad ;	\
				unsigned short charge ;	\
				unsigned short flags ;	\
				unsigned short t1, t2 ;	\
				unsigned short p1, p2 ;	\
			}" ;
		}

	}
		
	/************** TOF ****************/
	if(!strcasecmp(det,"tof")) {
		if(!strcasecmp(bank,"raw")) {
			return	"unsigned int" ;
		}
	}

	/************** PP2PP ****************/
	if(!strcasecmp(det,"pp2pp")) {
		if(!strcasecmp(bank,"raw")) {
			return	"unsigned char" ;
		}
	}

			
	/*************** ESMD ***************/
	if(!strcasecmp(det,"esmd")) {
		if(!strcasecmp(bank,"adc")) {
			return	"unsigned short (*)[192]" ;
		}
		if(!strcasecmp(bank,"raw")) {
			return "unsigned short" ;
		}
		if(!strcasecmp(bank,"preamble")) {
			return "unsigned short (*)[4]" ;
		}
	}

	LOG(ERR,"detector \"%s\", bank \"%s\" - none found",det,bank) ;
	return 0 ;	// found no match

}
