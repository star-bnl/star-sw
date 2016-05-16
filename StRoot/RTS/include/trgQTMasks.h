#ifndef _TRG_QT_MASKS_
#define _TRG_QT_MASKS_

#include <rtsSystems.h>


/*
	The "system" bits below are used in the per-event STP "system_word"
	which defines whic Trigger (sub)systems need to be readout for
	this particular event.
	
	For example, an STP word in this form:

	stp_system_word = (1<<TRG_SYS_VPD) | (1<<TRG_SYS_MTD) | (1<< TRG_SYS_FMS) ;

	will _only_ read the QT boards that correspond those sysbsystems
	for this specific event.

	These bits are used/set/sent in the L0 processor depending on the trigger/event
	based upon the Run Control per/trigger setup.

	They are also used in the QT readout code.
*/

/* bits of the various Trigger QT-based subsystems */
#define TRG_SYS_VPD	0
#define TRG_SYS_BBC	1
#define TRG_SYS_ZDC	2
#define TRG_SYS_TOF	3
#define TRG_SYS_MTD	4
#define TRG_SYS_FMS	5
#define TRG_SYS_PP2	6	/* pp2pp */
#define TRG_SYS_EPD	7




/*
	The function below will return the bitmask of QTs which only
	need to be readout for a given Trigger crate (or "instance" as defined
	in rtsSystems.h) using the per-event STP "system word" set by
	the L0 CPU based upon the Run Control setings.

	The function is ONLY used in the QT readout task.
*/
inline unsigned int trgQTCrateMask(int my_trg_instance, unsigned int stp_system_word)
{
	unsigned int mask = 0 ;

	switch(my_trg_instance) {
	case TRG_QT1_INSTANCE :
	case TRG_QT2_INSTANCE :
	case TRG_QT3_INSTANCE :
	case TRG_QT4_INSTANCE :
		if(stp_system_word & (1<<TRG_SYS_FMS)) mask |= 0x3FF ;	//11 QTs each
		break ;
	case TRG_BBQ_INSTANCE :
		if(stp_system_word & (1<<TRG_SYS_BBC)) mask |= 0x001 ;		
		if(stp_system_word & (1<<TRG_SYS_VPD)) mask |= 0x040 ;		
		if(stp_system_word & (1<<TRG_SYS_ZDC)) mask |= 0x100 ;		
		break ;
	case TRG_MXQ_INSTANCE :
		if(stp_system_word & (1<<TRG_SYS_MTD)) mask |= 0x371 ;
		if(stp_system_word & (1<<TRG_SYS_PP2)) mask |= 0x002 ;
		if(stp_system_word & (1<<TRG_SYS_ZDC)) mask |= 0x004 ;
		if(stp_system_word & (1<<TRG_SYS_VPD)) mask |= 0x018 ;
		break ;
	case TRG_FEQ_INSTANCE :
		if(stp_system_word & (1<<TRG_SYS_EPD)) mask |= 0x003 ;		
		break ;

	default :	//this is an error!
		break ;
	}

	return mask ;

}



#endif
