#ifndef TTRACKFRAME
#define TTRACKFRAME
#include "FTF_Hit.h"
#include "FTF_Track.h"
#include "FTF_Mc_Track.h"


class TrackerFrame {

public:
	int  CloseAscii ( ) ;
	int  Done       ( ) ;
	void EventReset  (  ) ;
	int  GetEvent   (  ) ;
	void Init       (  ) ;
	int  OpenAscii (char *datafile_name) ;
	
	int  event ;

} ;
#endif

