#ifndef StiDebug_H 
#define StiDebug_H 1


class StiDebug
{
 public:

	// constants
	static const int None;
	static const int All;
	static const int Flow;
	static const int Node;
	static const int Track;
	static const int Finding;
	
	// variable
	static int debug;
	static bool isReq(const int req=0);

};



#endif
