#include "StiDebug.h"



const int StiDebug::None = 0;
const int StiDebug::All  = 0xFF;
const int StiDebug::Flow = 2;
const int StiDebug::Node = 4;
const int StiDebug::Track = 8;
const int StiDebug::Finding = 16;

// variable
int StiDebug::debug = StiDebug::None;

bool StiDebug::isReq(const int req)
{
	return (req&debug);
}

