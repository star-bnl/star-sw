#ifndef ChapiDbHost_h
#define ChapiDbHost_h
#include <vector>
#include <string>
#include "limits.h"

const short DefaultPort = 3316;
const double DefaultPower = 1.;
const short DefaultCap = SHRT_MAX;
using std::string;

class ChapiDbHost
{
 public:
  string HostName;
  short Port;
  double Power; // proportional to machine's productivity (or desired 
                // involvement in the DB service)
  unsigned short Cap;  // max # of connections, applies to the ordinary, NOT machine-power weighted connections

  ChapiDbHost(const std::string h="", 
	      const short p = DefaultPort, 
	      const double power = DefaultPower, 
	      const short cap = DefaultCap);

};

#endif
