#ifndef ChapiDbHost_h
#define ChapiDbHost_h
#include <vector>
#include <string>

const short DefaultPort = 3316;
const double DefaultPower = 1.;
using std::string;

class ChapiDbHost
{
 public:
  string HostName;
  short Port;
  double Power;

  ChapiDbHost(const std::string h="", const short p = DefaultPort, const double power = DefaultPower);

};

#endif
