#ifndef ChapiDbHost_h
#define ChapiDbHost_h
#include <vector>
#include <string>

const short DefaultPort = 3316;

using std::string;

class ChapiDbHost
{
 public:
  string HostName;
  short Port;

  ChapiDbHost(){HostName=""; Port=DefaultPort;};
  ChapiDbHost(const std::string h, const short p=DefaultPort);

};

#endif
