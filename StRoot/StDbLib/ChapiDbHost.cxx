#include "ChapiDbHost.h"
using namespace std;

///////////////////////////////////////////////////////////////////////////
ChapiDbHost::ChapiDbHost(const string h, const short p, const double power):
  HostName(h),
  Port(p),
  Power(power)
{
}
