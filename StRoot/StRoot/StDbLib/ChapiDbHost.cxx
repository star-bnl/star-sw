#include "ChapiDbHost.h"
using namespace std;

///////////////////////////////////////////////////////////////////////////
ChapiDbHost::ChapiDbHost(const string h, const short p, const double power, const short cap):
  HostName(h),
  Port(p),
  Power(power),
  Cap(cap)
{
}
