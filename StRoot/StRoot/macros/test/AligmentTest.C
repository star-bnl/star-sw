#include "StArchInfo.h"
#include <iostream>
using namespace std;
int AligmentTest() {
  cout << "Checking aligments  . . .  " << endl;

  cout << " char="      << StArchInfo::align<char>()
       << " short="     << StArchInfo::align<short>()
       << " int="       << StArchInfo::align<int>()
       << " long="      << StArchInfo::align<long>()
       << " long long=" << StArchInfo::align<long long>()
       << " float="     << StArchInfo::align<float>()
       << " double="    << StArchInfo::align<double>()
       << endl;
  
  cout << " char="      << StArchInfo::align(char(0))
       << " short="     << StArchInfo::align(short(0))
       << " int="       << StArchInfo::align (int(0))
       << " long="      << StArchInfo::align (long(0))
       << " long long=" << StArchInfo::align ((long long)(0))
       << " float="     << StArchInfo::align (float(0))
       << " double="    << StArchInfo::align (double(0))
       << endl;
  
  int       iProbe;
  float     fProbe;
  short     sProbe;
  char      charProbe;
  char      cProbe[8];
  double    dProbe;
  long      lProbe;
  long long llProbe;

   cout << " char="      << StArchInfo::align (charProbe)
        << " short="     << StArchInfo::align (sProbe)
        << " int="       << StArchInfo::align (iProbe)
        << " long="      << StArchInfo::align (lProbe)
        << " long long=" << StArchInfo::align (llProbe)
        << " float="     << StArchInfo::align (fProbe)
        << " double="    << StArchInfo::align (dProbe)
       << endl;


  cout << "Checking  paddings  - the number of the bytes to be added to some type to align the other type properly. . .  " << endl;

    cout 
       << "         int to get the char=" << StArchInfo::padding<char>(iProbe)      << endl
       << "         int to get the char=" << StArchInfo::padding(iProbe,char(0))    << endl
       << "      float to get the short=" << StArchInfo::padding<short>(fProbe)     << endl
       << "      float to get the short=" << StArchInfo::padding(fProbe,short(0))   << endl
       << "        short to get the int=" << StArchInfo::padding<int>(sProbe)       << endl
       << "        short to get the int=" << StArchInfo::padding(sProbe,int(0))     << endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[0])   << endl
       << "        char to get the long=" << StArchInfo::padding(cProbe[0],long(0)) << endl
       << " double to get the long long=" << StArchInfo::padding<long long>(dProbe) << endl
       << "       long to get the float=" << StArchInfo::padding<float>(lProbe)     << endl
       << "       long to get the float=" << StArchInfo::padding(lProbe,float(0))   << endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[0])   << endl
       << "        char to get the long=" << StArchInfo::padding(cProbe[0],long(0)) << endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[1])   << endl
       << "        char to get the long=" << StArchInfo::padding(cProbe[1],long(0)) << endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[2])   << endl
       << "        char to get the long=" << StArchInfo::padding(cProbe[2],long(0)) << endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[3])   << endl
       << "        char to get the long=" << StArchInfo::padding(cProbe[3],long(0)) << endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[4])   << endl
       << "        char to get the long=" << StArchInfo::padding(cProbe[4],long(0)) << endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe)      << endl
       << "        char to get the long=" << StArchInfo::padding(cProbe,long(0))    << endl
       << " long long to get the double=" << StArchInfo::padding<double>(llProbe)   << endl
       << " long long to get the double=" << StArchInfo::padding(llProbe,double(0)) << endl
       ;


 return StArchInfo::align(long(0));
}
