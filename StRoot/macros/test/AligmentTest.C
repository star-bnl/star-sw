#include "StArchInfo.h"
#include <iostream>
using namespace std;
int AligmentTest() {
  cout << "Checking aligments  . . .  " << endl;
  cout << " char=" <<  StArchInfo::align<char>()
       << " short=" << StArchInfo::align<short>()
       << " int=" <<  StArchInfo::align<int>()
       << " long=" <<  StArchInfo::align<long>()
       << " long long=" <<  StArchInfo::align<long long>()
       << " float=" << StArchInfo::align<float>()
       << " double=" <<  StArchInfo::align<double>()
       << endl;
   cout << " char=" <<  StArchInfo::align(char(0))
       << " short=" << StArchInfo::align(short(0))
       << " int=" <<  StArchInfo::align (int(0))
       << " long=" <<  StArchInfo::align (long(0))
       << " long long=" <<  StArchInfo::align ((long long)(0))
       << " float=" << StArchInfo::align (float(0))
       << " double=" <<  StArchInfo::align (double(0))
       << endl;
  int       iProbe;
  float     fProbe;
  short     sProbe;
  char      cProbe[8];
  double    dProbe;
  long      lProbe;
  long long llProbe;
         
  cout << "Checking  paddings  - the number of the bytes to be added to some type to align the other type properly. . .  " << endl;

    cout 
       << "         int to get the char=" <<  StArchInfo::padding<char>(iProbe)   << endl
       << "      float to get the short=" <<  StArchInfo::padding<short>(fProbe)<< endl
       << "        short to get the int=" <<  StArchInfo::padding<int>(sProbe)<< endl
       << "        char to get the long=" <<  StArchInfo::padding<long>(cProbe[0])<< endl
       << " double to get the long long=" <<  StArchInfo::padding<long long>(dProbe)<< endl
       << "       long to get the float=" << StArchInfo::padding<float>(lProbe)<< endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[0])<< endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[1])<< endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[2])<< endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[3])<< endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe[4])<< endl
       << "        char to get the long=" << StArchInfo::padding<long>(cProbe)<< endl
       << " long long to get the double=" <<  StArchInfo::padding<double>(llProbe)
       << endl;


 return StArchInfo::align(long(0));
}
