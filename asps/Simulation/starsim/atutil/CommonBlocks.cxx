#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <assert.h>
using namespace std;

#include "CommonBlocks.h"

// Static instance of the class
CommonBlocks CommonBlocks::sInstance;

CommonBlocks &CommonBlocks::instance(){ return sInstance; }

void CommonBlocks::Register( string name, int *ptr )
{
  sInstance.mTable[name]=ptr;
}
void *CommonBlocks::Address( string name )
{
  void *ptr = sInstance.mTable[name];
  if ( !ptr )
    {
      cout << "Warning: common block " << name.c_str() << " not registered.  Better to just crash now." << endl << flush;
      assert(ptr);
    }
  return sInstance.mTable[name];
}

extern "C" {
  void register_common_( char *name, int &location, int len_name )
  {
    string temp = name;
    temp = temp.substr(0,len_name);
    CommonBlocks::Register(temp.c_str(),&location);
  }
  void *address_of_common( string name )
  {
    return (void *)CommonBlocks::Address(name);
  }
  void *address_of_common_( char *name )
  {
    string Name=name;
    return address_of_common(Name);
  }
};
