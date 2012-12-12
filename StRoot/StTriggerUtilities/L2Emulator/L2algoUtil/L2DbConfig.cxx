

#include <string>
#include <iostream>
#include <fstream>
#include <cassert>

#include "L2DbConfig.h"
using namespace std;
// ---------------------------------------------------------------------------
L2DbConfig::L2DbConfig( const Char_t *fname )
{
  mFilename=fname;
  ifstream inFile(fname, ifstream::in);  
  if ( !inFile )
    {
      std::cout << "Problem reading in L2EmcDb configuration from " << fname << std::endl;
      assert(2+2==5);
    }
  // check file for errors

  printf("L2DbConfig::L2DbConfig, opened =%s=\n", fname);
  while ( !inFile.eof() )
    {
      L2DbTime c;
      double timeOld=c.getFullStartTime();
      c.read(inFile);
      if ( c.comment() ) continue;
      mConfig.push_back(c); 
      c.print();	      
      assert(c.getFullStartTime() < c.getFullFinishTime());
      //tmp assert(c.getFullStartTime() > timeOld);
      timeOld=c.getFullStartTime();
    }
  std::cout << Form("L2DbConfig::Read in %i records from=%s=, done",mConfig.size(),fname) << std::endl;
  

}

//=================================================================
L2DbTime *L2DbConfig::getConfiguration( Int_t date, Int_t time, const Char_t *tag )
{
  std::cout << "L2DbConfig:: Get configuration for date=" << date << " time=" << time   <<" tag=" <<(tag? tag :"")<<"="<<std::endl;
  for ( UInt_t ii=0;ii<mConfig.size();ii++ )
    {
      if ( mConfig[ii].valid(date,time) )
      {
	  if ( !tag ) return &mConfig[ii];
	  if ( mConfig[ii].getTag().Contains(tag) ) return &mConfig[ii]; 
      }
    }
  return 0; // could not find a match
}

