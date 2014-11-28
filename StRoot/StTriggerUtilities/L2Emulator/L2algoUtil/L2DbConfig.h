#ifndef __L2DbConfig_h__
#define __L2DbConfig_h__
/*
 * \class L2DbConfig
 *
 */

#include <TObject.h>
#include <TString.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

#include "L2DbTime.h"

class L2DbConfig 
{

 public: 

  L2DbConfig( const Char_t *fname );
  ~L2DbConfig(){ /* nada */ };

  // Get configuration from the specified date / time 
  L2DbTime *getConfiguration( Int_t date, Int_t time, const Char_t *tag=0 );


 private:
 protected:

  TString mFilename;
  std::vector<L2DbTime> mConfig;


};


#endif
