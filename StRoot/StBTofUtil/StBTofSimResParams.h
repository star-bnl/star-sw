//
//  StBTofSimResParams.h
//
//
//  Created by jdb on 03/08/18.
//
//

#ifndef StBTofSimResParams_h
#define StBTofSimResParams_h

#include <iostream>
#include <fstream>
#include <vector>
#include "StDetectorDbMaker/St_tofSimResParamsC.h"


// using std::string;
// class tofSimResParams_st;

class StBTofSimResParams {
 public:
  
  StBTofSimResParams() {}
  ~StBTofSimResParams() {}
  
  double average_timeres_tof(){return mAverageTimeResTof;}
  
  /**
   * Calculates the average resolution across all 38 tubes (discounts inactive tubes)
   * then returns a single vertex resolution (in ps) for use in embedding w/ vpdStart
   */
  double timeres_tof(uint itray, uint imodule, uint icell) {
    double result = 8.5e-11;
    if ( itray > 120 || imodule > 32 || icell > 6 )
      return result;
    
    return params[ itray ][ imodule * 6 + icell ];
    
  }
  
  //! Loads Vpd Sim Params from database
  
  void loadParams(const int date = 20160913, const int time = 175725, const char* Default_time = "2016-09-13 17:57:25")
  {
    
    mAverageTimeResTof=0;
    for ( int i = 0; i < 120; i++ ){ //  nTrays
      for ( int j = 0; j < 192; j++ ){
	size_t index = i * 120 + j;
	params[i][j] = St_tofSimResParamsC::instance()->resolution()[index];
	mAverageTimeResTof+=params[i][j];
	LOG_DEBUG << "tray:" << i << ", mod cell:" << j << " = " << St_tofSimResParamsC::instance()->resolution()[index] << " == " << params[i][j] << endm;
      }
    }
    mAverageTimeResTof=mAverageTimeResTof/(120*192);
    LOG_INFO << "Loaded tofSimResParams. Average = " << mAverageTimeResTof << endm;
  }
  // loadParams
  
 protected:
  double params[120][192];
  double mAverageTimeResTof;
};

#endif /* Config_h */
