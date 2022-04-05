#ifndef FWD_TRACK_MAKER_COMMON_H
#define FWD_TRACK_MAKER_COMMON_H

#include "TAxis.h"
#include <vector>
#include <string>
#include <chrono>

class FwdTrackerUtils {
    public:
    static void labelAxis( TAxis *axis, std::vector<std::string> labels )
    {
       if ( !axis ) return;

       for ( unsigned int i = 0; i <= (unsigned int)axis->GetNbins(); i++ ) {
          if ( i < labels.size() )
             axis->SetBinLabel( i + 1, labels[i].c_str() );
       }
    }

    static long long nowNanoSecond(){
      return std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
    }
};

#endif