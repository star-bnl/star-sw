#ifndef DAQDB_HH
#define DAQDB_HH

inline int getCondtionsDbPort(int run)
{
  if(run <= 4171012) {        // 2003 or prior...
    return 3402;
  }
  
  if (run <= 5201019) {  // 2004 or prior...
    return 3403;
  }
  
  return 3501;
}



// DAQMAN files....
#define FILE_daqCnd "/data/dbSpool/daqCndDb"
#define FILE_daqCndTw "/data/dbSpool/daqCndTw"
#define FILE_daqCndEndRun "/data/dbSpool/daqCndEndRun"

// scalers....
#define FILE_l1_counter "/data/dbSpool/l1counterDb"
#define FILE_l2_counter "/data/dbSpool/l2counterDb"
#define FILE_l3_1_counter "/data/dbSpool/l3_1_counterDb"
#define FILE_l3_2_counter "/data/dbSpool/l3_2_counterDb"
#define FILE_l3_3_counter "/data/dbSpool/l3_3_counterDb"
#define FILE_scaler "/data/dbSpool/scalerDb"

// Buffbox files....
#define FILE_daqEventTag "/a/dbSpool/daqEventTagDb"
#define FILE_daqRunTag "/a/dbSpool/daqRunTagDb"
#define FILE_daqFileTag "/a/dbSpool/daqTagFilesDb"
#define FILE_daqFileTagUpdate "/a/dbSpool/daqTagFilesUpdateDb"
#define FILE_daqhpssUpdate "/a/dbSpool/hpssUpdateDb"

#endif
