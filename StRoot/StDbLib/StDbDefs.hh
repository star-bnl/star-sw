#ifndef STDBDEFS_HH
#define STDBDEFS_HH

enum StDbType {StarDb=0, DbServer, RunLog, Configurations, Conditions, Calibrations, Geometry, RunCatalog, RunParams };

enum StDbDomain {Unknown=0, Star, Tpc, Emc, Ftpc, Svt, Ctb, Trg, Daq, Scaler, Global, L3 };

#ifdef SOLARIS
#ifndef false
typedef int bool;
#define false 0
#define true 1
#endif
#endif


#endif










