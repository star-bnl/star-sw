
#ifndef __ROOT__  
#include <rtsLog.h>    // use real logging...
#else 

#define DBG "DBG"
#define NOTE "NOTE"
#define WARN "WARN"
#define ERR "ERR"
#define CRIT "CRIT"
#define INFO "INFO"

#define LOG(level,format, args...)			\
  printf("%s:%s %d" format, level, __FILE__, __LINE__ , ## args)

#endif
