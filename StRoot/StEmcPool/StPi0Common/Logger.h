#ifndef Logger_H
#define Logger_H

#ifndef LOG_INFO
#include <iostream>
using std::cout;
using std::cerr;
using std::endl;
#define LOG_INFO cout
#define endm  endl
#endif

#ifndef LOG_WARN
#define LOG_WARN cout
#endif

#ifndef LOG_INFO
#define LOG_INFO cout
#endif

#ifndef LOG_ERROR
#define LOG_ERROR  cerr
#endif

#ifndef LOG_FATAL
#define LOG_FATAL cerr
#endif

#ifndef LOG_DEBUG
#define LOG_DEBUG cout
#endif

#ifndef LOG_QA
#define LOG_QA cout
#endif

#endif
