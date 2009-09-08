#ifndef STAR_STLOGGERCONFIG
#define STAR_STLOGGERCONFIG
//! \file StLoggerConfig.h  the configuration file to account for the different versions of log4cxx package
//! \author Valeri Fine (fine@bnl.gov) 
//! \date 09/08/2009 
#ifndef LOG4CXX_9
#  include <log4cxx/config>
#  include <log4cxx/varia/stringmatchfilter.h>
#  include <log4cxx/varia/denyallfilter.h>
#  define LOG4CXX_LEVEL_FATAL    Level::FATAL
#  define LOG4CXX_LEVEL_ERROR    Level::ERROR
#  define LOG4CXX_LEVEL_WARN     Level::WARN
#  define LOG4CXX_LEVEL_INFO     Level::INFO
#  define LOG4CXX_LEVEL_DEBUG    Level::DEBUG
#else
#  include <log4cxx/logstring.h>
#  include <log4cxx/filter/stringmatchfilter.h>
#  include <log4cxx/filter/denyallfilter.h>
#  include <log4cxx/helpers/messagebuffer.h>
#  define LOG4CXX_LEVEL_FATAL    Level::FATAL_INT
#  define LOG4CXX_LEVEL_ERROR    Level::ERROR_INT
#  define LOG4CXX_LEVEL_WARN     Level::WARN_INT
#  define LOG4CXX_LEVEL_INFO     Level::INFO_INT
#  define LOG4CXX_LEVEL_DEBUG    Level::DEBUG_INT
   namespace log4cxx {
     typedef LogString String;
     typedef helpers::CharMessageBuffer StringBuffer;
  }

  #define _T(str) std::string(str)

#endif /* LOG4CXX10 */

#endif
