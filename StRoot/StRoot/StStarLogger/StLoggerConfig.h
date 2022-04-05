#ifndef STAR_STLOGGERCONFIG
#define STAR_STLOGGERCONFIG
//! \file StLoggerConfig.h  the configuration file to account for the different versions of log4cxx package
//! \author Valeri Fine (fine@bnl.gov) 
//! \date 09/08/2009 

#if __GNUC__ >= 4
#  define STAR_LOG4CXX_VERSION 10
#else
#  define STAR_LOG4CXX_VERSION 9
#endif

#include <log4cxx/helpers/stringhelper.h>

#if (STAR_LOG4CXX_VERSION == 9)
#  include <log4cxx/config.h>
#  include <log4cxx/varia/stringmatchfilter.h>
#  include <log4cxx/varia/denyallfilter.h>
#  define LOG4CXX_LEVEL_FATAL    Level::FATAL
#  define LOG4CXX_LEVEL_ERROR    Level::ERROR
#  define LOG4CXX_LEVEL_WARN     Level::WARN
#  define LOG4CXX_LEVEL_INFO     Level::INFO
#  define LOG4CXX_LEVEL_DEBUG    Level::DEBUG
#  define LocationInfo(sourceFileName,sourceFunction,lineNumber)  sourceFileName,lineNumber
#else
#  include <log4cxx/logstring.h>
#  include <log4cxx/filter/stringmatchfilter.h>
#  include <log4cxx/filter/denyallfilter.h>
#  include <log4cxx/helpers/messagebuffer.h>
#  define LOG4CXX_LEVEL_FATAL    Level::getFatal()
#  define LOG4CXX_LEVEL_ERROR    Level::getError()
#  define LOG4CXX_LEVEL_WARN     Level::getWarn()
#  define LOG4CXX_LEVEL_INFO     Level::getInfo()
#  define LOG4CXX_LEVEL_DEBUG    Level::getDebug()
   namespace log4cxx {
     typedef LogString String;
     typedef helpers::CharMessageBuffer StringBuffer;
  }

  #define _T(str) std::string(str)
  
#endif /* LOG4CXX10 */

namespace log4cxx {
   
   inline bool equalsIgnoreCase(const String& option1, const String& option2) 
   {
      return
        helpers::StringHelper::equalsIgnoreCase(option1,option2
#if  (STAR_LOG4CXX_VERSION==10)  
            ,option2
#endif
         );
   }
}
#endif  /* STAR_STLOGGERCONFIG   */
