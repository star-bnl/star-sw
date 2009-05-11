#ifndef __TX_UCM_UTILS__
#define __TX_UCM_UTILS__

#include <string>

namespace TxUCMUtils {
  /**
   * Utility method to trim a string off of its leading and trailing
   * blank characters
   *
   * @param string, string to be trimmed
   * @return string, trimmed
   */
  inline std::string trimString (std::string& str) {
    std::string::size_type pos = str.find_last_not_of (' ');
    if (pos != std::string::npos) {
      str.erase (pos + 1);
      pos = str.find_first_not_of (' ');
      if (pos != std::string::npos) str.erase (0, pos);
    }
    else str.erase (str.begin(), str.end());

    return str;
  }

  /**
   * Utility method to convert integer to string
   *
   * @param string, string to be trimmed
   * @return const char*, stringified integer
   */
  inline std::string itoa (int integer) {
    std::ostringstream str;
    str << integer;
    return str.str ().c_str ();
  }

  /**
   * Utility method to get an environment variable.
   * If not found, then returns "orphan"
   *
   * @param string, env var name
   * @return const char*, env variable value
   */
  inline const char* getEnv (const char* variable) {
    return (getenv (variable) ? getenv (variable) : "orphan");
  }

  /**
   * Utility method to get timestamp in 
   * YYYY:MM:DD HH:MM:SS format
   *
   * @return const char*, timestamp
   */
  inline const char* getTimeStamp () {
    time_t ltime; /* calendar time */  
    struct tm *tm;

    ltime = time (NULL); /* get current cal time */  
    tm = gmtime(&ltime);
  
    std::string ts = "";
    std::ostringstream date;
    date << (tm->tm_year + 1900);
    date << "-";
    if (tm->tm_mon < 9) {
      date << "0";
    }
    date << (tm->tm_mon + 1);
    date << "-";
    if (tm->tm_mday < 10) {
      date << "0";
    }
    date << tm->tm_mday;
    ts += date.str ();
    ts += " ";

    std::ostringstream time;
    if (tm->tm_hour < 10) {
      time << "0";
    }
    time << tm->tm_hour;
    time << "-";
    if (tm->tm_min < 10) {
      time << "0";
    }
    time << tm->tm_min;
    time << "-";
    if (tm->tm_sec < 10) {
      time << "0";
    }
    time << tm->tm_sec;  
    ts += time.str ();
  
    return ts.c_str ();  
  }

}

#endif
