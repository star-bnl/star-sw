/*!
 * Author: Mikhail Kopytin
 * Abstract: various utilities to work with C and C++ strings
*/


#ifndef ChapiStringUtilities_h
#define ChapiStringUtilities_h
#include <vector>
#include <map>
#include <sstream>
#include <iostream>

namespace chapi_string_utilities
{

  /* the third parameter of from_string() should be 
     one of std::hex, std::dec or std::oct. See
http://www.codeguru.com/forum/showthread.php?t=231054
 */

template <class T>
bool from_string(T& t, 
                 const std::string& s, 
                 std::ios_base& (*f)(std::ios_base&))
  {
    std::istringstream iss(s);
    return !(iss >> f >> t).fail();
  }  

  extern std::vector<std::string> slice(std::string A, std::string sep);
  extern std::map<std::string,std::string> associate_pieces(std::vector<std::string> v, std::string sep);
  extern void cut_string_after_sub(std::string& input, const std::string& sub);
  extern bool good_character(char* src);
  extern std::string filter_string(char* src);
}
#endif
