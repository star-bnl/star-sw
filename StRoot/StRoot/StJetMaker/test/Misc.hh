#ifndef MISC_HH
#define MISC_HH

#include <sstream>

template <class T>
inline std::string to_string (const T& t)
{
  std::stringstream ss;
  ss << t;
  return ss.str();
}

#endif // MISC_HH
