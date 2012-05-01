// StHyperUtilGeneric.h
#ifndef __ST_HYPERUTILGENERIC_H
#define __ST_HYPERUTILGENERIC_H

#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <iterator>

namespace StHyperUtilGeneric
{

std::string toHex(const std::vector<char>& data);
std::string toHex(const char* data_, size_t dataLength);

std::string base64_encode(const char* bytes_to_encode, size_t in_len);
std::string base64_decode(std::string const& str);

template <class T>
std::string toString(T& arg)
{
    std::ostringstream os;
    os << arg;
    return os.str();
}

template<class T>
T fromString(const std::string& s)
{
    std::istringstream stream (s);
    T t;
    stream >> t;
    return t;
}

// generic implode function, for all types
template <typename T>
std::string implode(const std::vector<T>& v, const std::string& delim) {
    std::ostringstream o;
	std::copy(v.begin(), --v.end(), std::ostream_iterator<T>(o, delim.c_str()));
    o << *(--v.end());
    return o.str();
} 

// concatenate strings selected by key, using delimeter string
std::string implode_map(const std::map<std::string, std::string>& arr, const std::string& key, const std::string& delimeter);

// concatenate all elements of vector (strings) using specified delimeter string
std::string implode_vector(const std::vector<std::string>& arr, const std::string& delimeter);

std::vector<std::string> explode(const std::string& str, const char& delim);

void ltrim(std::string& str, const std::string& chr = " \n\r\t");
void rtrim(std::string& str, const std::string& chr = " \n\r\t");
void trim(std::string& str, const std::string& chr = " \n\r\t");

void strtoupper(std::string& str);
void strtolower(std::string& str);

bool recursiveMkdir(const std::string& path);

} // namespace StHyperUtilGeneric

#endif // __ST_HYPERUTILGENERIC_H
