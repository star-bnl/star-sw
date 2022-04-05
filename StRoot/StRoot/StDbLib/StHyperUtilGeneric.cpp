#include "StHyperUtilGeneric.h"

#include <sys/stat.h>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <algorithm>

namespace StHyperUtilGeneric
{

static const std::string base64_chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789+/";

static inline bool st_hyper_is_base64(unsigned char c)
{
    return (isalnum(c) || (c == '+') || (c == '/'));
}

std::string base64_encode(const char* bytes_to_encode, size_t in_len) {
    std::string ret;
    int i = 0;
    int j = 0;
    unsigned char char_array_3[3];
    unsigned char char_array_4[4];
    while (in_len--) {
        char_array_3[i++] = *(bytes_to_encode++);
        if (i == 3) {
            char_array_4[0] = (char_array_3[0] & 0xfc) >> 2;
            char_array_4[1] = ((char_array_3[0] & 0x03) << 4) + ((char_array_3[1] & 0xf0) >> 4);
            char_array_4[2] = ((char_array_3[1] & 0x0f) << 2) + ((char_array_3[2] & 0xc0) >> 6);
            char_array_4[3] = char_array_3[2] & 0x3f;

            for(i = 0; (i <4) ; i++)
                ret += base64_chars[char_array_4[i]];
            i = 0;
        }
    }
    if (i) {
        for(j = i; j < 3; j++)
            char_array_3[j] = '\0';

        char_array_4[0] = (char_array_3[0] & 0xfc) >> 2;
        char_array_4[1] = ((char_array_3[0] & 0x03) << 4) + ((char_array_3[1] & 0xf0) >> 4);
        char_array_4[2] = ((char_array_3[1] & 0x0f) << 2) + ((char_array_3[2] & 0xc0) >> 6);
        char_array_4[3] = char_array_3[2] & 0x3f;

        for (j = 0; (j < i + 1); j++)
            ret += base64_chars[char_array_4[j]];

        while((i++ < 3))
            ret += '=';

    }
    return ret;
}

std::string base64_decode(std::string const& encoded_string)
{
    int in_len = encoded_string.size();
    int i = 0;
    int j = 0;
    int in_ = 0;
    unsigned char char_array_4[4], char_array_3[3];
    std::string ret;

    while (in_len-- && ( encoded_string[in_] != '=') && st_hyper_is_base64(encoded_string[in_])) {
        char_array_4[i++] = encoded_string[in_];
        in_++;
        if (i ==4) {
            for (i = 0; i <4; i++)
                char_array_4[i] = base64_chars.find(char_array_4[i]);

            char_array_3[0] = (char_array_4[0] << 2) + ((char_array_4[1] & 0x30) >> 4);
            char_array_3[1] = ((char_array_4[1] & 0xf) << 4) + ((char_array_4[2] & 0x3c) >> 2);
            char_array_3[2] = ((char_array_4[2] & 0x3) << 6) + char_array_4[3];

            for (i = 0; (i < 3); i++)
                ret += char_array_3[i];
            i = 0;
        }
    }
    if (i) {
        for (j = i; j <4; j++)
            char_array_4[j] = 0;

        for (j = 0; j <4; j++)
            char_array_4[j] = base64_chars.find(char_array_4[j]);

        char_array_3[0] = (char_array_4[0] << 2) + ((char_array_4[1] & 0x30) >> 4);
        char_array_3[1] = ((char_array_4[1] & 0xf) << 4) + ((char_array_4[2] & 0x3c) >> 2);
        char_array_3[2] = ((char_array_4[2] & 0x3) << 6) + char_array_4[3];

        for (j = 0; (j < i - 1); j++) ret += char_array_3[j];
    }

    return ret;
}


std::string toHex(const std::vector<char>& data)
{
    std::ostringstream os( std::ostringstream::out );
    for (std::vector<char>::const_iterator cur(data.begin()); cur != data.end(); ++cur) {
        os << std::setfill('0') << std::setw(2) << std::hex << std::uppercase << (unsigned short)(*cur);
    }
    return os.str();
}

std::string toHex(const char* data_, size_t dataLength)
{
    std::vector<char> data(data_, data_+dataLength);
    return toHex(data);
}

std::string implode_map(const std::map<std::string, std::string>& arr, const std::string& key, const std::string& delimeter)
{
    std::string impl_string = "";
    std::string delim = "";
    for (std::map<std::string,std::string>::const_iterator iter = arr.lower_bound(key); iter != arr.upper_bound(key); ++iter) {
        impl_string.append(delim);
        impl_string.append((*iter).second);
        delim = delimeter;
    }
    return impl_string;
}

std::string implode_vector(const std::vector<std::string>& arr, const std::string& delimeter)
{
    std::string impl_string = "";
    std::string delim = "";
    for (std::vector<std::string>::const_iterator iter = arr.begin(); iter != arr.end(); ++iter) {
        impl_string.append(delim);
        impl_string.append( (*iter) );
        delim = delimeter;
    }
    return impl_string;
}

std::vector<std::string> explode(const std::string& str, const char& delim) {
	std::vector<std::string> result;
	std::string token;
	std::istringstream iss(str);
	while ( getline(iss, token, delim) ) {
		result.push_back(token);
	}
	return result;
}

void rtrim(std::string& str, const std::string& chr)
{
    size_t endpos = str.find_last_not_of(chr);
    if( std::string::npos != endpos ) {
        str = str.substr( 0, endpos+1 );
    }
}

void ltrim(std::string& str, const std::string& chr)
{
    size_t startpos = str.find_first_not_of(chr);
    if( std::string::npos != startpos ) {
        str = str.substr( startpos, str.size() - startpos);
    }
}

void trim(std::string& str, const std::string& chr)
{
    rtrim(str, chr);
    ltrim(str, chr);
}

void strtoupper(std::string& str) {
	std::transform(str.begin(), str.end(), str.begin(), toupper);
}

void strtolower(std::string& str) {
	std::transform(str.begin(), str.end(), str.begin(), tolower);
}

bool recursiveMkdir(const std::string& path)
{
    std::string str(path);
    trim(str);
    rtrim(str, "/ \n\r\t");
    size_t found = str.find_first_of('/');
    while (found != std::string::npos) {
        if ( mkdir(str.substr(0, found).c_str(), S_IRWXU) != 0 ) {
			// FIXME: directory was not created by some reason
		}
        found = str.find_first_of('/', found+1);
    }
	if ( mkdir(str.c_str(), S_IRWXU) != 0 ) {
		// FIXME: directory was not created?
	}
    return true;
}

} // namespace StHyperUtilGeneric

