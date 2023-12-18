#include "FwdTrackerConfig.h"

const std::string FwdTrackerConfig::valDNE = std::string( "<DNE/>" );
const std::string FwdTrackerConfig::pathDelim = std::string( "." );
const std::string FwdTrackerConfig::attrDelim = std::string( ":" );
std::stringstream FwdTrackerConfig::sstr;

////
// template specializations
////

/**
 * @brief write a value to path
 * 
 * @tparam  template specialization for std::string
 * @param path path to write, if it DNE it is created
 * @param v value (of type string) to write
 */
template <>
void FwdTrackerConfig::set( std::string path, std::string v ) {

    FwdTrackerConfig::canonize( path );
    // convrt from string to type T and return
    mNodes[ path ] = v;
}

/**
 * @brief write a value to path
 * 
 * @tparam  template specialization for bool
 * @param path path to write, if it DNE it is created
 * @param bv boolean to write
 */
template <>
void FwdTrackerConfig::set( std::string path, bool bv ) {

    FwdTrackerConfig::canonize( path );
    // convrt from string to type T and return
    std::string v = "false";
    if (bv)
        v = "true";
    mNodes[ path ] = v;
}

// 
/**
 * @brief Get a value from the path
 * 
 * @tparam  Specialization for string to avoid extra conversions
 * @param path path to lookup
 * @param dv default value if path DNE
 * @return std::string value at path or default
 */
template <>
std::string FwdTrackerConfig::get( std::string path, std::string dv ) const {
    // return default value if path DNE
    if ( !exists( path ) )
        return dv;
    FwdTrackerConfig::canonize( path );
    // directly return string
    return ( mNodes.at( path ) );
}

/**
 * @brief conversion to string is a noop
 * 
 * @tparam  string specialization
 * @param str input
 * @return std::string output (unchanged)
 */
template <>
std::string FwdTrackerConfig::convert( std::string str ) const {
   return str;
}

/**
 * @brief specialization for bool adds recognition of strings "true" and "false" (lower case)
 * 
 * @tparam  bool specialization, fallback to int check
 * @param str input string 
 * @return true for "true"
 * @return false for "false"
 */
template <>
bool FwdTrackerConfig::convert( std::string str ) const {

    if ( str == "false" )
       return false;

    if ( str == "true" )
       return true;
    // fall back to an int cast
    return static_cast<bool>(convert<int>( str ));
}

/**
 * @brief get as ROOT TString
 * 
 * @tparam  TString specialization
 * @param str input value
 * @return TString output as ROOT TString
 */
template <>
TString FwdTrackerConfig::convert(std::string str) const {
    TString r(str);
    return r;
}