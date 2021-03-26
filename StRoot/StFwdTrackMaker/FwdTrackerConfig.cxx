#include "FwdTrackerConfig.h"

const std::string FwdTrackerConfig::valDNE = std::string( "<DNE/>" );
const std::string FwdTrackerConfig::pathDelim = std::string( "." );
const std::string FwdTrackerConfig::attrDelim = std::string( ":" );
std::stringstream FwdTrackerConfig::sstr;

////
// template specializations
////

// Specialization for string to avoid extra conversions
template <>
std::string FwdTrackerConfig::get( std::string path, std::string dv ) const {
    // return default value if path DNE
    if ( !exists( path ) )
        return dv;
    FwdTrackerConfig::canonize( path );
    // directly return string
    return ( mNodes.at( path ) );
}

// conversion to string is a noop
template <>
std::string FwdTrackerConfig::convert( std::string str ) const {
   return str;
}

// specialization for bool adds recognition of strings "true" and "false" (lower case)
template <>
bool FwdTrackerConfig::convert( std::string str ) const {

    if ( str == "false" )
       return false;

    if ( str == "true" )
       return true;
    // fall back to an int cast
    return static_cast<bool>(convert<int>( str ));
}

// get as ROOT TString
template <>
TString FwdTrackerConfig::convert(std::string str) const {
    TString r(str);
    return r;
}