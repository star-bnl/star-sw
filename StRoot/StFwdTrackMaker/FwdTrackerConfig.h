#ifndef FWD_TRACKER_CONFIG_H
#define FWD_TRACKER_CONFIG_H

#include "St_base/StMessMgr.h"

#include "TXMLEngine.h"
#include "TString.h"

#include <string>
#include <iostream>
#include <sstream>
#include <vector>
#include <map>
#include <algorithm>

// Class provides an interface for reading configuration from an XML file
class FwdTrackerConfig {
protected:

    static const std::string valDNE; // used for nodes that DNE
    static const std::string pathDelim; // separate node levels
    static const std::string attrDelim; // separate attributes on nodes

    bool mErrorParsing = false;
    // read only map of the config, read with get<> functions
    std::map<std::string, std::string> mNodes;
    static std::stringstream sstr; // reused for string to numeric conversion

    /**
     * @brief get lowest non-existing path index
     * assumes bare path and adds [i] until DNE
     * starts at 1 since 0 is checked on existance
     * @param path base path to check
     * @return size_t index, starts at 1
     */
    size_t pathCount( const std::string path ){
        size_t index = 1;
        std::string p = path + TString::Format( "[%zu]", index ).Data();
        while ( mNodes.count( p ) ){
            index ++;
            p = path + TString::Format( "[%zu]", index ).Data();
        }
        return index;
    }

    /**
     * @brief Reads an xml document and writes it into map
     * 
     * @param xml xml document to map
     * @param node starting node - allows recursive mapping
     * @param level the integer index of the level of current parsing
     * @param path the path for the current node
     */
    void mapFile(TXMLEngine &xml, XMLNodePointer_t node, Int_t level, std::string path = "") {
        using namespace std;
        // add the path delimeter above top level
        if ( !path.empty() ) path += FwdTrackerConfig::pathDelim;

        // we skip the root node to maintain consistency with original XmlConfig
        if ( level > 1)
            path += xml.GetNodeName(node);

        // get the node name and content if it exists
        const string node_name = xml.GetNodeName(node);
        const string node_content = xml.GetNodeContent(node) != nullptr ? xml.GetNodeContent(node) : FwdTrackerConfig::valDNE;

        // be careful about repeated nodes
        if ( mNodes.count( path ) == 0 ) {
            mNodes[ path ] = node_content;
        } else { // add an array index if more than one
            size_t index = pathCount( path );
            path += TString::Format( "[%zu]", index ).Data();
            mNodes[ path ] = node_content;
        }

        // loop through attributes of this node
        XMLAttrPointer_t attr = xml.GetFirstAttr(node);
        while (attr != 0) {

            // get the attribute name and value if exists
            const string attr_name = xml.GetAttrName(attr);
            const string attr_val = xml.GetAttrValue(attr) != nullptr ? xml.GetAttrValue(attr) : FwdTrackerConfig::valDNE;
            
            // save attributes with the attribute delim ":" 
            mNodes[ (path + FwdTrackerConfig::attrDelim + attr_name) ] = attr_val;
            attr = xml.GetNextAttr(attr);
        }

        // recursively get child nodes
        XMLNodePointer_t child = xml.GetChild(node);
        while (child != 0) {
            mapFile(xml, child, level + 1, path);
            child = xml.GetNext(child);
        }
    } // mapFile
public:

    /**
     * @brief Returns a path in its cannonical form
     * 
     * @param path Path to cannoize, returned in place by reference
     */
    static void canonize( std::string &path ) {
        // remove whitespace
        path.erase(std::remove_if(path.begin(), path.end(), static_cast<int(*) (int)>(std::isspace)), path.end());

        // removes "[0]" found in paths, so that the first element in a list can be accessed by index 0 or bare path
        size_t pos = path.find( "[0]" );

        // branchless version using ternary op
        size_t len = (pos != std::string::npos) ? 3 : 0;
        pos = (pos != std::string::npos) ? pos : 0;
        path.erase( pos, len ); // does nothing if "[0]" not found
        return;
    }

    /**
     * @brief dump config to a basic string representation - mostly for debugging
     * 
     * @return std::string 
     */
    std::string dump() const {
        using namespace std;
        FwdTrackerConfig::sstr.str("");
        FwdTrackerConfig::sstr.clear();
        for ( auto kv : mNodes ){
            FwdTrackerConfig::sstr << "[" << kv.first << "] = " << kv.second << endl;
        }
        return FwdTrackerConfig::sstr.str();
    }

    /**
     * @brief returns whether or not a path exist
     * Either node or attribute - used to determine if default value is used
     * 
     * @param path - the path to check
     * @return true : path exists
     * @return false : path DNE
     */
    bool exists( std::string path ) const {
        FwdTrackerConfig::canonize( path );
        if ( 0 == mNodes.count( path ) )
            return false;
        return true;
    }

    /**
     * @brief Generic conversion of type T from string
     * override this for special conversions
     * 
     * @tparam T : Type to convert to and return
     * @param s : input string to use for conversion
     * @return T converted value of type T
     */
    template <typename T>
    T convert( std::string s ) const {
        T rv;
        FwdTrackerConfig::sstr.str("");
        FwdTrackerConfig::sstr.clear();
        FwdTrackerConfig::sstr << s;
        FwdTrackerConfig::sstr >> rv;
        return rv;
    }

    /**
     * @brief Generic conversion of type T to a string
     * 
     * @tparam T : type to convert
     * @param v : value of type T
     * @return std::string output string with representation of T
     */
    template <typename T>
    std::string convertTo( T v ) const {
        FwdTrackerConfig::sstr.str("");
        FwdTrackerConfig::sstr.clear();
        FwdTrackerConfig::sstr << v;
        return FwdTrackerConfig::sstr.str();
    }


    /**
     * @brief template function for getting any type that can be converted from string via stringstream
     * 
     * @tparam T type to return 
     * @param path path to lookup
     * @param dv default value to return if the node DNE
     * @return T return value of type T
     */
    template <typename T>
    T get( std::string path, T dv ) const {
    
        // return default value if path DNE
        if ( !exists( path ) )
            return dv;

        FwdTrackerConfig::canonize( path );
        // convrt from string to type T and return
        return convert<T>( mNodes.at( path ) );
    }

    /**
     * @brief Writes a value of type T to the map
     * Uses convertTo<T> to convert type T to a string rep 
     * @tparam T type of value to write
     * @param path path to write to
     * @param v value of type T
     */
    template <typename T>
    void set( std::string path, T v ) {
        FwdTrackerConfig::canonize( path );
        // convrt from string to type T and return
        mNodes[ path ] = convertTo<T>( v );
    }
    
    /**
     * @brief Get a Vector object from config
     * 
     * @tparam T type of value for the vector object
     * @param path path to lookup
     * @param dv default value, can use initializer list
     * @return std::vector<T> vector of type T returned
     */
    template <typename T>
    std::vector<T> getVector( std::string path, std::vector<T> dv ) const {
        if ( !exists( path ) )
            return dv;
        
        FwdTrackerConfig::canonize( path );
        std::string val = mNodes.at( path );
        // remove whitespace
        val.erase(std::remove_if(val.begin(), val.end(), static_cast<int(*) (int)>(std::isspace) ), val.end());
        std::vector<std::string> elems;

        // split the string by commas
        [&]() {
            std::stringstream  ss(val);
            std::string str;
            while (std::getline(ss, str, ',')) {
                elems.push_back(str);
            }
        }();

        // for each element, convert to type T and push into vector
        std::vector<T> result;
        for ( auto sv : elems ){
            result.push_back( convert<T>( sv ) );
        }
        return result;
    }

    /**
     * @brief list the paths of children nodes for a given node
     * 
     * @param path path to search for children
     * @return std::vector<std::string> list of full paths to the children nodes
     */
    std::vector<std::string> childrenOf( std::string path ) const {
        using namespace std;
        vector<string> result;

        canonize(path);

        // test a path to see if it is an attribute
        auto is_attribute = [&](string str){
            return ( str.find( FwdTrackerConfig::attrDelim ) != string::npos );
        };

        for ( auto kv : mNodes ){
            // get the first n characters of this path
            string parent = (kv.first).substr( 0, path.length() );

            // dont add self as a child
            if ( parent == kv.first ) continue;

            // if parent path matches query path then it is a child.
            if ( parent == path && !is_attribute( kv.first )){
                result.push_back( kv.first );
            }
        } // loop over all nodes

        return result;
    }

    /**
     * @brief Constructor is noop, use load(...)
     * 
     */
    FwdTrackerConfig() {}

    /**
     * @brief Construct a new Fwd Tracker Config object and load a file
     * 
     * @param filename 
     */
    FwdTrackerConfig(std::string filename) {
        load( filename );
    }

    /**
     * @brief Main setup routine
     * Loads the given XML file (or string) and maps it
     * @param filename filename (or xml string) to load. If file the content is loaded as an xml doc
     * @param asString false: filename is loaded and contents treated as xml doc, true: treat the string `filename` directly as an xml doc
     */
    void load( std::string filename, bool asString = false ) {
        using namespace std;

        // empty the map of mNodes
        mNodes.clear();

        // Create XML engine for parsing file
        TXMLEngine xml;

        // Now try to parse xml file
        XMLDocPointer_t xmldoc;
        if (asString)
            xmldoc = xml.ParseString(filename.c_str());
        else
            xmldoc = xml.ParseFile(filename.c_str());

        if (!xmldoc) { // parse failed, TODO inform of error
            mErrorParsing = true;
            return;
        }

        // access to root node (should be "config")
        XMLNodePointer_t root_node = xml.DocGetRootElement(xmldoc);
        // build the file map for config access
        mapFile(xml, root_node, 1);

        // Release memory before finishing
        xml.FreeDoc(xmldoc);
    }
};

// Forward declare the templates, otherwise undefined behavior in Release builds
template <>
std::string FwdTrackerConfig::convert( std::string str ) const;
template <>
bool FwdTrackerConfig::convert( std::string str ) const;
template <>
TString FwdTrackerConfig::convert(std::string str) const;
template <>
std::string FwdTrackerConfig::get( std::string path, std::string dv ) const;
template <>
void FwdTrackerConfig::set( std::string path, std::string v );
template <>
void FwdTrackerConfig::set( std::string path, bool bv );

#endif
