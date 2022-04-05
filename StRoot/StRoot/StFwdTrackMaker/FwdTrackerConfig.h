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

    // assumes bare path and adds [i] until DNE
    // reports lowest non-existant index
    // starts at 1 since 0 is checked on existance
    size_t pathCount( const std::string path ){
        size_t index = 1;
        std::string p = path + TString::Format( "[%zu]", index ).Data();
        while ( mNodes.count( p ) ){
            index ++;
            p = path + TString::Format( "[%zu]", index ).Data();
        }
        return index;
    }

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

    // sanitizes a path to its canonical form 
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

    // dump config to a basic string representation - mostly for debugging
    std::string dump() const {
        using namespace std;
        FwdTrackerConfig::sstr.str("");
        FwdTrackerConfig::sstr.clear();
        for ( auto kv : mNodes ){
            FwdTrackerConfig::sstr << "[" << kv.first << "] = " << kv.second << endl;
        }
        return FwdTrackerConfig::sstr.str();
    }

    // Does a path exist
    // Either node or attribute - used to determine if default value is used
    bool exists( std::string path ) const {
        FwdTrackerConfig::canonize( path );
        if ( 0 == mNodes.count( path ) )
            return false;
        return true;
    }

    // generic conversion to type T from std::string
    // override this for special conversions
    template <typename T>
    T convert( std::string s ) const {
        T rv;
        FwdTrackerConfig::sstr.str("");
        FwdTrackerConfig::sstr.clear();
        FwdTrackerConfig::sstr << s;
        FwdTrackerConfig::sstr >> rv;
        return rv;
    }

    


    // template function for getting any type that can be converted from string via stringstream
    template <typename T>
    T get( std::string path, T dv ) const {
    
        // return default value if path DNE
        if ( !exists( path ) )
            return dv;

        FwdTrackerConfig::canonize( path );
        // convrt from string to type T and return
        return convert<T>( mNodes.at( path ) );
    }

    

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

    // list the paths of children nodes for a given node
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

    // Constructor is noop, use load(...)
    FwdTrackerConfig() {}

    // constructor that immediately loads an xml file
    FwdTrackerConfig(std::string filename) {
        load( filename );
    }

    // Main setup routine.
    // Loads the given XML file and maps it
    void load( std::string filename ) {
        using namespace std;

        // empty the map of mNodes
        mNodes.clear();

        // Create XML engine for parsing file
        TXMLEngine xml;

        // Now try to parse xml file
        XMLDocPointer_t xmldoc = xml.ParseFile(filename.c_str());
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

#endif
