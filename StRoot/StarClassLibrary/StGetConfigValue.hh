/***************************************************************************
 *
 * $Id: StGetConfigValue.hh,v 1.3 2003/09/02 17:59:34 perev Exp $
 *
 * Author: Thomas Ullrich, Nov 4, 1997
 ***************************************************************************
 *
 * Description:
 * Returns resource value read from a configuration file.
 * There are two version of StGetConfigValue(). One for
 * reading a scalar resource and one for a multi-value
 * resources (e.g arrays).
 *
 * Example:
 * EtInt foo = 10;
 * StGetConfigValue("my.conf", "foo", foo);
 * // foo == 10 in case foo is not defined in my.conf.
 *
 * The second version has different arguments.
 *
 * Example:
 * EtInt afoo[3];
 * StGetConfigValue("my.conf", "afoo", afoo, 3);
 *
 * Syntax of the resource file:
 * The resource (configuration) file has to be in ascii format.
 * Each resource line must be defined as:
 *         resource: value
 * as in:
 * foo:     17
 * afoo:    0.1 0.2 0.3
 *
 * The following comment characters are supported: '//' and '#'
 * Example:
 * // foo:  10 
 * #  afoo: 10 20 30
 * foo:  20       # was 10 
 *
 * Note that no warning is submitted in case the file doesn't exist
 * or isn't readable.
 ***************************************************************************
 *
 * $Log: StGetConfigValue.hh,v $
 * Revision 1.3  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  1999/12/21 15:14:00  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/01/30 03:59:01  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_GET_CONFIG_VALUE_HH
#define ST_GET_CONFIG_VALUE_HH

#include "Stiostream.h"
#include <Stsstream.h>
#include <string>
#include "StGlobals.hh"
#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif

template<class T>
void StGetConfigValue(const char* filename, const char* name, T& value)
{
    ifstream ifs(filename);
    if (!ifs) return;
    
    string       line;
    StSizeType   pos;
    while (ifs.good() && !ifs.eof()) {
#if defined(__SUNPRO_CC)
	char c; line.erase();
	while ((c = ifs.get()) && c != '\n' && !ifs.eof()) line += c;
#else
	getline(ifs,line,'\n');
#endif
	if ((pos = line.find('#')) != StNPOS)		// remove text after '#'
	    line.erase(pos,line.length());
	if ((pos = line.find("//")) != StNPOS)		// remove text after '//'
	    line.erase(pos,line.length());
	if ((pos = line.find(name)) != StNPOS) {	// search for resource name
	    if ((pos = line.find(':')) != StNPOS) {	// search for separator
		line.erase(0,pos+1);	       
		istrstream ist(line.c_str());
		ist >> value;                           // type conversion
		return;                                 // found and assign resource value
	    }
	}
    }
    return;
}


template<class T>
void StGetConfigValue(const char* filename, const char* name, T& value, int nitems)
{
    ifstream ifs(filename);
    if (!ifs) return;
    
    string        line;
    StSizeType    pos;
    while (ifs.good() && !ifs.eof()) {
#if defined(__SUNPRO_CC)
	char c; line.erase();
	while ((c = ifs.get()) && c != '\n' && !ifs.eof()) line += c;
#else
	getline(ifs,line,'\n');
#endif
	if ((pos = line.find('#')) != StNPOS)		// remove text after '#'
	    line.erase(pos,line.length());
	if ((pos = line.find("//")) != StNPOS)		// remove text after '//'
	    line.erase(pos,line.length());
	if ((pos = line.find(name)) != StNPOS) {	// search for resource name
	    if ((pos = line.find(':')) != StNPOS) {	// search for separator
		line.erase(0,pos+1);	       
		istrstream ist(line.c_str());
		for (int i=0; i<nitems; i++)
		    ist >> value[i];                    // type conversion
	    }
	}
    }
}

#endif
