/***************************************************************************
 *
 * $Id: StPrompt.hh,v 1.6 2003/09/02 17:59:35 perev Exp $
 *
 * Author: Thomas Ullrich, Oct 15 1997
 ***************************************************************************
 *
 * Description:
 * Template function to prompt the user for input of type T.
 * Reads the new input value, unless <CR> is 
 * pressed directly after the prompt. In this case the
 * previous value (indicated in brackets) stays untouched.
 *
 * For non-build-in types/classes make sure the input and output 
 * operators '<<' and '>>' are defined.
 *
 * Syntax:
 *
 * StPrompt("text", T);        // T can be: int double, float, string, etc.
 *
 *
 * Specialization:
 *
 * bool answer;
 * StPrompt("text", answer);   // accepts: true, t, yes, y, on, 1
 *                             // as 'true', everything else is assumed
 *                             // to have value 'false'
 *
 * char name[10];
 * StPrompt("text", name, 10); // note the length as last parameter
 *
 ***************************************************************************
 *
 * $Log: StPrompt.hh,v $
 * Revision 1.6  2003/09/02 17:59:35  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2002/05/01 01:09:50  ullrich
 * Changed int to unsigned int to get rid of warning message.
 *
 * Revision 1.4  2000/06/22 01:57:29  ullrich
 * Removed unused variable.
 *
 * Revision 1.3  1999/12/21 20:20:35  ullrich
 * Fixed bug in macro for Sun.
 *
 * Revision 1.2  1999/12/21 15:14:28  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:28:01  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_PROMPT_HH
#define ST_PROMPT_HH

#include <Stiostream.h>
#include <Stsstream.h>
#include <string>
#include <ctype.h>
#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif

inline void StPrompt()
{
    cout << "-- Press return to continue -- ";
    cin.get();
}

template<class T>
inline void StPrompt(const char *text, T& var)
{
    string line;
    char   c;
    
    cout << text << " [" << var << "]: ";
    while ((c = cin.get()) && c != '\n') line += c;
    if (line.length() > 0) {
	istrstream ist(line.c_str(), line.length());
	ist >> var;
    }
}

inline void StPrompt(const char *text, string& var)
{
    string line;
    char   c;
    
    cout << text << " [" << var.c_str() << "]: ";
    while ((c = cin.get()) && c != '\n') line += c;
    if (line.length() > 0) var = line;
}

#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500
inline void StBoolPrompt(const char *text, bool& var)
#else
inline void StPrompt(const char *text, bool& var)
#endif
{
    string line;
    char   c;
    string svar = var ? "true" : "false";

    cout << text << " [" << svar.c_str() << "]: ";
    while ((c = cin.get()) && c != '\n') line += c;
    if (line.length() > 0) {
	if (line == "true")
	    var = true;
	else if (line == "t")
	    var = true;
	else if (line == "yes")
	    var = true;
	else if (line == "y")
	    var = true;
	else if (line == "on")
	    var = true;
	else if (line == "1")
	    var = true;
        else
	    var = false;
    }
}


inline void StPrompt(const char *text, char* var, unsigned int maxlength)
{
    string line;
    unsigned char   c;
    
    cout << text << " [" << var << "]: ";
    while ((c = cin.get()) && c != '\n' && line.length() < maxlength)
	line += c;
    if (line.length() > 0) strcpy(var, line.c_str());
}

#endif
