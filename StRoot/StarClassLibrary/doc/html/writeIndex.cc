/***************************************************************************
 *
 * $Id: writeIndex.cc,v 1.1 1999/02/17 12:38:49 ullrich Exp $
 *
 * Author: Thomas Ullrich, August 1998
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: writeIndex.cc,v $
 * Revision 1.1  1999/02/17 12:38:49  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <string>
#include <fstream.h>
#include <cstdlib>
#include <unistd.h>
#include <stdio.h>
#include <utility>
#include <list>
#include <vector>
#include <map>
#include <stdexcept>
#include <time.h>
#include <assert.h>
#include <algorithm>

// #define DEBUG
#define PR(x) cout << #x << " = " << (x) << endl;

typedef map<string, string, less<string> > keywordMap_type;
typedef keywordMap_type::value_type keyword_type;

template<class A, class B>
ostream& operator<<(ostream& out, const pair<A,B> &p)
{
    out << p.first << " is defined in " << p.second;
    return out;
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
	cerr <<  "Usage:  " << argv[0] << " file ..." << endl;
	return 2;
    }
    
    //
    //   Prepare new list of keywords and filenames
    //   If there are no keywords available we use the
    //   name of the header file directly (without extensions)
    //
    keywordMap_type keywordMap;
    string          htmlFile, keywordFile, baseName, line;
    size_t          pos, nkeys;
    ifstream        ifs;
    int             i, j, k;
    for (i=1; i<argc; i++) {
	htmlFile = keywordFile = baseName = argv[i];
	pos = htmlFile.find(".html");
	if (pos == string::npos) {
	    cerr << argv[0] << ": error, file must have extension '.html': "
		 << htmlFile << endl;
	    continue;
	}
	keywordFile.replace(pos, 5, ".keywords");
	baseName.erase(pos, 5);
	pos = baseName.rfind(".");
	if (pos != string::npos) baseName.erase(pos, line.size()-pos);
	ifs.open(keywordFile.c_str());
	if (!ifs) {
	    keywordMap.insert(keyword_type(baseName, htmlFile));
	}
	else {
	    nkeys = 0;
	    while (ifs.good()) {
		getline(ifs, line, '\n');
		if (ifs.eof()) break;
		if (line.empty()) continue;
		nkeys++;
		keywordMap.insert(keyword_type(line, htmlFile));
	    }
	    if (nkeys == 0) keywordMap.insert(keyword_type(baseName, htmlFile));
	}
	ifs.close();
    }
#ifdef DEBUG
    copy(keywordMap.begin(), keywordMap.end(), ostream_iterator<keyword_type>(cout, "\n"));
#endif
    //
    //   Write the index file
    //
    const string indexName = "index.html";
    ofstream ofs(indexName.c_str());
    if (!ofs) {
	cerr << argv[0] << ": error, cannot create file " << indexName << endl;
	return 1;
    }
    cout << "writing index to " << indexName << " .";
    ofs << "<html>" << endl;
    ofs << "<!-- This code is generated automatically. Dont' modify it! -->" << endl;
    ofs << "<head>" << endl;
    ofs << "<title>StarClassLibrary: Class Browser</title>" << endl;
    ofs << "</head>" << endl;
    ofs << "<body bgcolor=#ffffff text=#000000>" << endl;
    ofs << "<table border=0 width=110% cellpadding=0>" << endl;
    ofs << "<tr bgcolor=darkblue>" << endl;
    ofs << "<td align=left valign=top>" << endl;
    ofs << "<ul>" << endl;
    ofs << "<h4><br></h4>" << endl;  
    ofs << "<h1><font color=white>StarClassLibrary: </font><font color=red>"
	<< "Class Browser Index</font></h1>" << endl;
    ofs << "</ul>" << endl;  
    ofs << "</td>" << endl;  
    ofs << "</tr>" << endl; 
    ofs << "</table>" << endl;
    ofs << "<p>" << endl; 
    ofs << "<table border=0 width=100% cellpadding=15>" << endl;
    ofs << "<tr>" << endl;
    ofs << "<td bgcolor=#eeeeee align=left valign=top>" << endl;
    ofs << "<ul>" << endl;
    keywordMap_type::const_iterator iter;
    for (iter = keywordMap.begin(); iter != keywordMap.end(); iter++) {
	cout << '.'; cout.flush();
	ofs << "<li><a href=" << iter->second << ">"
	    << iter->first << "</a></li>" << endl;
    }    
    ofs << "</ul>" << endl;
    ofs << "</td>" << endl;  
    ofs << "<td align=left valign=top>" << endl;
    ofs << "<p>This reference guide is an alphabetical
listing of all of the classes, algorithms, and function
objects provided by <i>this</i> release of the Star C++
Class Library. For each class, the reference begins with
a synopsis, which indicates the header file(s) and the
signature of a class object. It continues with
the C++ code that describes the <i>public</i> interface.
Please note, that the interface is displayed in its pure
ANSI form. In a few rare cases and depending on your platform
the actual code might differ slightly. </p>
<p>This guide only shows the interfaces, i.e., the prototypes
of the classes and function objects. For a more detailed description see the
<font color=green><i>Star C++ Class Library User Guide and
Reference Manual </i></font> which is available as a
PostScript document. It contains information concerning the
various platforms, installation procedures and a complete
description of all classes including many example programs.
Developers should also inspect the corrseponding header files.</p>
This index and all other HTML pages referenced were
generated automatically." << endl; 
    ofs << "</td>" << endl;  
    ofs << "</tr>" << endl; 
    ofs << "</table>" << endl;
    time_t now = time(0);
    ofs << "<hr noshade=noshade>" << endl;
    ofs << "Index generated on " << ctime(&now);
    ofs << "</body>" << endl;
    ofs << "</html>" << endl;

    ofs.close();
    cout << ". done" << endl;
    return 0;
}
