/***************************************************************************
 *
 * $Id: writeHtml.cc,v 1.1 1999/02/17 12:38:48 ullrich Exp $
 *
 * Author: Thomas Ullrich, August 1998
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: writeHtml.cc,v $
 * Revision 1.1  1999/02/17 12:38:48  ullrich
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

//#define DEBUG
#define PR(x) cout << #x << " = " << (x) << endl;

typedef map<string, string, less<string> > keywordMap_type;
typedef keywordMap_type::value_type keyword_type;

template<class A, class B>
ostream& operator<<(ostream& out, const pair<A,B> &p)
{
    out << p.first << " is defined in " << p.second;
    return out;
}

class FileInfo {
public:
    FileInfo(const string&);
    string&         filename() {return mFilename;}
    string&         htmlFile() {return mHTMLFile;}
    vector<string>& keywords() {return mKeywords;}
    vector<string>& body()     {return mBody;}
    string basename() const;
    
private:
    string         mFilename;
    string         mHTMLFile;
    vector<string> mKeywords;
    vector<string> mBody;
    FileInfo();
};

FileInfo::FileInfo() { /* noop */ }

FileInfo::FileInfo(const string& fname)
{
    size_t pos;
    pos = fname.rfind(".out");
    if (pos == string::npos) {
	throw(invalid_argument("FileInfo::FileInfo(): error file must have extension '.out'"));
    }
    mFilename = fname;
    ifstream ifs(mFilename.c_str());
    if (!ifs) {
	throw(invalid_argument("FileInfo::FileInfo(): cannot open file."));
    }
    string line;
    while (ifs.good()) {
	getline(ifs, line, '\n');
	if (ifs.eof()) break;
	if (line.empty()) continue;
	mBody.push_back(line);
    }
    ifs.close();
    string keyfile = mFilename;
    pos = keyfile.find(".out", pos);
    keyfile.replace(keyfile.find(".out", pos), 4 , ".keywords");
    if (access(keyfile.c_str(), R_OK) == 0) {
	ifs.open(keyfile.c_str());
	if (!ifs) {
	    throw(invalid_argument("FileInfo::FileInfo(): cannot open keywords file."));
	}
	while (ifs.good()) {
	    getline(ifs, line, '\n');
	    if (ifs.eof()) break;
	    if (line.empty()) continue;
	    mKeywords.push_back(line);
	}
	ifs.close();
    }
    mHTMLFile = basename();
    mHTMLFile += ".html";
}

string FileInfo::basename() const
{
    size_t pos;
    string bname = mFilename;
    pos = bname.rfind(".out");
    if (pos != string::npos)
	bname.erase(pos, bname.size()-pos);
    pos = bname.rfind("/");
    if (pos != string::npos)
	bname.erase(0, pos);
    return bname;
}

void writeHtmlHeader(ostream& ofs, const string& name)
{
    string context(name);
    size_t pos = context.rfind(".");
    if (pos != string::npos)
	context.erase(pos, context.size()-pos);

    ofs << "<html>" << endl;
    ofs << "<!-- This code is generated automatically. Dont' modify it! -->" << endl;
    ofs << "<head>" << endl;
    ofs << "<title>StarClassLibrary: " << context << "</title>" << endl;
    ofs << "</head>" << endl;
    ofs << "<body bgcolor=#ffffff text=#000000>" << endl;
    ofs << "<table border=0 width=110% cellpadding=0>" << endl;
    ofs << "<tr bgcolor=darkblue>" << endl;
    ofs << "<td align=left valign=top>" << endl;
    ofs << "<ul>" << endl;
    ofs << "<h4><br></h4>" << endl;  
    ofs << "<h1><font color=white>StarClassLibrary: </font><font color=red>"
	<< context << "</font></h1>" << endl;
    ofs << "</ul>" << endl;  
    ofs << "</td>" << endl;  
    ofs << "<tr>" << endl;  
    ofs << "<td align=left> <font size=-1>" << endl;  
    ofs << "&nbsp; <a href=index.html>Back to Index</a>" << endl; 
    ofs << "</td>" << endl; 
    ofs << "</tr>" << endl; 
    ofs << "</table>" << endl;

    ofs << "<h3>Synopsis</h3>" << endl;
    ofs << "<pre>" << endl;
    ofs << "#include \"" << name << "\"" << endl;    
}

void writeHtmlTrailor(ostream& ofs)
{
    time_t now = time(0);
    ofs << "<hr noshade=noshade>" << endl;
    ofs << "See the <font color=green><i>StarClassLibrary User Guide and Reference Manual</i></font> for more.<br>" << endl;
    ofs << "File generated on " << ctime(&now);
    ofs << "</body>" << endl;
    ofs << "</html>" << endl;
}

void writeLine(ostream& ofs, const string& str, const keywordMap_type &theMap)
{
    //
    //  Translate special characters in 'str'
    //
    string line(str);
    size_t pos;
    while (true) {
	pos = line.find("<",0);
	if (pos == string::npos) break;
	line.replace(pos, 1 , "&lt;");
    }
    while (true) {
	pos = line.find(">",0);
	if (pos == string::npos) break;
	line.replace(pos, 1 , "&gt;");
    }
    pos = line.find(" ;");
    if (pos != string::npos) 
	line.replace(pos, 2 , ";");

    //
    //   Add the hyperlinks
    //
    keywordMap_type::const_iterator iter;
    size_t lpos;
    string html;
    bool   isComplete;
    for (iter = theMap.begin(); iter != theMap.end(); iter++) {
	lpos = 0;
	while (lpos < line.size()) {
	    const string& search = iter->first;
	    pos = line.find(search,lpos);
	    if (pos == string::npos) break;
	    //  keyword candidate found,
	    //  make sure its the full name
	    isComplete = true;
	    lpos = pos+search.size();
	    if (lpos < line.size() && isalnum(line[lpos])) isComplete = false;
            if (pos > 5 && line.substr(pos-5, 5) == string("href=")) isComplete = false;
            if (isComplete) {
		html = "<a href=";
		html += iter->second;
		html += '>';
		html += iter->first;
		html += "</a>";
		line.replace(pos, search.size(), html);
		lpos = pos+html.size();
	    }
	}
    }

    //   Write to stream
    ofs << line << endl;    
}

int main(int argc, char* argv[])
{
    int i, j, k;
    
    if (argc < 2) {
	cerr <<  "Usage:  " << argv[0] << " file ..." << endl;
	return 2;
    }

    //
    //   Collect all the info we got
    //
    vector<FileInfo> files;
    for (i=1; i<argc; i++) {
	FileInfo *info;
	try {
	    info = new FileInfo(string(argv[i]));
	}
	catch (exception &e) {
	    cerr << e.what() << endl;
	    continue;
	}
	files.push_back(*info);
	delete info;
    }
    
#ifdef DEBUG
    PR(files.size());
    for (i=0; i<files.size(); i++) {
	PR(i);
	PR(files[i].basename());
	PR(files[i].filename());
	PR(files[i].htmlFile());
	PR(files[i].keywords().size());
	PR(files[i].body().size());
    }	
#endif

    //
    //   Prepare new list of keywords and filenames
    //
    keywordMap_type keywordMap, emptyMap;
    string hname;
    for (i=0; i<files.size(); i++) {
	hname = files[i].htmlFile();
	for (k=0; k<files[i].keywords().size(); k++)
	    keywordMap.insert(keyword_type(files[i].keywords()[k], hname));
    }
#ifdef DEBUG
    copy(keywordMap.begin(), keywordMap.end(), ostream_iterator<keyword_type>(cout, "\n"));
#endif

    //
    //   Write the HTML file(s)
    //
    ofstream ofs;
    for (i=0; i<files.size(); i++) {
	ofs.open(files[i].htmlFile().c_str());
	if (!ofs) {
	    cerr << argv[0] << ": error cannot open file " << files[i].htmlFile() << endl;
	    continue;
	}
        cout << "writing file " << files[i].htmlFile() << " .";

	writeHtmlHeader(ofs,files[i].basename());
	cout << '.'; cout.flush();
	
	//
	//   Write the HTML body
	//
	vector<string> &theBody = files[i].body();

	//   complete the synopsis section
	string search, line;
	size_t pos;
	for (k=0; k<theBody.size(); k++) {
	    if (k+1 >= theBody.size()) continue;
	    if (theBody[k+1][0] != '{') continue;
	    for (j=0; j<files[i].keywords().size(); j++) {
		search = "class ";
		search += files[i].keywords()[j];
		search += ' ';
		if (theBody[k].find(search) != string::npos) {
		    line = theBody[k];
		    pos = line.find(":");
		    if (pos != string::npos)
			line.erase(pos, line.size()-pos);
		    line += ';';
		    writeLine(ofs, line, emptyMap);
		}
	    }
	}
	ofs << "</pre>" << endl;      // end synopsis section
	cout << '.'; cout.flush();
	
	//   some formatting
	int indentLevel = 0;
	const int indentSpaces = 3;
	for (k=0; k<theBody.size(); k++) {
	    if (theBody[k].find("}") != string::npos) indentLevel--;
	    assert(indentLevel >= 0);
	    if (theBody[k].find("public:") != string::npos && indentLevel > 0) 		
		theBody[k].insert(0, string((indentLevel-1)*indentSpaces, ' '));
	    else 
		theBody[k].insert(0, string(indentLevel*indentSpaces, ' '));
	    if (theBody[k].find("{") != string::npos) indentLevel++;
	}
	cout << '.'; cout.flush();

	//   write the interface section 
	ofs << "<h3>Interface</h3>" << endl;
	ofs << "<pre>" << endl;
	for (k=0; k<theBody.size(); k++) {
	    writeLine(ofs, theBody[k], keywordMap);
	}
	ofs << "</pre>" << endl;
	ofs << "<p>" << endl;
	cout << '.'; cout.flush();
	
	writeHtmlTrailor(ofs);
	ofs.close();	
	cout << ". done" << endl;
    }
    return 0;
}
