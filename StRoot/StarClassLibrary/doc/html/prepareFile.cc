/***************************************************************************
 *
 * $Id: prepareFile.cc,v 1.1 1999/02/17 12:38:48 ullrich Exp $
 *
 * Author: Thomas Ullrich, August 1998
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: prepareFile.cc,v $
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
#include <stdexcept>

//#define DEBUG
#define PR(x) cout << #x << " = " << (x) << endl;

struct ClassInfo {
    ClassInfo();
    string name;
    size_t bodyStart;
    size_t bodyEnd;
    bool   correct;
    void   print();
};

ClassInfo::ClassInfo() : correct(false), bodyStart(0), bodyEnd(0) {}

void ClassInfo::print()
{
    cout << "name =      " << name << endl;
    cout << "bodyStart = " << bodyStart << endl;
    cout << "bodyEnd =   " << bodyEnd << endl;
}

size_t ccount(string& s, char c)
{
    size_t n = 0;
    for (int i=0; i<s.size(); i++)
	if (s[i] == c) n++;
    return n;
}

int main(int argc, char* argv[])
{
    long i, j, k;
    
    if (argc < 2) {
	cerr <<  "Usage:  " << argv[0] << " file ..." << endl;
	return 2;
    }

    vector<string> inputFileList;
    for (i=1; i<argc; i++)
	inputFileList.push_back(argv[i]);
    

    for (int iFile=0; iFile<inputFileList.size(); iFile++) {

	string inputfile = inputFileList[iFile];

	if (access(inputfile.c_str(), R_OK) != 0) {
	    cerr <<  argv[0] << ": cannot access file " << inputfile << endl;
	    continue;
	}

	cout << "preparing file " << inputfile << " .";

	string tempfile1(tempnam(".",0)); tempfile1 += ".cc";
	string tempfile2(tempnam(".",0)); tempfile2 += ".cc";
	string command;
#if defined(DEBUG)
	PR(tempfile1);
	PR(tempfile2);
#endif
    
	//
	//  Remove tabs and comment #include statements
	//
	command = "expand ";
	command += inputfile;
	command += "| sed 's/\\#.*include/\\/\\/&/g' > ";
	command += tempfile1;
#if defined(DEBUG)
	cout << command << endl;
#endif
	system(command.c_str());
	cout << '.'; cout.flush();

	//
	//  Run the preprocessor and eliminate #line statements
	//  (they contain the name of the $TMPFILE file)
	//
	command = "g++ -E ";
	command += tempfile1;
	command += " | grep -v ";
	command += tempfile1;
	command += " > ";
	command += tempfile2;
#if defined(DEBUG)
	cout << command << endl;
#endif
	system(command.c_str());
	cout << '.'; cout.flush();
			
	//
	//  Now in C++,
	//  read the whole file in one string
	//  and indent everything in a way that
	//  further processing is easy.
	//
	long     len, pos;
	string   line;
	string   bigLine;
	char     c;
	ifstream ifs(tempfile2.c_str());
	while (ifs.good()) {
	    getline(ifs, line, '\n');
	    if (ifs.eof()) break;
	    if (line.empty()) continue;		// skip empty lines
	    line += ' ';
	    for (i=0; i<line.size(); i++) {
		c = line[i];
                switch(c) {
		case ';':
		    bigLine += ";\n";
		    break;
		case '{':
		    bigLine += "\n{\n";
		    break;
		case '}':
		    if (i+1 < line.size() && line[i+1] == ';') {
			bigLine += "\n};\n";
			i++;
		    }
		    else
			bigLine += "\n}\n";
		    break;
		case '\t':
		    break;
		default:
		    bigLine += c;
		    break;
		}
	    }
	}
	ifs.close();
	cout << '.'; cout.flush();
	pos = 0;
	while(pos < bigLine.size()) {
	    pos = bigLine.find("public:", pos);
	    if (pos != string::npos) {
		bigLine.replace(pos, 7 , "public:\n");
		pos += 7;
	    }
	    else
		break;
	}
	pos = 0;
	while(pos < bigLine.size()) {
	    pos = bigLine.find("private:", pos);
	    if (pos != string::npos) {
		bigLine.replace(pos, 8 , "private:\n");
		pos += 8;
	    }
	    else
		break;
	}
	pos = 0;
	while(pos < bigLine.size()) {
	    pos = bigLine.find("protected:", pos);
	    if (pos != string::npos) {
		bigLine.replace(pos, 10 , "protected:\n");
		pos += 10;
	    }
	    else
		break;
	}
	cout << '.'; cout.flush();

	//
	//  Remove inline keyword
	//
	pos = 0;
	while(pos < bigLine.size()) {
	    pos = bigLine.find("inline ", pos);
	    if (pos != string::npos) {
		bigLine.erase(pos, 7);
	    }
	    else
		break;
	}
	cout << '.'; cout.flush();

	//
	//  Fix some bad coding styles
	//
	pos = 0;
	while(pos < bigLine.size()) {
	    pos = bigLine.find(":", pos);
	    if (pos != string::npos) {
		bigLine.replace(pos, 1, " : ");
		pos += 3;
	    }
	    else
		break;
	}
	cout << '.'; cout.flush();
	
	//
	//  Remove double blanks
	//
	pos = 0;
	bool reset = false;
	while(true) {
	    pos = bigLine.find("  ", pos);
	    if (pos != string::npos) {
		bigLine.erase(pos, 1);
		reset = false;
	    }
	    else {
		if (reset) break;
	    }
	    if (pos >= bigLine.size()) {
		reset = true;
		pos = 0;
	    }
	}
	cout << '.'; cout.flush();

	//
	//  Merge into line vector
	//
	vector<string> allLines;
	line.erase();
	bool beginLine = true;
	for (i=0; i<bigLine.size(); i++) {
	    c = bigLine[i];
	    switch(c) {
	    case ' ':
		if (!beginLine) 
		    line += c;
		break;
	    case '\n':
		if (!line.empty()) {
		    line += c;
		    allLines.push_back(line);
		}
		line.erase();
		beginLine = true;
		break;		
	    default:
		line += c;
		beginLine = false;
		break;
	    }	    
	}
	bigLine.erase();
	cout << '.'; cout.flush();

#if defined(DEBUG)
 	copy(allLines.begin(), allLines.end(), ostream_iterator<string>(cout));
#endif
	

	//
	//   Some cosmetics
	//
	for (i=0; i<allLines.size(); i++) {
	    line = allLines[i];
	    pos = line.find("operator ",0);
	    if (pos != string::npos) line.replace(pos, 9 , "operator"); 
	    pos = line.find("< class",0);
	    if (pos != string::npos) line.replace(pos, 7 , "<class"); 
	    pos = line.find(" < ",0);
	    if (pos != string::npos) line.replace(pos, 3 , "<"); 
	    pos = line.find(" > ",0);
	    if (pos != string::npos) line.replace(pos, 3 , "> "); 
	    pos = line.find(" >",0);
	    if (pos != string::npos) line.replace(pos, 2 , ">"); 
	    pos = line.find(" & ",0);
	    if (pos != string::npos) line.replace(pos, 3 , "& "); 
	    pos = line.find("template <",0);
	    if (pos != string::npos) line.replace(pos, 10 , "template<");
	    pos = line.find("private :",0);
	    if (pos != string::npos) line.replace(pos, 9 , "private:");
	    pos = line.find("protected :",0);
	    if (pos != string::npos) line.replace(pos, 11 , "protected:");
	    pos = line.find("public :",0);
	    if (pos != string::npos) line.replace(pos, 8 , "public:");
	    pos = line.find(": :",0);
	    if (pos != string::npos) line.replace(pos, 3 , "::");
	    pos = line.find(" :: ",0);
	    if (pos != string::npos) line.replace(pos, 4 , "::");
 	    allLines[i] = line;
	}
	cout << '.'; cout.flush();

	//
	//  Remove private/protected sections 
	//
	vector<string>::iterator viter;
	vector<string> theText;
	bool inPrivateRegion = false;
	bool inProtectedRegion = false;
	for (viter = allLines.begin(); viter != allLines.end(); viter++) {
	    if (inPrivateRegion) {
		if (viter->find("protected:") != string::npos ||
		    viter->find("public:") != string::npos ||
		    *viter == string("};\n"))
		    inPrivateRegion = false;
	    }
	    else {
		if (viter->find("private:") != string::npos)
		    inPrivateRegion = true;
	    }
	    if (inProtectedRegion) {
		if (viter->find("private:") != string::npos ||
		    viter->find("public:") != string::npos ||
		    *viter == string("};\n"))
		    inProtectedRegion = false;
	    }
	    else {
		if (viter->find("protected:") != string::npos)
		    inProtectedRegion = true;
	    }
	    
	    if (!inPrivateRegion && !inProtectedRegion &&
		*viter != string("\n") && !viter->empty())
		theText.push_back(*viter);	    
	}	
	allLines.clear();
	cout << '.'; cout.flush();
	
	//
	//  Get class name(s) (if any). Includes enums.
	//	
	vector<ClassInfo> classInfo;
	size_t pos1, pos2;
	for (k=0; k<theText.size(); k++) {
	    if ((pos=theText[k].rfind("class ")) != string::npos &&
		theText[k].rfind(";") == string::npos ) {      // no forward declerations
		if (pos-1 >=  0) {
		    if (theText[k][pos-1] == '<' ||
			theText[k][pos-1] == ',') continue;
		}
		if (pos-2 >=  0) {
		    if (theText[k][pos-2] == '<' ||
			theText[k][pos-2] == ',') continue;
		}
		ClassInfo info;
		pos1 = theText[k].find_first_not_of(' ', pos+5);
		pos2 = theText[k].find_first_of(' ', pos1)-1;
		info.name = theText[k].substr(pos1, pos2-pos1+1);
		info.bodyStart = k+1;   // '{' should be in next line
		classInfo.push_back(info);
	    }
	    if ((pos=theText[k].find("enum ")) != string::npos) {
		    ClassInfo info;
		    pos1 = theText[k].find_first_not_of(' ', pos+4);
		    pos2 = theText[k].find_first_of(' ', pos1)-1;
		    info.name = theText[k].substr(pos1, pos2-pos1+1);
		    info.bodyStart = k+1;   // '{' should be in next line
		    classInfo.push_back(info);
	    }
	    if ((pos=theText[k].find("struct ")) != string::npos) {
		    ClassInfo info;
		    pos1 = theText[k].find_first_not_of(' ', pos+4);
		    pos2 = theText[k].find_first_of(' ', pos1)-1;
		    info.name = theText[k].substr(pos1, pos2-pos1+1);
		    info.bodyStart = k+1;   // '{' should be in next line
		    classInfo.push_back(info);
	    }
	}
	cout << '.'; cout.flush();
	
	//
	//  Find position where class body ends '};'
	//  Caution here, classes could be nested
	//
	vector<size_t> tmp;
	for (k = 0; k<theText.size(); k++) {
	    if (theText[k].find("};") != string::npos) {
		tmp.push_back(k);
	    }
	}
#if defined(DEBUG)
 	PR(tmp.size());
 	PR(classInfo.size());
#endif
	if (tmp.size() != classInfo.size()) {
	    cout << " . error" << endl;
	    cout << argv[0] << ": tmp.size() != classInfo.size()" << endl;
	    remove(tempfile1.c_str());
	    remove(tempfile2.c_str());	    
	    theText.clear();
	    continue;	    
	}
	
	// Which is which -> look for closest distance first;
	int dist, minDist;
	pair<size_t, size_t> minPair;
	int nMatches = 0;
	
	while (nMatches != classInfo.size()) {
	    minDist = theText.size();
	    for (k=0; k<tmp.size(); k++) {
		if (tmp[k] == 0) continue;                  // is flagged as used
		for (i=0; i<classInfo.size(); i++) {
		    if (classInfo[i].correct) continue;     // has already a partner
		    dist = tmp[k]-classInfo[i].bodyStart;
		    if (dist < 0) continue;
		    if (dist < minDist) {
			minDist = dist;
			minPair.first = k;     // position of end-of-body
			minPair.second = i;    // index of class
		    }
		}
	    }
	    classInfo[minPair.second].bodyEnd = tmp[minPair.first];
	    classInfo[minPair.second].correct = true;       // flag as correct
	    tmp[minPair.first] = 0;                         // flag used
	    nMatches++;
	}
	cout << '.'; cout.flush();
#if defined(DEBUG)
	for (i=0; i<classInfo.size(); i++) {
	    classInfo[i].print();
	}
#endif

	//
	//  Next is to remove all bodies, except the class bodies
	//  and the namespaces.
	//  Add a semicolon instead: definition -> declaration
	//
	vector<bool> markForDelete(theText.size(), false);
	bool isStart;
	size_t leftBrackets;
	size_t rightBrackets;
	for (k=0; k<theText.size(); k++) {
	    if (theText[k][0] == '{') {
		isStart = true;
		for (i=0; i<classInfo.size(); i++) {
		    if (classInfo[i].bodyStart == k) {
			isStart = false;
			break;
		    }
		}
		if (k-1 >= 0 &&
		    theText[k-1].find("namespace") != string::npos &&
		    theText[k-1].find("using") == string::npos) {
		    isStart = false;
		    break;
		}		    		    
		if (isStart) {
		    leftBrackets = rightBrackets = 0;
		    theText[k-1].insert(theText[k-1].size()-1, ";");
		    for (j=k; j<theText.size(); j++) {
			markForDelete[j] = true;
			// One problem here: they might be more {,} in the bodies
			// therefore ...
			leftBrackets += ccount(theText[j], '{');
			rightBrackets += ccount(theText[j], '}');
			if (j > k && leftBrackets == rightBrackets) {
			    isStart == false;
			    break;
			}
		    }
		}
	    }
	}
#if defined(DEBUG)
	for (k=0; k<theText.size(); k++) {
	    if (markForDelete[k]) cout << "DELETE>>>";
	    cout << theText[k];
	}
#endif	
	for (k=0; k<theText.size(); k++)
	    if (markForDelete[k]) theText[k].erase();
	cout << '.'; cout.flush();
	
	//
	//  Now get rid of all definitions, only declarations are needed.
	//  Do not remove them in case they are inside a class body. This
	//  could mean different things.
	//  Remove also the data member constructors. Some cleanup ...
	//
	bool isClassDeclaration;
	bool isInsideClassBody;
	for (k=0; k<theText.size(); k++) {
	    if (theText[k].find("::") != string::npos) {
		isInsideClassBody = false;
		for (i=0; i<classInfo.size(); i++) {
		    if (k > classInfo[i].bodyStart && k < classInfo[i].bodyEnd)
			isInsideClassBody = true;
		}		
		if (!isInsideClassBody) theText[k].erase();
	    }
	    if (theText[k].find("extern \"C\"") != string::npos) theText[k].erase();
	    pos = theText[k].find(" ;",0);
	    if (pos != string::npos) theText[k].replace(pos, 2 , ";");
	    isClassDeclaration = false;
	    for (i=0; i<classInfo.size(); i++) {
		if (classInfo[i].bodyStart-1 == k)
		    isClassDeclaration = true;
	    }
	    if (!isClassDeclaration) {
		pos = theText[k].find(" : ",0);
		if (pos != string::npos) {
		    theText[k].erase(pos, theText[k].size()-pos);
		    line += '\n';
		}
	    }
	}
	cout << '.'; cout.flush();
		
	//
	//  Get basename of input file
	//
	pos = inputfile.rfind('/');
	if (pos != string::npos)
	    inputfile.erase(0, pos+1);
	
	string keywordFile(inputfile); keywordFile += ".keywords";
	string outFile(inputfile); outFile += ".out";
	
	//
	//  Write out the results
	//
	ofstream keyfs(keywordFile.c_str());
	ofstream outfs(outFile.c_str());
	for (k=0; k<theText.size(); k++) {
	if (!theText[k].empty() && theText[k] != "\n")
	    outfs << theText[k];
	}
	for (i=0; i<classInfo.size(); i++) keyfs << classInfo[i].name << endl;
	cout << '.'; cout.flush();
        
	//
	//  Remove temporary files
	//
	remove(tempfile1.c_str());
	remove(tempfile2.c_str());

	theText.clear();
#if defined(DEBUG)
	copy(theText.begin(), theText.end(), ostream_iterator<string>(cout, "\n"));
#endif
	cout << ". done" << endl;
       }
    return 0;
}
