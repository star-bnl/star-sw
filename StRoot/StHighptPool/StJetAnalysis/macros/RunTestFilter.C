#include <iostream>
#include <fstream>
#include <string>
#include "TUnixSystem.h"
#include "TROOT.h"

void RunTestFilter()
{
    ifstream in;
    in.open("runs.txt", ios::in);

    string dir;
    string file;
    string infile;

    int i=0;
    while (1) {
	
	if (!in.good() ) break;
	in >> file;
	
	if (i==0) {
	    dir=file;
	    cout <<"dir:\t"<<dir<<endl;
	}
	else {
	    infile = dir+file;
	    cout <<"file:\t"<<infile<<endl;

	    //Launch job:
	    
	    cout <<"concat the unix command"<<endl;
	    char* command = new char[2000];
	    sprintf(command,"root4star -b -q macros/TestFilter.C\'(\"%s\",\"%s\",\"%s\")\'",
		    dir.c_str(), file.c_str(), file.c_str());
	    cout <<"command:\t"<<command<<endl;

	    cout <<"Get TUnixSystem"<<endl;
	    TUnixSystem* sys = static_cast<TUnixSystem*>(gSystem);
	    
	    sys->Exec(command);
	}
	++i;
    }
}
