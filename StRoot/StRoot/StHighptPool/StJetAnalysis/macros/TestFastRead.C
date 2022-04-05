#include <string>

//Note, if you run this compiled, then you're fine.  If not, you have to manually set npos to 1

void TestFastRead()
{
    cout <<"TestFastRead()"<<endl;
    string infile = "Dummy.txt_has_10_events";
    cout <<"infile:\t"<<infile<<endl;
    string begin = "_has_";
    string end = "_events";
    int where1 = infile.find(begin);
    int where2 = infile.find(end);
    //int npos = string::npos; //for compiled code
    int npos = infile.npos; //for compiled code
    //int npos=-1 //for interpreted code
    cout <<"npos:\t"<<npos<<"\twhere1:\t"<<where1<<"\twhere2:\t"<<where2<<endl;

    int start=where1+begin.size();
    int stop=where2;
    cout <<"numbers of events is between indices: ["<<start<<","<<stop<<")"<<endl;

    cout <<"Get number of events"<<endl;
    string number;
    for (int i=start; i<stop; ++i) {
	number += infile[i];
    }

    int nevents = atoi(number.c_str());
    cout <<"Number of events as string:\t"<<number<<endl;
    cout <<"Number of events as int:   \t"<<nevents<<endl;
    
    cout <<"Done"<<endl;
}
