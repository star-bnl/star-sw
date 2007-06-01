//StTamuRelLum.h
//M.L. Miller (MIT)
//10/05

//This file contains necessary classes to read/store/access Ernst Sichterman's (Es) Relative Luminosity file

#ifndef StTamuRelLum_HH
#define StTamuRelLum_HH

#include <iostream>
#include <fstream>
#include <string>
#include <map>
using namespace std;

/* note, Ernst's definitions are as follows:

#R1=UpUp+UpDw/DwDw+DwUp R2=UpUp+DwUp/DwDw+UpDw R3=UpUp+DwDw/UpDw+DwUp R4=UpUp/DwDw R5=UpDw/DwDw R6=DwUp/DwDw Board11[R[i],eR[i]]

*/
struct TamuRelLum
{
	//Star RunNumber
	int runId;
	
	//fill
	int fill;
	
	//scaler board number
	int board;
	
	//bbc time bin
	int bbcTimeBin;
	
	//The N-counts as a function of spin-state (YB)
	int uu;
	int du;
	int ud;
	int dd;	
};

//This is needed for internal storage
struct TamuRelLumLessThan
{
	bool operator()(const TamuRelLum& lhs, const TamuRelLum& rhs) const;
};

//Class StTamuRelLum is a singleton class to read/store/access 

class StTamuRelLum
{
public:
	//here's the map typedef
	typedef map<TamuRelLum, TamuRelLum, TamuRelLumLessThan> RelLumMap;

	//here's access to the one global instance
	//NOTE: the input file is assumed to be in the directory where the process is running.
	//To change that, edit StTamuRelLum.cxx and re-compile
	static StTamuRelLum* instance();

	//Print the contents of the map to screen
	void print();

	//Access to the information for a given run:
	//NOTE: test the pointer!  If it is null, that means theres no entry for this run
	const TamuRelLum* getRelLum(int runId, int board, int timeBin);

private:

	//the map:
	RelLumMap mMap;

	//This is the static instance
	static StTamuRelLum* sInstance;

	~StTamuRelLum();

private:
	//StTamuRelLum(	const string infile = "/Volumes/star1.lns.mit.edu/kocolosk/analysis/lum.dat.txt" ); //We make the constructor private for singleton access
	StTamuRelLum(	const string infile = "/Volumes/star1.lns.mit.edu/kocolosk/analysis/finalLum.list" ); //We make the constructor private for singleton access

};


inline StTamuRelLum* StTamuRelLum::instance()
{
	return (sInstance) ? sInstance : new StTamuRelLum();
}


inline ostream& operator<<(ostream& os, const TamuRelLum& rl)
{
	os <<"run:\t"<<rl.runId<<" fill:\t"<<rl.fill<<" board:\t"<<rl.board<<" bin:\t"<<rl.bbcTimeBin
	<<" uu:\t"<<rl.uu<<" du:\t"<<rl.du<<" ud:\t"<<rl.ud<<" dd:\t"<<rl.dd;
	return os;
}
#endif