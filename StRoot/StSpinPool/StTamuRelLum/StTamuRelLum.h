//StTamuRelLum.h
//M.L. Miller (MIT)
//10/05

//This file contains necessary classes to read/store/access Ernst Sichterman's (Es) Relative Luminosity file

#ifndef StTamuRelLum_HH
#define StTamuRelLum_HH
#ifdef __ROOT__
#include "Rtypes.h"
#endif

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
	static StTamuRelLum* instance(const string infile = "StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt");

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

	virtual ~StTamuRelLum();

private:
	StTamuRelLum( const string infile ); //We make the constructor private for singleton access
	
	#ifdef __ROOT__
    ClassDef(StTamuRelLum,1)
    #endif
};


inline StTamuRelLum* StTamuRelLum::instance(const string infile)
{
	return (sInstance) ? sInstance : new StTamuRelLum(infile);
}


inline ostream& operator<<(ostream& os, const TamuRelLum& rl)
{
	os <<"run:\t"<<rl.runId<<" fill:\t"<<rl.fill<<" board:\t"<<rl.board<<" bin:\t"<<rl.bbcTimeBin
	<<" uu:\t"<<rl.uu<<" du:\t"<<rl.du<<" ud:\t"<<rl.ud<<" dd:\t"<<rl.dd;
	return os;
}
#endif
