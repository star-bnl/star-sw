//StTamuRelLum.cxx

#include "StTamuRelLum.h"
#include <assert.h>

#ifdef __ROOT__
ClassImp(StTamuRelLum)
#endif

//have to initialize the static instance to zero
StTamuRelLum* StTamuRelLum::sInstance = 0;

StTamuRelLum::StTamuRelLum(const string infile)
{
	cout <<"creating a new instance of StTamuRelLum"<<endl;

	//store static pointer for later
	sInstance = this;

	cout <<"\n------------------------------- open file:\t"<<infile<<"\tfor reading"<<endl;
	

	ifstream myin(infile.c_str());

	cout <<"\n------------------------------- Read file header"<<endl;
	char name[256];
	myin.getline(name, 256);
	cout <<name<<endl;

	cout <<"\n------------------------------- Begin Read Loop"<<endl;
	//double r3,dr3;
	while (1) {
		TamuRelLum rl;
		myin >> rl.fill >> rl.runId >> rl.board >> rl.bbcTimeBin >> rl.uu >> rl.du >> rl.ud >> rl.dd;
		//	>> r3 >> dr3;
		if (!myin.good()) break;

		//cout <<rl<<endl;

		//add to map
		RelLumMap::iterator where = mMap.find(rl);
		if (where!=mMap.end()) {//oops, it's already in the map
			cout <<"StTamuRelLum::StTamuRelLum() -- Error:\trunId:\t"<<rl.runId<<"\tis already stored in map!"<<endl;
			assert(0);
		}
		else {
			mMap[rl] = rl;
		}

	}

	cout <<"---------------- Read:\t"<<mMap.size()<<"\tentries from file"<<endl;
}

void StTamuRelLum::print()
{
	
	cout <<"\n -------------- void StTamuRelLum::print() -------------------"<<endl;
	cout <<"\n contents of map \n"<<endl;

	for (RelLumMap::iterator it=mMap.begin(); it!=mMap.end(); ++it) {
		const TamuRelLum& rl = (*it).second;
		cout <<rl<<endl;
	}
}

const TamuRelLum* StTamuRelLum::getRelLum(int runId, int board, int timeBin)
{
	TamuRelLum key;
	key.runId = runId;
	key.board = board;
	key.bbcTimeBin = timeBin;
	
	RelLumMap::iterator where = mMap.find(key);
	if (where!=mMap.end()) {
		const TamuRelLum& rl = (*where).second;
		const TamuRelLum* rlp = &rl;
		return rlp;
	}
	else {
		return 0;
	}
}

StTamuRelLum::~StTamuRelLum()
{
	cout <<"killing StTamuRelLum"<<endl;
}

bool TamuRelLumLessThan::operator()(const TamuRelLum& lhs, const TamuRelLum& rhs) const
{
	if (lhs.runId<rhs.runId) {
		return true;
	}
	else if (lhs.runId>rhs.runId) {
		return false;
	}
	else {
		if (lhs.board<rhs.board) {
			return true;
		}
		else if (lhs.board>rhs.board) {
			return false;
		}
		else {
			return lhs.bbcTimeBin<rhs.bbcTimeBin;
		}
	}
}
