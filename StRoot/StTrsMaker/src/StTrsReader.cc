/***************************************************************************
 *
 * $Id: StTrsReader.cc,v 1.2 1999/01/18 21:02:55 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrsReader.cc,v $
 * Revision 1.2  1999/01/18 21:02:55  lasiuk
 * comment diagnostics
 *
 * Revision 1.1  1998/11/10 17:12:26  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.4  1998/11/08 17:29:23  lasiuk
 * bool built-in data type and allocators
 *
 * Revision 1.3  1998/06/30 22:53:23  lasiuk
 * temp memory management for g2t pointers
 *
 * Revision 1.2  1998/06/04 23:21:32  lasiuk
 * change order of arguments for reading example file
 * **Need to read g2t structure as a pointer**
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StTrsReader.hh"

StTrsReader::StTrsReader() {/* cannot call */ }

StTrsReader::StTrsReader(const string& file)
    : mIfs(file.c_str()) {/* nopt */ }

StTrsReader::~StTrsReader()
{
    //mData.clear();   // mData not declared
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
bool StTrsReader::getData(vector<StTrsChargeSegment>& data)
#else
bool StTrsReader::getData(vector<StTrsChargeSegment, allocator<StTrsChargeSegment> >& data)
#endif
{
// #ifndef ST_NO_TEMPLATE_DEF_ARGS
//     static vector<g2t_tpc_hit*> temporary;
// #else
//     static vector<g2t_tpc_hit*, allocator<g2t_tpc_hit*> > temporary;    
// #endif    
//     for (int i=0; i<temporary.size(); i++) delete temporary[i];
//     temporary.clear();
    
//     //g2t_tpc_hit aG2tSegment;// = new g2t_tpc_hit;

//     StThreeVector<double> position, momentum;
//     int numberOfG2tSegments;

//     cout << "Try Read ..." << endl;
    
//     mIfs >> numberOfG2tSegments;
//     if(mIfs.eof() || mIfs.bad()) {
// 	cout << "return false " << endl;
// 	return false;
//     }

//     cout << "Trying to read " << numberOfG2tSegments << " Segments" << endl;
//     for(int jj=0; jj<numberOfG2tSegments; jj++) {

// 	g2t_tpc_hit* aG2tSegment = new g2t_tpc_hit;
//         temporary.push_back(aG2tSegment);
	
// 	mIfs >> momentum >> position >> aG2tSegment->de >> aG2tSegment->ds;

// 	if(mIfs.fail() || mIfs.eof()) {
// 	    cerr << "Corrupt file stream at line " << jj << " of " << numberOfG2tSegments << endl;
// 	    return false;
// 	}

// 	StTrsChargeSegment aSegment(position, momentum, aG2tSegment);
// 	data.push_back(aSegment);
	
//     }

//     cout << "Read " << data.size() << " segments..." << endl;

    return true;
}
