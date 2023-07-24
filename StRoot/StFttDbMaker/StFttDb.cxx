/***************************************************************************
 * StFttDb.cxx
 * jdb Feb, 2022
 ***************************************************************************
 * Description: This interface between FTT and the STAR database
 ***************************************************************************/

#include "StFttDb.h"
#include "StMaker.h"
#include "StMessMgr.h"
#include "StEvent/StFttRawHit.h"
#include "StEvent/StFttCluster.h"
#include "StEvent/StFttPoint.h"
#include <math.h>

#include "tables/St_fttHardwareMap_Table.h"
#include "tables/St_fttDataWindows_Table.h"


ClassImp(StFttDb)


double StFttDb::stripPitch = 3.2; // mm
double StFttDb::rowLength = 180; // mm
double StFttDb::lowerQuadOffsetX = 101.6; // mm
double StFttDb::idealPlaneZLocations[] = { 281.082,304.062,325.058,348.068 };
double StFttDb::HVStripShift = 15.95;//mm
double StFttDb::DiagStripShift = 19.42;//mm
vector<string> StFttDb::orientationLabels = { "Horizontal", "Vertical", "DiagonalH", "DiagonalV", "Unknown" };


StFttDb::StFttDb(const char *name) : TDataSet(name) {}; 

StFttDb::~StFttDb() {}

int StFttDb::Init(){

  return kStOK;
}

void StFttDb::setDbAccess(int v) {mDbAccess =  v;}
void StFttDb::setRun(int run) {mRun = run;}

int StFttDb::InitRun(int runNumber) {
    mRun=runNumber;
    return kStOK;
}


size_t StFttDb::uuid( StFttRawHit * h, bool includeStrip ) {
    // this UUID is not really universally unique
    // it is unique up to the hardware location 
    // at the give precision
    // NOT including strip level is useful for cluster 
    // calculations that combine all strips from given 
    // plane, quad, row, orientation

    
    size_t _uuid = (size_t)h->orientation() + (nStripOrientations) * ( h->row() + nRowsPerQuad * ( h->quadrant() + nQuadPerPlane * h->plane() ) );
    
    if ( includeStrip ){
        _uuid = (size_t) h->strip() * maxStripPerRow *( h->orientation() + (nStripOrientations) * ( h->row() + nRowsPerQuad * ( h->quadrant() + nQuadPerPlane * h->plane() ) ) );
    } 

    return _uuid;
}

size_t StFttDb::uuid( StFttCluster * c ) {
    // this UUID is not really universally unique
    // it is unique up to the hardware location 

    size_t _uuid = (size_t)c->orientation() + (nStripOrientations) * ( c->row() + nRowsPerQuad * ( c->quadrant() + nQuadPerPlane * c->plane() ) );
    return _uuid;
}


uint16_t StFttDb::packKey( int feb, int vmm, int ch ) const{
    // feb = [1 - 6] = 3 bits
    // vmm = [1 - 4] = 3 bits
    // ch  = [1 - 64] = 7 bits
    return feb + (vmm << 3) + (ch << 6);
}
void StFttDb::unpackKey( int key, int &feb, int &vmm, int &ch ) const{
    feb = key & 0b111;
    vmm = (key >> 3) & 0b111;
    ch  = (key >> 6) & 0b1111111;
    return;
}
uint16_t StFttDb::packVal( int row, int strip ) const{
    // row = [1 - 4] = 3 bits
    // strip = [1 - 152] = 8 bits
    return row + ( strip << 3 );
}
void StFttDb::unpackVal( int val, int &row, int &strip ) const{
    row = val & 0b111; // 3 bits
    strip = (val >> 3) & 0b11111111; // 8 bit
    return;
}

void StFttDb::loadDataWindowsFromDb( St_fttDataWindows * dataset ) {
    if (dataset) {
        Int_t rows = dataset->GetNRows();

        if ( !rows ) return;

        dwMap.clear();

        fttDataWindows_st *table = dataset->GetTable();
        for (Int_t i = 0; i < rows; i++) {
            for ( int j = 0; j < StFttDb::nVMM; j++ ) {
                // printf( "[feb=%d, vmm=%d, ch=%d] ==> [row=%d, strip%d]\n", table[i].feb[j], table[i].vmm[j], table[i].vmm_ch[j], table[i].row[j], table[i].strip[j] );


                // uint16_t key = packKey( table[i].feb[j], table[i].vmm[j], table[i].vmm_ch[j] );
                // uint16_t val = packVal( table[i].row[j], table[i].strip[j] );
                // mMap[ key ] = val;
                // rMap[ val ] = key;
                FttDataWindow fdw;
                fdw.uuid   = table[i].uuid[j];
                fdw.mode   = table[i].mode[j];
                fdw.min    = table[i].min[j];
                fdw.max    = table[i].max[j];
                fdw.anchor = table[i].anchor[j];
                dwMap[ fdw.uuid ] = fdw;


                // std::cout << (int)table[i].feb[j] << std::endl;
            }
            // sample output of first member variable
        }
    } else {
        std::cout << "ERROR: dataset does not contain requested table" << std::endl;
    }
}

void StFttDb::loadDataWindowsFromFile( std::string fn ) {


}


void StFttDb::loadHardwareMapFromDb( St_fttHardwareMap * dataset ) {
    if (dataset) {
        Int_t rows = dataset->GetNRows();
        if (rows > 1) {
            std::cout << "INFO: found INDEXED table with " << rows << " rows" << std::endl;
        }

        mMap.clear();
        rMap.clear();

        fttHardwareMap_st *table = dataset->GetTable();
        for (Int_t i = 0; i < rows; i++) {
            for ( int j = 0; j < 1250; j++ ) {
                uint16_t key = packKey( table[i].feb[j], table[i].vmm[j], table[i].vmm_ch[j] );
                uint16_t val = packVal( table[i].row[j], table[i].strip[j] );
                mMap[ key ] = val;
                rMap[ val ] = key;
            }
            // sample output of first member variable
        }
    } else {
        std::cout << "ERROR: dataset does not contain requested table" << std::endl;
    }
}


void StFttDb::loadHardwareMapFromFile( std::string fn ){
    std::ifstream inf;
    inf.open( fn.c_str() );

    mMap.clear();
    if ( !inf ) {
        LOG_WARN << "sTGC Hardware map file not found" << endm;
        return;
    }

    mMap.clear();
    string hs0, hs1, hs2, hs3, hs4;
    // HEADER:
    // Row_num    FEB_num    VMM_num    VMM_ch         strip_ch
    inf >> hs0 >> hs1 >> hs2 >> hs3 >> hs4;
    
    if ( mDebug ){
        printf( "Map Header: %s, %s, %s, %s, %s", hs0.c_str(), hs1.c_str(), hs2.c_str(), hs3.c_str(), hs4.c_str() );
        puts("");
    }
    
    uint16_t row, feb, vmm, ch, strip;
    while( inf >> row >> feb >> vmm >> ch >> strip ){
        // pack the key (feb, vmm, ch)
        uint16_t key = packKey( feb, vmm, ch );
        uint16_t val = packVal( row, strip );
        mMap[ key ] = val;
        rMap[ val ] = key;
        if ( mDebug ){
            printf( "key=%d", key );
            printf( "in=(feb=%d, vmm=%d, ch=%d)\n", feb, vmm, ch );
            int ufeb, uvmm, uch;
            unpackKey( key, ufeb, uvmm, uch );
            printf( "key=(feb=%d, vmm=%d, ch=%d)\n", ufeb, uvmm, uch ); puts("");
            assert( feb == ufeb && vmm == uvmm && ch == uch );
            int urow, ustrip;
            printf( "val=%d", val );
            unpackVal( val, urow, ustrip );
            assert( row == urow && strip == ustrip );
            printf( "(row=%d, strip=%d)\n", row, strip );
        }
    }
    inf.close();
}

//read the strip center file to get the position information
//for the local coordinate, zero point will be the pin hole, that may changed depends on survey result
bool StFttDb::loadStripCenterFromFile( std::string fn ){
    std::ifstream inf;
    inf.open( fn.c_str() );
    if ( !inf ) {
        LOG_WARN << "sTGC Hardware map file not found" << endm;
        return kFALSE;
    }
    std::string st1 = "Row1";
    std::string st2 = "Row4";

    //check the input file, input file should be Row1 or Row4
    //Row1 for H&V; Row4 for Diagonal
    size_t idx1 = fn.find(st1);
    size_t idx2 = fn.find(st2);
    if( idx1 == string::npos && idx2 == string::npos )
    {
        cout<< "Wrong Input Strip Center File !!!!!!!!!!!" << endl;
        return kFALSE;
    }

    if ( idx1 == string::npos ) // load the Row4(Diagonal)
    {
        scMapDiag.clear();

        //File Header
        std::string nStrip;
        std::string StripCenter;
        inf >> nStrip >> StripCenter;
        if (mDebug)
        {
            printf( "File Header: %s, %s", nStrip.c_str(), StripCenter.c_str());
        }

        //read strip Center info
        int n_strip; Float_t pos_strip_center;
        while (inf >> nStrip >> StripCenter)
        {
            n_strip = atoi(nStrip.c_str());
            pos_strip_center = atof(StripCenter.c_str());

            scMapDiag[n_strip] = pos_strip_center;
        }

    }

    if ( idx2 == string::npos ) // load the Row1(H&V)
    {
        scMapXY.clear();

        //File Header
        std::string nStrip, StripCenter;
        inf >> nStrip >> StripCenter;
        if (mDebug)
        {
            printf( "File Header: %s, %s", nStrip.c_str(), StripCenter.c_str());
        }

        //read strip Center info
        int n_strip; Float_t pos_strip_center;
        while (inf >> nStrip >> StripCenter)
        {
            n_strip = atoi(nStrip.c_str());
            pos_strip_center = atof(StripCenter.c_str());

            scMapXY[n_strip] = pos_strip_center;
        }

    }

    inf.close();
    return kTRUE;
}

//read the strip edge file, it will be used for reject ghost hits
//for the local coordinate, zero point will be the pin hole, that may changed depends on survey result
bool StFttDb::loadStripEdgeFromFile( std::string fn ){
    std::ifstream inf;
    inf.open( fn.c_str() );
    if ( !inf ) {
        LOG_WARN << "sTGC Hardware map file not found" << endm;
        return kFALSE;
    }
    std::string st1 = "Row4_edge";

    //check the input file, input file should include Row4_edge
    //Row4 for Diagonal
    size_t idx1 = fn.find(st1);
    if( idx1 == string::npos)
    {
        LOG_ERROR << "Wrong Input Strip Edge File !!!!!!!!!!!" << endm;
        return kFALSE;
    }

    if ( idx1 != string::npos ) // load the Row4(Diagonal)
    {
        seMapDiagLeft.clear();
        seMapDiagRight.clear();

        //File Header
        std::string nStrip, StripEdge_L, StripEdge_R;
        inf >> nStrip >> StripEdge_L >> StripEdge_R;
        if (mDebug)
        {
            printf( "File Header: %s, %s, %s", nStrip.c_str(), StripEdge_L.c_str(), StripEdge_R.c_str());
        }

        //read strip Center info
        int n_strip; Float_t pos_strip_edge_L, pos_strip_edge_R;
        while (inf >> nStrip >> StripEdge_L >> StripEdge_R)
        {
            n_strip = atoi(nStrip.c_str());
            pos_strip_edge_L = atof(StripEdge_L.c_str());
            pos_strip_edge_R = atof(StripEdge_R.c_str());

            seMapDiagLeft[n_strip] = pos_strip_edge_L;
            seMapDiagRight[n_strip] = pos_strip_edge_R;
        }

    }

    inf.close();
    LOG_INFO << "sTGC Strip Edges loaded from File: " << fn << endm;
    return kTRUE;
}

// same for all planes
// we have quadrants like:
// 
// D | A
// ------
// C | B
// Row 3 and 4 are always diagonal
// odd (even) FOB are horizontal (vertical) for A and C (B and D)
// even (odd) FOB are vertical (horizontal) for A and C (B and D)
UChar_t StFttDb::getOrientation( int rob, int feb, int vmm, int row ) const {
    if ( mDebug ) {
        printf( "getOrientation( %d, %d, %d, %d )", rob, feb, vmm, row ); puts("");
    }

    if ( rob % 2 == 0 ){ // even rob
        if ( feb % 2 == 0 ) { // odd
            // row 3 and 4 are always diagonal
            if ( 3 == row || 4 == row )
                return kFttDiagonalH;    
            return kFttHorizontal;
        }
        // row 3 and 4 are always diagonal
        if ( 3 == row || 4 == row )
            return kFttDiagonalV;
        // even
        return kFttVertical;
    } else { // odd rob
        
        if ( feb % 2 == 0 ) { // odd
            // row 3 and 4 are always diagonal
            if ( 3 == row || 4 == row )
                return kFttDiagonalV;
            return kFttVertical;
        }
        // row 3 and 4 are always diagonal
        if ( 3 == row || 4 == row )
            return kFttDiagonalH;
        // even
        return kFttHorizontal;
    }
    // should never get here!
    return kFttUnknownOrientation;
}

/* get 
 * returns the mapping for a given input
 * 
 * input:
 *      rob: 1 - 16
 *      feb: 1 - 6
 *      vmm: 1 - 4
 *      ch : 0 - 63
 *
 * output:
 *      row: 0 - 4
 *      strip: 0 - 162
 *      orientation: one of {Horizontal, Vertical, Diagonal, Unknown}
 *
 */
bool StFttDb::hardwareMap( int rob, int feb, int vmm, int ch, int &row, int &strip, UChar_t &orientation ) const{
    uint16_t key = packKey( feb, vmm, ch );
    if ( mMap.count( key ) ){
        uint16_t val = mMap.at( key );
        unpackVal( val, row, strip );
        orientation = getOrientation( rob, feb, vmm, row );
        return true;
    }
    return false;
}

bool StFttDb::hardwareMap( StFttRawHit * hit ) const{
    uint16_t key = packKey( hit->feb()+1, hit->vmm()+1, hit->channel() );
    if ( mMap.count( key ) ){
        uint16_t val = mMap.at( key );
        int row=-1, strip=-1;
        unpackVal( val, row, strip );
        
        u_char iPlane = hit->sector() - 1;
        u_char iQuad = hit->rdo() - 1;
        int rob = iQuad + ( iPlane *nQuadPerPlane ) + 1;

        UChar_t orientation = getOrientation( rob, hit->feb()+1, hit->vmm()+1, row );
        hit->setMapping( iPlane, iQuad, row, strip, orientation );

        // set strip info
        Float_t stripCenter = -1;
        Float_t stripLeftEdge = -1;
        Float_t stripRightEdge = -1;
        if (orientation == kFttHorizontal || orientation == kFttVertical){
            if ( scMapXY.count( strip ) > 0 )
                stripCenter = scMapXY.at(strip);
            else {
                LOG_ERROR << "Cannot find StripCenter for " << strip << endm;
            }
        }
        if (orientation == kFttDiagonalH || orientation == kFttDiagonalV) {
            if ( scMapDiag.count(strip) > 0 )
                stripCenter    = scMapDiag.at(strip);
            else {
                LOG_ERROR << "Cannot find StripCenter for Diag " << strip << endm;
            }
            if (seMapDiagLeft.count(strip) > 0)
                stripLeftEdge  = seMapDiagLeft.at(strip);
            else {
                LOG_ERROR << "Cannot find StripLeftEdge for Diag " << strip << endm;
            }
            if (seMapDiagRight.count(strip) > 0)
                stripRightEdge = seMapDiagRight.at(strip);
            else {
                LOG_ERROR << "Cannot find StripRightEdge for " << strip << endm;
            }
        }
        hit->setStripEdges( stripCenter, stripLeftEdge, stripRightEdge );

        return true;
    }
    return false;
}


UChar_t StFttDb::plane( StFttRawHit * hit ){
    if ( hit->plane() < nPlane )
        return hit->plane();
    return hit->sector() - 1;
}

UChar_t StFttDb::quadrant( StFttRawHit * hit ){
    if ( hit->quadrant() < nQuad )
        return hit->quadrant();
    return hit->rdo() - 1;
}

UChar_t StFttDb::rob( StFttRawHit * hit ){
    return quadrant(hit) + ( plane(hit) * nQuadPerPlane ) + 1;
}

UChar_t StFttDb::rob( StFttCluster * clu ){
    return clu->quadrant() + ( clu->plane() * StFttDb::nQuadPerPlane );
}

UChar_t StFttDb::fob( StFttRawHit * hit ){
    return hit->feb() + ( quadrant( hit ) * nFobPerQuad ) + ( plane(hit) * nFobPerPlane ) + 1;
}

UChar_t StFttDb::orientation( StFttRawHit * hit ){
    if ( hit->orientation() < kFttUnknownOrientation ){
        return hit->orientation();
    }
    return kFttUnknownOrientation;
}

void StFttDb::getGloablOffset( UChar_t plane, UChar_t quad, 
                                float &dx, float &sx,
                                float &dy, float &sy, 
                                float &dz, float &sz ){
    // TODO: connect to DB for calibrated positions. 
    // for now we use the ideal positions (from simulated geometry)
    // calibration will come later

    // scale factors
    sx = 1.0;
    sy = 1.0;
    sz = 1.0;

    // shifts
    dx = 0.0;
    dy = 59.0;
    dz = 0.0;

    if ( plane < 4 )
        dz = StFttDb::idealPlaneZLocations[plane];

    // upper quadrants are not displaced
    if ( quad == 0 )
        dx = 0.0; 
    else if ( quad == 1 )
        dx = StFttDb::lowerQuadOffsetX;
    else if ( quad == 2 )
        dx = StFttDb::lowerQuadOffsetX;
    else if ( quad == 3 )
        dx = 0.0;

    // these are the reflections of a pentagon into the symmetric shape for quadrants A, B, C, D
    if ( quad == 1 ){
        sy = -1.0;
        // this is so that we get everything shifted up
        dy = dy * sy; // flip it now so it does not get flipped later by scale factor;
    }
    else if ( quad == 2 ){
        sx = -1.0;
        sy = -1.0;
        // this is so that we get everything shifted up
        dy = dy * sy; // flip it now so it does not get flipped later by scale factor;

    } else if ( quad == 3 )
        sx = -1.0;

}