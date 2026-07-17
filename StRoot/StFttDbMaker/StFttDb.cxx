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
#include "tables/St_fttDataWindowsB_Table.h"


ClassImp(StFttDb)


double StFttDb::stripPitch = 3.2; // mm
double StFttDb::rowLength = 180; // mm
double StFttDb::lowerQuadOffsetX = 101.6; // mm
double StFttDb::idealPlaneZLocations[] = { 280.90499, 303.70498, 326.60501, 349.40499 };

vector<string> StFttDb::orientationLabels = { "Horizontal", "Vertical", "DiagonalH", "DiagonalV", "Unknown" };


StFttDb::StFttDb(const char *name) : TDataSet(name) {
    initStripGeometry();
};

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

size_t StFttDb::vmmId( StFttRawHit * h ) {
    // Calculate VMM hardware ID based on electronic readout structure
    // VMM_ID = vmm + nVMMPerFob * (feb + nFobPerQuad * (quadrant + nQuadPerPlane * plane))
    // Where: plane [0-3], quadrant [0-3], feb [0-5], vmm [0-3]
    // Valid range: 0-383 (total of 384 VMMs)

    u_char iPlane = h->sector() - 1;     // sector is 1-based
    u_char iQuad  = h->rdo() - 1;        // rdo is 1-based
    u_char iFeb   = h->feb();            // feb is 0-based
    u_char iVmm   = h->vmm();            // vmm is 0-based

    size_t vmm_id = iVmm + nVMMPerFob * ( iFeb + nFobPerQuad * ( iQuad + nQuadPerPlane * iPlane ) );

    return vmm_id;
}


void StFttDb::getTimeCut( StFttRawHit * hit, int &mode, int &l, int &h ){
        mode = mTimeCutMode;
        l = mTimeCutLow;
        h = mTimeCutHigh;
        if (mUserDefinedTimeCut)
            return;

        // load calibrated data windows from DB
        // NOTE: dwMap is indexed by VMM hardware ID, not geometric UUID
        size_t hit_vmmid = vmmId( hit );

        // Validate VMM ID is in expected range
        if ( hit_vmmid >= nVMM ) {
            LOG_WARN << "StFttDb::getTimeCut - VMM ID out of range: " << hit_vmmid
                     << " (max=" << (nVMM-1) << ")" << endm;
            LOG_WARN << "  Hit: plane=" << (int)plane(hit)
                     << " quad=" << (int)quadrant(hit)
                     << " feb=" << (int)hit->feb()
                     << " vmm=" << (int)hit->vmm() << endm;
            return;
        }

        if ( dwMap.count( hit_vmmid ) ){
            mode = dwMap[ hit_vmmid ].mode;
            l = dwMap[ hit_vmmid ].min;
            h = dwMap[ hit_vmmid ].max;
        }

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

void StFttDb::loadDataWindowsFromDb( St_fttDataWindowsB * dataset ) {
    if (dataset) {
        Int_t rows = dataset->GetNRows();

        if ( !rows ) return;

        dwMap.clear();

        fttDataWindowsB_st *table = dataset->GetTable();
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
        LOG_ERROR << "dataset does not contain requested table" << endm;
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
    LOG_INFO << "sTGC Hardware map loaded from File: " << fn << endm;
}

// FTT detector hardware was retired in the static configuration captured below.
// Strip center, edge, and length tables previously lived in Row*.txt /
// Row*_StripLength.txt / Row4_edge.txt and are now compiled in. All values are
// in millimetres relative to the local pin-hole origin.
namespace {

    // Row1.txt: H/V strip centers, indexed by strip number 0..166 (167 strips)
    constexpr Float_t kStripCenterXY[] = {
        15.95f, 19.15f, 22.35f, 25.55f, 28.75f, 31.95f, 35.15f, 38.35f,
        41.55f, 44.75f, 47.95f, 51.15f, 54.35f, 57.55f, 60.75f, 63.95f,
        67.15f, 70.35f, 73.55f, 76.75f, 79.95f, 83.15f, 86.35f, 89.55f,
        92.75f, 95.95f, 99.15f, 102.35f, 105.55f, 108.75f, 111.95f, 115.15f,
        118.35f, 121.55f, 124.75f, 127.95f, 131.15f, 134.35f, 137.55f, 140.75f,
        143.95f, 147.15f, 150.35f, 153.55f, 156.75f, 159.95f, 163.15f, 166.35f,
        169.55f, 172.75f, 175.95f, 179.15f, 182.35f, 185.55f, 188.75f, 191.95f,
        195.15f, 198.35f, 201.55f, 204.75f, 207.95f, 211.15f, 214.35f, 217.55f,
        220.75f, 223.95f, 227.15f, 230.35f, 233.55f, 236.75f, 239.95f, 243.15f,
        246.35f, 249.55f, 252.75f, 255.95f, 259.15f, 262.35f, 265.55f, 268.75f,
        271.95f, 275.15f, 278.35f, 281.55f, 284.75f, 287.95f, 291.15f, 294.35f,
        297.55f, 300.75f, 303.95f, 307.15f, 310.35f, 313.55f, 316.75f, 319.95f,
        323.15f, 326.35f, 329.55f, 332.75f, 335.95f, 339.15f, 342.35f, 345.55f,
        348.75f, 351.95f, 355.15f, 358.35f, 361.55f, 364.75f, 367.95f, 371.15f,
        374.35f, 377.55f, 380.75f, 383.95f, 387.15f, 390.35f, 393.55f, 396.75f,
        399.95f, 403.15f, 406.35f, 409.55f, 412.75f, 415.95f, 419.15f, 422.35f,
        425.55f, 428.75f, 431.95f, 435.15f, 438.35f, 441.55f, 444.75f, 447.95f,
        451.15f, 454.35f, 457.55f, 460.75f, 463.95f, 467.15f, 470.35f, 473.55f,
        476.75f, 479.95f, 483.15f, 486.35f, 489.55f, 492.75f, 495.95f, 499.15f,
        502.35f, 505.55f, 508.75f, 511.95f, 515.15f, 518.35f, 521.55f, 524.75f,
        527.95f, 531.15f, 534.35f, 537.55f, 540.75f, 543.95f, 547.15f
    };

    // Row4.txt: diagonal strip centers, indexed by strip number 0..151 (152 strips)
    constexpr Float_t kStripCenterDiag[] = {
        25.14f, 28.34f, 31.54f, 34.74f, 37.94f, 41.14f, 44.34f, 47.54f,
        50.74f, 53.94f, 57.14f, 60.34f, 63.54f, 66.74f, 69.94f, 73.14f,
        76.34f, 79.54f, 82.74f, 85.94f, 89.14f, 92.34f, 95.54f, 98.74f,
        101.94f, 105.14f, 108.34f, 111.54f, 114.74f, 117.94f, 121.14f, 124.34f,
        127.54f, 130.74f, 133.94f, 137.14f, 140.34f, 143.54f, 146.74f, 149.94f,
        153.14f, 156.34f, 159.54f, 162.74f, 165.94f, 169.14f, 172.34f, 175.54f,
        178.74f, 181.94f, 185.14f, 188.34f, 191.54f, 194.74f, 197.94f, 201.14f,
        204.34f, 207.54f, 210.74f, 213.94f, 217.14f, 220.34f, 223.54f, 226.74f,
        229.94f, 233.14f, 236.34f, 239.54f, 242.74f, 245.94f, 249.14f, 252.34f,
        255.54f, 258.74f, 261.94f, 265.14f, 268.34f, 271.54f, 274.74f, 277.94f,
        281.14f, 284.34f, 287.54f, 290.74f, 293.94f, 297.14f, 300.34f, 303.54f,
        306.74f, 309.94f, 313.14f, 316.34f, 319.54f, 322.74f, 325.94f, 329.14f,
        332.34f, 335.54f, 338.74f, 341.94f, 345.14f, 348.34f, 351.54f, 354.74f,
        357.94f, 361.14f, 364.34f, 367.54f, 370.74f, 373.94f, 377.14f, 380.34f,
        383.54f, 386.74f, 389.94f, 393.14f, 396.34f, 399.54f, 402.74f, 405.94f,
        409.14f, 412.34f, 415.54f, 418.74f, 421.94f, 425.14f, 428.34f, 431.54f,
        434.74f, 437.94f, 441.14f, 444.34f, 447.54f, 450.74f, 453.94f, 457.14f,
        460.34f, 463.54f, 466.74f, 469.94f, 473.14f, 476.34f, 479.54f, 482.74f,
        485.94f, 489.14f, 492.34f, 495.54f, 498.74f, 501.94f, 505.14f, 508.34f
    };

    // Row4_edge.txt: diagonal strip left/right edges, 0..151
    constexpr Float_t kStripEdgeDiagLeft[] = {
        23.79f, 26.99f, 30.19f, 33.39f, 36.59f, 39.79f, 42.99f, 46.19f,
        49.39f, 52.59f, 55.79f, 58.99f, 62.19f, 65.39f, 68.59f, 71.79f,
        74.99f, 78.19f, 81.39f, 84.59f, 87.79f, 90.99f, 94.19f, 97.39f,
        100.59f, 103.79f, 106.99f, 110.19f, 113.39f, 116.59f, 119.79f, 122.99f,
        126.19f, 129.39f, 132.59f, 135.79f, 138.99f, 142.19f, 145.39f, 148.59f,
        151.79f, 154.99f, 158.19f, 161.39f, 164.59f, 167.79f, 170.99f, 174.19f,
        177.39f, 180.59f, 183.79f, 186.99f, 190.19f, 193.39f, 196.59f, 199.79f,
        202.99f, 206.19f, 209.39f, 212.59f, 215.79f, 218.99f, 222.19f, 225.39f,
        228.59f, 231.79f, 234.99f, 238.19f, 241.39f, 244.59f, 247.79f, 250.99f,
        254.19f, 257.39f, 260.59f, 263.79f, 266.99f, 270.19f, 273.39f, 276.59f,
        279.79f, 282.99f, 286.19f, 289.39f, 292.59f, 295.79f, 298.99f, 302.19f,
        305.39f, 308.59f, 311.79f, 314.99f, 318.19f, 321.39f, 324.59f, 327.79f,
        330.99f, 334.19f, 337.39f, 340.59f, 343.79f, 346.99f, 350.19f, 353.39f,
        356.59f, 359.79f, 362.99f, 366.19f, 369.39f, 372.59f, 375.79f, 378.99f,
        382.19f, 385.39f, 388.59f, 391.79f, 394.99f, 398.19f, 401.39f, 404.59f,
        407.79f, 410.99f, 414.19f, 417.39f, 420.59f, 423.79f, 426.99f, 430.19f,
        433.39f, 436.59f, 439.79f, 442.99f, 446.19f, 449.39f, 452.59f, 455.79f,
        458.99f, 462.19f, 465.39f, 468.59f, 471.79f, 474.99f, 478.19f, 481.39f,
        484.59f, 487.79f, 490.99f, 494.19f, 497.39f, 500.59f, 503.79f, 506.99f
    };

    constexpr Float_t kStripEdgeDiagRight[] = {
        26.49f, 29.69f, 32.89f, 36.09f, 39.29f, 42.49f, 45.69f, 48.89f,
        52.09f, 55.29f, 58.49f, 61.69f, 64.89f, 68.09f, 71.29f, 74.49f,
        77.69f, 80.89f, 84.09f, 87.29f, 90.49f, 93.69f, 96.89f, 100.09f,
        103.29f, 106.49f, 109.69f, 112.89f, 116.09f, 119.29f, 122.49f, 125.69f,
        128.89f, 132.09f, 135.29f, 138.49f, 141.69f, 144.89f, 148.09f, 151.29f,
        154.49f, 157.69f, 160.89f, 164.09f, 167.29f, 170.49f, 173.69f, 176.89f,
        180.09f, 183.29f, 186.49f, 189.69f, 192.89f, 196.09f, 199.29f, 202.49f,
        205.69f, 208.89f, 212.09f, 215.29f, 218.49f, 221.69f, 224.89f, 228.09f,
        231.29f, 234.49f, 237.69f, 240.89f, 244.09f, 247.29f, 250.49f, 253.69f,
        256.89f, 260.09f, 263.29f, 266.49f, 269.69f, 272.89f, 276.09f, 279.29f,
        282.49f, 285.69f, 288.89f, 292.09f, 295.29f, 298.49f, 301.69f, 304.89f,
        308.09f, 311.29f, 314.49f, 317.69f, 320.89f, 324.09f, 327.29f, 330.49f,
        333.69f, 336.89f, 340.09f, 343.29f, 346.49f, 349.69f, 352.89f, 356.09f,
        359.29f, 362.49f, 365.69f, 368.89f, 372.09f, 375.29f, 378.49f, 381.69f,
        384.89f, 388.09f, 391.29f, 394.49f, 397.69f, 400.89f, 404.09f, 407.29f,
        410.49f, 413.69f, 416.89f, 420.09f, 423.29f, 426.49f, 429.69f, 432.89f,
        436.09f, 439.29f, 442.49f, 445.69f, 448.89f, 452.09f, 455.29f, 458.49f,
        461.69f, 464.89f, 468.09f, 471.29f, 474.49f, 477.69f, 480.89f, 484.09f,
        487.29f, 490.49f, 493.69f, 496.89f, 500.09f, 503.29f, 506.49f, 509.69f
    };

    // Row1_StripLength.txt: H/V strip lengths for Row 0, 0..166
    constexpr Float_t kStripLengthRow1[] = {
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f, 160.8f,
        160.8f, 205.4f, 202.2f, 199.0f, 195.8f, 192.6f, 189.4f, 186.2f,
        183.0f, 179.8f, 176.6f, 173.4f, 170.2f, 167.0f, 163.8f
    };

    // Row2_StripLength.txt: H/V strip lengths for Row 1, 0..152
    constexpr Float_t kStripLengthRow2[] = {
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f,
        188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 188.5f, 232.8f, 229.6f,
        226.4f, 223.2f, 220.0f, 216.8f, 213.6f, 210.4f, 207.2f, 204.0f,
        200.8f, 197.6f, 194.4f, 191.2f, 188.0f, 184.8f, 181.6f, 178.4f,
        175.2f, 172.0f, 168.8f, 165.6f, 162.4f, 159.2f, 156.0f, 152.8f,
        149.6f, 146.4f, 143.2f, 140.0f, 136.8f, 133.6f, 130.4f, 127.2f,
        124.0f, 120.8f, 117.6f, 114.4f, 111.2f, 108.0f, 104.8f, 101.6f,
        98.4f, 95.2f, 92.0f, 88.8f, 85.6f, 82.4f, 79.2f, 76.0f,
        72.8f, 69.6f, 66.4f, 63.2f, 60.0f, 56.8f, 53.6f, 50.4f,
        47.2f
    };

    // Row3_StripLength.txt: H/V strip lengths for Row 2, 0..93
    constexpr Float_t kStripLengthRow3[] = {
        184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f,
        184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f,
        184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f,
        184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f,
        184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f,
        184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f, 184.4f,
        184.4f, 184.4f, 184.4f, 181.4f, 178.2f, 175.0f, 171.8f, 168.6f,
        165.4f, 162.2f, 159.0f, 155.8f, 152.6f, 149.4f, 146.2f, 143.0f,
        139.8f, 136.6f, 133.4f, 130.2f, 127.0f, 123.8f, 120.6f, 117.4f,
        114.2f, 111.0f, 107.8f, 104.6f, 101.4f, 98.2f, 95.0f, 91.8f,
        88.6f, 85.4f, 82.2f, 79.0f, 75.8f, 72.6f, 69.4f, 66.2f,
        63.0f, 59.8f, 56.6f, 53.4f, 50.2f, 47.0f
    };

    // Row4_StripLength.txt: diagonal strip lengths for Row 3, 0..150
    constexpr Float_t kStripLengthRow4[] = {
        6.6f, 9.8f, 13.0f, 16.2f, 19.4f, 22.6f, 25.8f, 29.0f,
        32.2f, 35.4f, 38.6f, 41.8f, 45.0f, 48.2f, 51.4f, 54.6f,
        57.8f, 61.0f, 64.2f, 67.4f, 70.6f, 73.8f, 77.0f, 80.2f,
        83.4f, 86.6f, 89.8f, 93.0f, 96.2f, 99.4f, 102.6f, 105.8f,
        109.0f, 112.2f, 115.4f, 118.6f, 121.8f, 125.0f, 128.2f, 131.4f,
        134.6f, 137.8f, 141.0f, 144.2f, 147.4f, 150.6f, 153.8f, 157.0f,
        160.2f, 163.4f, 166.6f, 169.8f, 173.0f, 176.2f, 179.4f, 182.6f,
        185.8f, 189.0f, 192.2f, 195.4f, 198.6f, 201.8f, 205.0f, 208.2f,
        211.4f, 214.6f, 217.8f, 221.0f, 224.2f, 227.4f, 230.6f, 233.8f,
        237.0f, 240.2f, 243.4f, 246.6f, 249.8f, 253.0f, 256.2f, 259.4f,
        262.6f, 265.8f, 269.0f, 272.2f, 275.4f, 278.6f, 281.8f, 285.0f,
        288.2f, 291.4f, 294.6f, 297.8f, 301.0f, 304.2f, 307.4f, 310.6f,
        313.8f, 317.0f, 320.2f, 323.4f, 326.6f, 329.8f, 333.0f, 336.2f,
        339.4f, 342.6f, 345.8f, 349.0f, 352.2f, 355.4f, 358.6f, 361.8f,
        365.0f, 368.2f, 371.4f, 374.6f, 377.8f, 381.0f, 377.8f, 374.6f,
        371.4f, 368.2f, 365.0f, 361.8f, 358.6f, 355.4f, 352.2f, 349.0f,
        345.8f, 342.6f, 339.4f, 336.2f, 333.0f, 329.8f, 326.6f, 323.4f,
        320.2f, 317.0f, 313.8f, 310.6f, 307.4f, 304.2f, 301.0f, 297.8f,
        294.6f, 291.4f, 288.2f, 285.0f, 281.8f, 278.6f, 275.4f
    };

    // Row5_StripLength.txt: diagonal strip lengths for Row 4, 0..58
    constexpr Float_t kStripLengthRow5[] = {
        9.6f, 12.8f, 16.0f, 19.2f, 22.4f, 25.6f, 28.8f, 32.0f,
        35.2f, 38.4f, 41.6f, 44.8f, 48.0f, 51.2f, 54.4f, 57.6f,
        60.8f, 64.0f, 67.2f, 70.4f, 73.6f, 76.8f, 80.0f, 83.2f,
        86.4f, 89.6f, 92.8f, 96.0f, 99.2f, 102.4f, 105.6f, 108.8f,
        112.0f, 115.2f, 118.4f, 121.6f, 124.8f, 128.0f, 131.2f, 134.4f,
        137.6f, 140.8f, 144.0f, 147.2f, 150.4f, 153.6f, 156.8f, 160.0f,
        163.2f, 166.4f, 169.6f, 172.8f, 176.0f, 179.2f, 182.4f, 185.6f,
        188.8f, 192.0f, 195.2f
    };

    template <typename Map, typename Array>
    void fillMap( Map & m, const Array & a ) {
        m.clear();
        const size_t n = sizeof(a) / sizeof(a[0]);
        for ( size_t i = 0; i < n; ++i ) m[ (int)i ] = a[i];
    }

} // anonymous namespace

void StFttDb::initStripGeometry() {
    fillMap( scMapXY,        kStripCenterXY );
    fillMap( scMapDiag,      kStripCenterDiag );
    fillMap( seMapDiagLeft,  kStripEdgeDiagLeft );
    fillMap( seMapDiagRight, kStripEdgeDiagRight );
    fillMap( slMapRow1,      kStripLengthRow1 );
    fillMap( slMapRow2,      kStripLengthRow2 );
    fillMap( slMapRow3,      kStripLengthRow3 );
    fillMap( slMapRow4,      kStripLengthRow4 );
    fillMap( slMapRow5,      kStripLengthRow5 );
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
        if ( feb % 2 == 0 ) { // even feb
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
        
        if ( feb % 2 == 0 ) { // even feb
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
    if ( mDebug ) {
        LOG_DEBUG << "kFttUnknownOrientation = " << kFttUnknownOrientation << endm;
    }
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
    dy = 6.0;
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
    if ( quad == 1 )
        sy = -1.0;
    else if ( quad == 2 ){
        sx = -1.0;
        sy = -1.0;
    } else if ( quad == 3 )
        sx = -1.0;

}