#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include <memory>
#include "TString.h"




#ifndef __CINT__
#include <assert.h>
/*********************************************************/
// class for mapping the VMM electronics
// 
// Some terminology
// in the raw data from Tonko we have (SEC, RDO, FEB ...)
// these correspond to our sTGC naming as:
//
// SEC = Plane (1, 2, 3, 4)
// RDO = Fiber # = ROB 
// RDO = 1 - 4 for each Plane
// Fiber and ROB = 1 - 16; RDO + (4 * SEC)
// FEB = FOB

class VMMHardwareMap {
public:
    VMMHardwareMap() {}
    VMMHardwareMap( std::string mapfile ){
        loadMap( mapfile );
    }
    ~VMMHardwareMap(){}

    uint16_t packKey( int feb, int vmm, int ch ){
        // feb = [1 - 6] = 3 bits
        // vmm = [1 - 4] = 3 bits
        // ch  = [1 - 64] = 7 bits
        return feb + (vmm << 3) + (ch << 6);
    }
    void unpackKey( int key, int &feb, int &vmm, int &ch ){
        feb = key & 0b111;
        vmm = (key >> 3) & 0b111;
        ch  = (key >> 6) & 0b1111111;
        return;
    }
    uint16_t packVal( int row, int strip ){
        // row = [1 - 4] = 3 bits
        // strip = [1 - 152] = 8 bits
        return row + ( strip << 3 );
    }
    void unpackVal( int val, int &row, int &strip ){
        row = val & 0b111; // 3 bits
        strip = (val >> 3) & 0b11111111; // 8 bit
        return;
    }

    void loadMap( std::string fn ){
        ifstream inf;
        inf.open( fn.c_str() );

        if ( !inf ) return;

        string hs0, hs1, hs2, hs3, hs4;
        // HEADER:
        // Row_num    FEB_num    VMM_num    VMM_ch         strip_ch
        inf >> hs0 >> hs1 >> hs2 >> hs3 >> hs4;
        
        if ( DEBUG ){
            printf( "Map Header: %s, %s, %s, %s, %s", hs0.c_str(), hs1.c_str(), hs2.c_str(), hs3.c_str(), hs4.c_str() );
            puts("");
        }
        
        uint16_t row, feb, vmm, ch, strip;
        while( inf >> row >> feb >> vmm >> ch >> strip ){
            // pack the key (feb, vmm, ch)
            uint16_t key = packKey( feb, vmm, ch );
            uint16_t val = packVal( row, strip );
            mMap[ key ] = val;
            if ( DEBUG ){
                printf( "in=(feb=%d, vmm=%d, ch=%d)\n", feb, vmm, ch );
                int ufeb, uvmm, uch;
                unpackKey( key, ufeb, uvmm, uch );
                printf( "key=(feb=%d, vmm=%d, ch=%d)\n", ufeb, uvmm, uch ); puts("");
                assert( feb == ufeb && vmm == uvmm && ch == uch );
                int urow, ustrip;
                unpackVal( val, urow, ustrip );
                assert( row == urow && strip == ustrip );
            }
        }
        inf.close();
    }

    // Enum for strip orientation in descriptive terms
    enum class StripOrientation: int {
        Horizontal = 0,
        Vertical = 1,
        Diagonal = 2,
        Unknown = 3
    };

    // need non-class enum to get CINT to work
    enum Quadrant {
        A = 0,
        B = 1,
        C = 2,
        D = 3
    };

    static constexpr double stripPitch = 3.2; // mm
    static constexpr double rowLength = 180; // mm

    // same for all planes
    // we have quadrants like:
    // 
    // D | A
    // ------
    // C | B
    // Row 3 and 4 are always diagonal
    // odd (even) FOB are horizontal (vertical) for A and C (B and D)
    // even (odd) FOB are vertical (horizontal) for A and C (B and D)
    StripOrientation getOrientation( int rob, int feb, int vmm, int row ) {
        if ( DEBUG ) {
            printf( "getOrientation( %d, %d, %d, %d )", rob, feb, vmm, row ); puts("");
        }
        if ( 3 == row || 4 == row ){
            return StripOrientation::Diagonal;
        }
        if ( rob % 2 == 0 ){ // even rob
            if ( feb % 2 != 0 ) { // odd
                return StripOrientation::Horizontal;
            }
            // even
            return StripOrientation::Vertical;
        } else { // odd rob
            if ( feb % 2 != 0 ) { // odd
                return StripOrientation::Vertical;
            }
            // even
            return StripOrientation::Horizontal;
        }
        // should never get here!
        return StripOrientation::Unknown;
    }

    /* get 
     * returns the mapping for a given input
     * 
     * input:
     *      rob: 1 - 16
     *      feb: 1 - 6
     *      vmm: 1 - 4
     *      ch : 1 - 64
     *
     * output:
     *      row: 0 - 4
     *      strip: 0 - 162
     *      orientation: one of {Horizontal, Vertical, Diagonal, Unknown}
     *
     */
    bool get( int rob, int feb, int vmm, int ch, int &row, int &strip, StripOrientation &orientation ){
        uint16_t key = packKey( feb, vmm, ch );
        if ( mMap.count( key ) ){
            uint16_t val = mMap[ key ];
            unpackVal( val, row, strip );
            orientation = getOrientation( rob, feb, vmm, row );
            return true;
        }
        return false;
    }

    static const int DEBUG=0;
    std :: map< uint16_t , uint16_t > mMap;
};
#endif

#ifdef __CINT__
class VMMHardwareMap;
#endif


class stgc_vmm_t;

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//

// class VMMHardwareMap;
// enum class Quadrant: int;

class fttBuilder : public JevpBuilder {
 public:
 
  fttBuilder(JevpServer *parent=NULL) : JevpBuilder(parent) {
    plotsetname = (char *)"ftt";
    memset(&contents, 0, sizeof(contents));
  }

  ~fttBuilder() {
    int n = sizeof(contents) / sizeof(TH1 *);
    for(int i=0;i<n;i++) {
      if(contents.array[i]) delete contents.array[i];
    }
  }

  // base 
  static const size_t nPlane        = 4;
  static const size_t nQuadPerPlane = 4;
  static const size_t nFobPerQuad   = 6;
  static const size_t nVMMPerFob    = 4;
  static const size_t nChPerVMM     = 64;

  static const size_t nQuad = nQuadPerPlane * nPlane; // 4 * 4 = 16 Total number of Quadrants

  static const size_t nFobPerPlane = nFobPerQuad * nQuadPerPlane; // 4 * 6 = 24 Total number of FOB
  static const size_t nFob         = nFobPerPlane * nPlane; // 24 * 4 = 96 total FOB;
  
  static const size_t nVMMPerQuad   = nFobPerQuad * nVMMPerFob; // 6 * 4 = 24 VMM per Quad
  static const size_t nVMMPerPlane = nVMMPerQuad * nQuadPerPlane; // 24 * 4 = 96 VMM per Plane
  static const size_t nVMM         = nVMMPerPlane * nPlane; // 96 * 4 = 384 VMM total

  static const size_t nChPerFob    = nVMMPerFob * nChPerVMM; // 4 * 64 = 256 VMM per Plane
  static const size_t nChPerQuad    = nChPerFob * nFobPerQuad; // 256 * 6 = 1536 VMM per Plane
  static const size_t nChPerPlane  = nChPerQuad * nQuadPerPlane; // 1536 * 4 = 6144 Ch per Plane
  static const size_t nCh          = nChPerPlane * nPlane; // 6144 * 4 = 24576 Ch per Plane
  
  static const size_t maxADC       = 1024 + 1; // 10 bits;
  static const size_t maxBCID      = 65536 + 1; // 16 bits;
  static const std::string quadLabels[4];
  static const std::string dirLabels[4];

  // Histo declarations!
  union {
    TH1 *array[];
    struct {
        // VMM electronics
        TH1 *hitsPerPlane;
        TH1 *hitsPerQuad; // all disks
        TH1 *hitsPerFob; // all disks
        TH1 *hitsPerVMM; // all Quadrants, all Planes
        // TH1 *SEC0;
        TH2 *hitsPerPlaneQuad; 
        TH2 *hitsPerVMMPlane; 
        // TH2 *SECRDO0; 
        TH2 *hitsFobQuadPerPlane[nPlane];
        TH1 *hitsVMMPerPlane[nPlane];
        TH2 *hitsVMMChPerPlaneQuad[nQuad];

        TH2 *adcVMMChPerPlaneQuad[nQuad];
        TH2 *adcVMM;
        TH2 *bcidVMM;

        TH2 *hStripPerPlane[nPlane];
        TH2 *vStripPerPlane[nPlane];
        TH2 *dStripPerPlane[nPlane];
        
        TH1 *nStripsFired;
    };
  } contents;

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);

  void processVMM(daqReader *rdr);

  
  void processTPX(daqReader *rdr);

  void setQuadLabels( TAxis * ax ) {
    ax->SetBinLabel( 1, TString::Format( "%s", quadLabels[0].c_str() ) );
    ax->SetBinLabel( 2, TString::Format( "%s", quadLabels[1].c_str() ) );
    ax->SetBinLabel( 3, TString::Format( "%s", quadLabels[2].c_str() ) );
    ax->SetBinLabel( 4, TString::Format( "%s", quadLabels[3].c_str() ) );
  }

  void floodFill( TH2 * h2, int x0, int y0, int x1, int y1, double w = 1.0 ){
    for( int ix = x0; ix <= x1; ix++ ){
        for( int iy = y0; iy <= y1; iy++ ){
            h2->SetBinContent( ix, iy, h2->GetBinContent( ix, iy ) + w );
        }
    }
  }
  

  


  static void main(int argc, char *argv[]);

#ifndef __CINT__
  void drawStrip( TH2 * h2, int row, int strip, VMMHardwareMap::Quadrant q, VMMHardwareMap::StripOrientation so );
    void processVMMHit( int iPlane, VMMHardwareMap::Quadrant iQuad, stgc_vmm_t rawVMM);
    shared_ptr<VMMHardwareMap> mHardwareMap;
#endif


  int nStripsFired;

  ClassDef(fttBuilder, 1);
};
