#include <stdio.h>
#include <stdlib.h>
#include <fstream>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h>
#include <DAQ_STGC/daq_stgc.h>
#include "Jevp/StJevpPlot/RunStatus.h"

#include <TH1D.h>
#include <TH2F.h>
#include <TString.h>

#include <math.h>
#include "fttBuilder.h"
#include <RTS/include/rtsLog.h>
#include <rtsSystems.h>

#include <algorithm>    // std::find
#include <vector>       // std::vector
#include <assert.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//

/*********************************************************/
// class for mapping the TPX electronics

#include <map>
#include <string>

// helper macro for string formatting
#define TSF( ... ) TString::Format(__VA_ARGS__)


ClassImp(fttBuilder);

const std::string fttBuilder::quadLabels[4] = {"A", "B", "C", "D"};
const std::string fttBuilder::dirLabels[4]  = {"Horizontal", "Vertical", "Diagonal", "Unknown"};

void fttBuilder::initialize(int argc, char *argv[]) {
    

    // Control draw/visiblity options
    std::vector<std::string> setLogy   = { "hitsPerTb", "chargePerPlane0", "chargePerPlane1", "chargePerPlane2", "chargePerPlane3", "hitsTbPerPlane0", "hitsTbPerPlane1", "hitsTbPerPlane2", "hitsTbPerPlane3", "hitsPerPlane", "hitsPerQuad", "hitsPerFob", "hitsPerVMM", "hitsVMMPerPlane0", "hitsVMMPerPlane1", "hitsVMMPerPlane2", "hitsVMMPerPlane3", "nStripsFired" };
    std::vector<std::string> setLogz   = { "hitsPerPlaneQuad", "hitsFobQuadPerPlane0", "hitsFobQuadPerPlane1", "hitsFobQuadPerPlane2", "hitsFobQuadPerPlane3" };
    std::vector<std::string> showStats = {  };

    //////////////////////////////////////////////////////////////////////// 
    // General
    ////////////////////////////////////////////////////////////////////////
        contents.nStripsFired            = new TH1D( "nStripsFired", "sTGC; nStripsFired; counts", 6144, 0, 6144 );

    //////////////////////////////////////////////////////////////////////// 
    // hits and adc
    ////////////////////////////////////////////////////////////////////////
        contents.hitsPerPlane             = new TH1D( "hitsPerPlane", "sTGC (hits / Plane); Plane; counts (hits)", nPlane,0.5, nPlane + 0.5 );
        contents.hitsPerQuad              = new TH1D( "hitsPerQuad", "sTGC (hits / Quadrant); Plane & Quadrant; counts (hits)", nQuad,0.5, nQuad + 0.5 );
        contents.hitsPerFob               = new TH1D( "hitsPerFob", "sTGC (hits / Fob); Fob; counts (hits)", nFob,0.5, nFob + 0.5 );
        contents.hitsPerVMM               = new TH1D( "hitsPerVMM", "sTGC (hits / VMM); VMM Index (96VMM / Plane); counts (hits)", nVMM,0.5, nVMM + 0.5 );
        contents.hitsPerTb                = new TH1D( "hitsPerTb", "sTGC (hits / Timebin); Tb; counts (hits)", 338, minTb, maxTb );
        contents.hitsPerTb400             = new TH1D( "hitsPerTb400", "sTGC (hits / Timebin); Tb; counts (hits)", 400, -400, 400 );
        contents.hitsPerPlaneQuad         = new TH2D( "hitsPerPlaneQuad", "sTGC (hits / Quadrant); Plane; Quadrant", nPlane,0.5, nPlane + 0.5, nQuadPerPlane, 0.5, nQuadPerPlane + 0.5);
        contents.hitsPerVMMPlane          = new TH2D( "hitsPerVMMPlane", "sTGC (hits / VMM / Plane); VMM index; Plane", nVMMPerPlane, 0.5, nVMMPerPlane+0.5, nPlane,0.5, nPlane + 0.5);
        contents.adcVMM                   = new TH2D( "adcVMM", "sTGC; VMM; ADC", nVMM,0.5, nVMM + 0.5, maxADC/10.0, 0, maxADC);
        contents.bcidVMM                  = new TH2D( "bcidVMM", "sTGC; VMM; BCID", nVMM,0.5, nVMM + 0.5, maxBCID/10.0, 0, maxBCID);
    
        contents.hitsPerPlane->GetXaxis()->SetNdivisions( 5, 1, 0 );
        contents.hitsPerPlaneQuad->GetXaxis()->SetNdivisions( 5, 1, 0 );
        contents.hitsPerVMMPlane->GetYaxis()->SetNdivisions( 5, 1, 0 );

    //////////////////////////////////////////////////////////////////////// 
    // hits Per Plane
    ////////////////////////////////////////////////////////////////////////
        for ( u_char iPlane = 0; iPlane < fttBuilder::nPlane; iPlane ++ ){
            contents.hitsFobQuadPerPlane[iPlane]    = new TH2D( TSF("hitsFobQuadPerPlane%d", iPlane), TSF("sTGC Plane %d (hits / Fob); Fob index; Quadrant", iPlane+1), nFobPerQuad, 0.5, nFobPerQuad+0.5, nQuadPerPlane, 0.5, nQuadPerPlane+0.5 );
            setQuadLabels( contents.hitsFobQuadPerPlane[iPlane]->GetYaxis() );
            contents.hitsVMMPerPlane[iPlane]        = new TH1D( TSF("hitsVMMPerPlane%d", iPlane), TSF("sTGC Plane %d (hits / VMM); VMM; counts (hits)", iPlane+1), nVMMPerPlane, 0.5, nVMMPerPlane + 0.5 );
            contents.hStripPerPlane[iPlane]         = new TH2D( TSF("hStripPerPlane%d", iPlane), TSF("sTGC Horizontal Strips, Plane %d (hits / Strip); x; y", iPlane+1), 140, -700, 700, 438, -700, 700 );
            contents.vStripPerPlane[iPlane]         = new TH2D( TSF("vStripPerPlane%d", iPlane), TSF("sTGC Vertical Strips, Plane %d (hits / Strip); x; y", iPlane+1), 438, -700, 700, 140, -700, 700 );
            contents.dStripPerPlane[iPlane]         = new TH2D( TSF("dStripPerPlane%d", iPlane), TSF("sTGC Diagonal Strips, Plane %d (hits / Strip); x; y", iPlane+1), 438, -700, 700, 438, -700, 700 );

            contents.hitsTbPerPlane[iPlane]        = new TH1D( TSF("hitsTbPerPlane%d", iPlane), TSF("sTGC Plane %d (Timing); Tb; counts (hits)", iPlane+1), 400, -400, 400 );
            contents.chargePerPlane[iPlane]        = new TH1D( TSF("chargePerPlane%d", iPlane), TSF("sTGC Plane %d (Charge); ADC; counts (hits)", iPlane+1), maxADC/10.0, 0, maxADC );


            for ( u_char iQuad = 0; iQuad < nQuadPerPlane; iQuad ++ ){
                u_char iQuadPerFtt = iQuad + (iPlane * nQuadPerPlane);
                contents.hitsVMMChPerPlaneQuad[ iQuadPerFtt ] = new TH2D( TSF("hitsVMMChPerPlaneQuad%d", iQuadPerFtt), TSF("sTGC (Plane %d, Quadrant %s); VMM; Channel", iPlane + 1, quadLabels[iQuad].c_str()), nVMMPerQuad,0.5, nVMMPerQuad + 0.5, nChPerVMM, 0.5, nChPerVMM + 0.5);
                contents.adcVMMChPerPlaneQuad[ iQuadPerFtt ]  = new TH2D( TSF("adcVMMChPerPlaneQuad%d", iQuadPerFtt), TSF("sTGC (Plane %d, Quadrant %s) ADC weighted; VMM; Channel", iPlane + 1, quadLabels[iQuad].c_str()), nVMMPerQuad,0.5, nVMMPerQuad + 0.5, nChPerVMM, 0.5, nChPerVMM + 0.5);

                contents.hitsPerQuad->GetXaxis()->SetBinLabel( iQuadPerFtt+1, TSF( "%d%s", iPlane+1, quadLabels[iQuad].c_str() ) );



                if ( iPlane == 0 ){
                    contents.hitsPerPlaneQuad->GetYaxis()->SetBinLabel( iQuadPerFtt+1, TSF( "%s", quadLabels[iQuad].c_str() ) );
                }
                for ( u_char iFob = 0; iFob < nFobPerQuad; iFob ++ ){
                    int iFobPerFtt = iFob + ( iQuad * nFobPerQuad ) + ( iPlane * nFobPerPlane );
                    // contents.adcChPerFob[ iFobPerFtt ] = new TH2D( TSF( "adcChPerFob%d",iFobPerFtt ), TSF("sTGC (Plane %d, Quadule %d, Fob %d); Channel; ADC;", iPlane+1, iQuad+1, iFob+1), nChPerFob, 0.5, nChPerFob + 0.5, maxADC, 0, maxADC );
                    contents.chargePerFob[iFobPerFtt] = new TH1D( TSF("chargePerFob%d", iFobPerFtt), TSF("sTGC Fob %d (Charge); ADC; counts (hits)", iFobPerFtt+1), maxADC/10.0, 0, maxADC );
                    setLogy.push_back( TSF("chargePerFob%d", iFobPerFtt).Data() );
                }

            }
        }


    ////////////////////////////////////////////////////////////////////////
    // Add root histograms to Plots
    int np = sizeof(contents) / sizeof(TH1 *);
    JevpPlot *plots[np];


    // Add all of the plots
    for ( int i = 0; i < np; i++ ){
        if ( !contents.array[i] ) continue;
        plots[i] = new JevpPlot( contents.array[i] );
        
        // set some special options logy, opt stats
        std::string name = contents.array[i]->GetName();
        if ( std::find( setLogy.begin(), setLogy.end(), name ) != setLogy.end() ) {
            plots[i]->logy=1;
        }
        if ( std::find( setLogz.begin(), setLogz.end(), name ) != setLogz.end() ) {
            plots[i]->optlogz=1;
        }
        plots[i]->optstat=0;
        if ( std::find( showStats.begin(), showStats.end(), name ) != showStats.end() ) {
            plots[i]->optstat=111;
        }
        if ( contents.array[i]->GetZaxis())
            contents.array[i]->GetZaxis()->SetLabelSize(10.0 / 360.0);

        addPlot(plots[i]);
    }

    ////////////////////////////////////////////////////////////////////////
    // Set up the mapping for the VMM electronics
    ////////////////////////////////////////////////////////////////////////
    mHardwareMap = std::make_shared<VMMHardwareMap>();
    
} // initialize
    
void fttBuilder::startrun(daqReader *rdr) {
    resetAllPlots();
    // Set the "time" window for accepting data
    ((daq_stgc *)rdr->det("stgc"))->xing_min = -65000 ;
    ((daq_stgc *)rdr->det("stgc"))->xing_max = 65000 ;

    // Draw plane outlines
    for ( int iPlane = 0; iPlane < nPlane; iPlane++ ){
        drawOutline( contents.hStripPerPlane[ iPlane ], VMMHardwareMap::StripOrientation::Horizontal );
        drawOutline( contents.vStripPerPlane[ iPlane ], VMMHardwareMap::StripOrientation::Vertical );
    }

    // reload the map every run for fast updates
    mHardwareMap->loadMap( string(confdatadir)+"/ftt/vmm_map.dat" );
}

void fttBuilder::stoprun(daqReader *rdr) {
}


void fttBuilder::drawStrip( TH2 * h2, int row, int strip, VMMHardwareMap::Quadrant q, VMMHardwareMap::StripOrientation so ){

    double x0 = 0;
    double y0 = 0;

    if ( VMMHardwareMap::Quadrant::B == q ){
        x0 = 60;
        y0 = 0;
    } else if ( VMMHardwareMap::Quadrant::C == q ){
        x0 = -60;
        y0 = 0;
    } else if ( VMMHardwareMap::Quadrant::D == q ){
        x0 = 0;
        y0 = 0;
    }

    TAxis *ax = h2->GetXaxis();
    TAxis *ay = h2->GetYaxis();

    const double rLength = VMMHardwareMap::rowLength; 
    const double sPitch = VMMHardwareMap::stripPitch;

    if ( VMMHardwareMap::StripOrientation::Horizontal == so ){
        if ( VMMHardwareMap::Quadrant::B == q ){
            strip = -strip-1;
        } else if ( VMMHardwareMap::Quadrant::C == q ){
            row = -row -1;
            strip = -strip-1;
        } else if ( VMMHardwareMap::Quadrant::D == q ){
            row = -row-1;
        }

        int ix0 = ax->FindBin( x0 + row * rLength );
        int ix1 = ax->FindBin( x0 + (row + 1) * rLength - 1 );
        if ( VMMHardwareMap::Quadrant::C == q || VMMHardwareMap::Quadrant::D == q ){
            int ix0 = ax->FindBin( x0 + (row - 1) * rLength );
            int ix1 = ax->FindBin( x0 + (row) * rLength - 1 );
        }
        const int iy0 = ay->FindBin( y0 + strip * sPitch );
        const int iy1 = ay->FindBin( y0 + (strip) * sPitch );
        floodFill( h2, ix0, iy0, ix1, iy1 );
    } else if ( VMMHardwareMap::StripOrientation::Vertical == so ){
        
        if ( VMMHardwareMap::Quadrant::B == q ){
            row = -row-1;
        } else if ( VMMHardwareMap::Quadrant::C == q ){
            strip = -strip -1;
            row = -row-1;
        } else if ( VMMHardwareMap::Quadrant::D == q ){
            strip = -strip-1;
        }

        int iy0 = ay->FindBin( y0 + row * rLength );
        int iy1 = ay->FindBin( y0 + (row + 1) * rLength - 1 );
        if ( VMMHardwareMap::Quadrant::C == q || VMMHardwareMap::Quadrant::D == q ){
            int iy0 = ay->FindBin( y0 + (row - 1) * rLength );
            int iy1 = ay->FindBin( y0 + (row) * rLength - 1 );
        }
        const int ix0 = ax->FindBin( x0 + strip * sPitch );
        const int ix1 = ax->FindBin( x0 + (strip) * sPitch );
        floodFill( h2, ix0, iy0, ix1, iy1 );
    } else if ( VMMHardwareMap::StripOrientation::Diagonal == so ){
        double l0 = VMMHardwareMap::stripPitch * 5;
        double l = l0 + VMMHardwareMap::stripPitch * strip;
        double x0 = l0 / sqrt(2);
        double y0 = x0;
    }
} // drawStrip


void fttBuilder::drawOutline( TH2 * h2, VMMHardwareMap::StripOrientation so ) {
    
    double val = 1e-6;
    for ( int ix = 1; ix < h2->GetNbinsX() + 1; ix++ ){
        for ( int iy = 1; iy < h2->GetNbinsY() + 1; iy++ ){
            float x = h2->GetXaxis()->GetBinCenter( ix );
            float y = h2->GetYaxis()->GetBinCenter( iy );


            if ( so == VMMHardwareMap::StripOrientation::Horizontal ){
                int hStrip = 1000;
                int hRow = 100;
                
                hStrip = y / VMMHardwareMap::stripPitch;
                if ( y > 0 )
                    hRow = x / VMMHardwareMap::rowLength;
                else if ( x > 0 )
                    hRow = ( x - 60 ) / VMMHardwareMap::rowLength;
                else if ( x < 0 )
                    hRow = ( x + 60 ) / VMMHardwareMap::rowLength;
                
                if ( hRow == 0 && abs(hStrip) > 166 )
                    h2->SetBinContent( ix, iy, val );
                if ( abs(hRow) == 1 && abs(hStrip) > 152 )
                    h2->SetBinContent( ix, iy, val );
                if ( abs(hRow) == 2 && abs(hStrip) > 93 )
                    h2->SetBinContent( ix, iy, val );
                if ( abs(hRow) > 2 || abs(hStrip) > 166 )
                    h2->SetBinContent( ix, iy, val );
                if ( y < 0 && x > -60 && x < 60 )
                    h2->SetBinContent( ix, iy, val );
            }

            if ( so == VMMHardwareMap::StripOrientation::Vertical ){
                int vStrip = 1000;
                int vRow = 100;
                
                if ( y > 0 )
                    vStrip = x / VMMHardwareMap::stripPitch;
                else if( x > 0 )
                    vStrip = (x-60) / VMMHardwareMap::stripPitch;
                else if( x < 0 )
                    vStrip = (x+60) / VMMHardwareMap::stripPitch;

                vRow = y / VMMHardwareMap::rowLength;
                
                if ( vRow == 0 && abs(vStrip) > 166 )
                    h2->SetBinContent( ix, iy, val );
                if ( abs(vRow) == 1 && abs(vStrip) > 152 )
                    h2->SetBinContent( ix, iy, val );
                if ( abs(vRow) == 2 && abs(vStrip) > 93 )
                    h2->SetBinContent( ix, iy, val );
                if ( abs(vRow) > 2 || abs(vStrip) > 166 )
                    h2->SetBinContent( ix, iy, val );
                if ( y < 0 && x > -60 && x < 60 )
                    h2->SetBinContent( ix, iy, val );
            }

        }
    }
} // drawOutline

void fttBuilder::processVMMHit( int iPlane, VMMHardwareMap::Quadrant quad, stgc_vmm_t rawVMM ){

    int iQuad = (int)quad;
    int thePlane = iPlane + 1;
    int theQuad = iQuad + 1;
    int iQuadPerFtt = iQuad + ( iPlane * nQuadPerPlane );
    

    int iFob = rawVMM.feb_vmm >> 2 ;   // feb [0..5]
    int theFob = iFob + 1;
    int iVMM = rawVMM.feb_vmm & 3 ;    // VMM [0..3]
    int theVMM = iVMM + 1;
    int iCh = rawVMM.ch;
    int theCh = iCh + 1;


    // This is the "FOB" according to Prashanth's map
    size_t iFobPerFtt = iFob + ( iQuad * nFobPerQuad ) + ( iPlane * nFobPerPlane );

    size_t iVMMPerQuad  = iVMM + ( iFob * nVMMPerFob);
    size_t iVMMPerPlane = iVMMPerQuad + ( iQuad * nVMMPerQuad );
    size_t iVMMPerFtt  = iVMMPerPlane + ( iPlane * nVMMPerPlane ); // global VMM index

    size_t iChPerFob   = iCh + ( iVMM * nChPerVMM );

    // global counter on strips fired
    nStripsFired++;

    // count hits per
    contents.hitsPerPlane->Fill( thePlane ); // disk
    contents.hitsPerQuad->Fill( iQuadPerFtt+1 ); // quad index
    contents.hitsPerFob->Fill( iFobPerFtt+1 ); // Fob index
    contents.hitsPerVMM->Fill( iVMMPerFtt+1 ); // VMM index
    contents.hitsPerTb->Fill( rawVMM.tb );
    contents.hitsPerTb400->Fill( rawVMM.tb );
    contents.hitsPerPlaneQuad->Fill( thePlane, theQuad ); // 2D Quadule vs. Plane
    contents.hitsPerVMMPlane->Fill( iVMMPerPlane+1, iPlane + 1 );

    contents.hitsVMMPerPlane[ iPlane ]->Fill( iVMMPerPlane + 1 );
    contents.hitsFobQuadPerPlane[ iPlane ]->Fill( theFob, theQuad );
    contents.hitsTbPerPlane[ iPlane ] ->Fill( rawVMM.tb );
    contents.chargePerPlane[ iPlane ] ->Fill( rawVMM.adc );
    contents.chargePerFob[iFobPerFtt] ->Fill( rawVMM.adc );


    contents.hitsVMMChPerPlaneQuad[ iQuadPerFtt % nQuad ]->Fill( iVMMPerQuad + 1, theCh );
    contents.adcVMMChPerPlaneQuad[ iQuadPerFtt % nQuad ]->Fill( iVMMPerQuad + 1, theCh, rawVMM.adc );
    if ( iQuadPerFtt >= 16 ){
        printf( "iQuadPerFtt = %d, iQuad=%d, iPlane=%d", (int) iQuadPerFtt, (int)iQuad, (int)iPlane );
    }

    // contents.adcChPerFob[ iFobPerFtt ]->Fill( iChPerFob+1, rawVMM.adc );
    contents.adcVMM->Fill( iVMMPerFtt+1, rawVMM.adc );
    contents.bcidVMM->Fill( iVMMPerFtt+1, rawVMM.bcid );



    int iRow = -1;
    int iStrip = -1;
    VMMHardwareMap::StripOrientation stripDir;
    mHardwareMap->get( /*rob=*/iQuadPerFtt+1, iFob+1, iVMM+1, iCh, iRow, iStrip, stripDir );

    if ( (iRow == -1 || iStrip == -1) ){
        // printf( "\n\n------>START\n" );
        // printf( "ROB=%d, FOB=%d, VMM=%d, CH=%d", iQuadPerFtt+1, iFob+1, iVMM+1, iCh ); puts("");
        // printf( "Row=%d, Strip=%d, dir=%s\n", iRow, iStrip, dirLabels[(int)stripDir].c_str() );
        // printf( "<------STOP\n\n" );
    } else {
        if ( VMMHardwareMap::StripOrientation::Horizontal == stripDir ){
            drawStrip(contents.hStripPerPlane[ iPlane ], iRow, iStrip, quad, stripDir );
        }
        else if ( VMMHardwareMap::StripOrientation::Vertical == stripDir ){
            drawStrip(contents.vStripPerPlane[ iPlane ], iRow, iStrip, quad, stripDir );
        }
        else if ( VMMHardwareMap::StripOrientation::Diagonal == stripDir ){
            drawStrip(contents.dStripPerPlane[ iPlane ], iRow, iStrip, quad, stripDir );
        }
    }

    // outline detector
    // drawStrip(contents.hStripPerPlane[ 0 ], 0, 167, VMMHardwareMap::Quadrant::A, VMMHardwareMap::StripOrientation::Horizontal );
    // drawStrip(contents.hStripPerPlane[ 0 ], 1, 153, VMMHardwareMap::Quadrant::A, VMMHardwareMap::StripOrientation::Horizontal );
    // drawStrip(contents.hStripPerPlane[ 0 ], 2, 94,  VMMHardwareMap::Quadrant::A, VMMHardwareMap::StripOrientation::Horizontal );
    // for ( int i = 0; i < 300; i++ ){
    //     drawStrip(contents.hStripPerPlane[ 0 ], 3, i,  VMMHardwareMap::Quadrant::A, VMMHardwareMap::StripOrientation::Horizontal );
    //     if ( i > 93 )
    //         drawStrip(contents.hStripPerPlane[ 0 ], 2, i,  VMMHardwareMap::Quadrant::A, VMMHardwareMap::StripOrientation::Horizontal );
    //     if ( i > 152 )
    //         drawStrip(contents.hStripPerPlane[ 0 ], 1, i,  VMMHardwareMap::Quadrant::A, VMMHardwareMap::StripOrientation::Horizontal );
    //     if ( i > 166 )
    //         drawStrip(contents.hStripPerPlane[ 0 ], 0, i,  VMMHardwareMap::Quadrant::A, VMMHardwareMap::StripOrientation::Horizontal );
    // }

    // drawStrip(contents.hStripPerPlane[ 0 ], 0, 167, VMMHardwareMap::Quadrant::B, VMMHardwareMap::StripOrientation::Horizontal );
    // drawStrip(contents.hStripPerPlane[ 0 ], 1, 153, VMMHardwareMap::Quadrant::B, VMMHardwareMap::StripOrientation::Horizontal );
    // drawStrip(contents.hStripPerPlane[ 0 ], 2, 94,  VMMHardwareMap::Quadrant::B, VMMHardwareMap::StripOrientation::Horizontal );

    // drawOutline( contents.hStripPerPlane[ 0 ] );
}

void fttBuilder::processVMM(daqReader *rdr) {
    daq_dta *dd = nullptr;
    dd = rdr->det("stgc")->get("vmm");

    nStripsFired = 0;

    int vmm0bcid = -1;
    int vmm3bcid = -1;

    bool vmm_found = false;
    while(dd && dd->iterate()) {    
        vmm_found = true ;

        struct stgc_vmm_t *vmm = (stgc_vmm_t *)dd->Void ;
        // Zero index to disk, module
        u_char iPlane = dd->sec - 1;
        u_char iQuad = dd->rdo - 1;

        // loop over the hits
        for(uint32_t iHit=0; iHit<dd->ncontent; iHit++) {
            processVMMHit( iPlane, (VMMHardwareMap::Quadrant)iQuad, vmm[iHit] );
        } // Loop over iHit
    } // iterate dd

    contents.nStripsFired->Fill( nStripsFired );
} // processVMM

void fttBuilder::event(daqReader *rdr) {
    LOG(DBG, "-------> START EVENT, #%d",rdr->seq);
    processVMM(rdr);
}

void fttBuilder::main(int argc, char *argv[])
{
    fttBuilder me;  
    me.Main(argc, argv);
}
