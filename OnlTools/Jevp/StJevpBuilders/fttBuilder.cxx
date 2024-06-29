#include <stdio.h>
#include <stdlib.h>
#include <fstream>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h>
#include <DAQ_STGC/daq_stgc.h>
#include "Jevp/StJevpPlot/RunStatus.h"
#include "Jevp/StJevpPlot/ImageWriter.h"
#include <TH1D.h>
#include <TH2F.h>
#include <TString.h>
#include <TF1.h>
#include <TColor.h>

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
const double fttBuilder::PENT_LS = 60.2361 * 10; // mm
const double fttBuilder::PENT_SS = 0.308571429 * 60.2361 * 10; // mm


void fttBuilder::initialize(int argc, char *argv[]) {
    

    // Control draw/visiblity options
    std::vector<std::string> setLogy   = { "hitsPerTb", "chargePerPlane0", "chargePerPlane1", "chargePerPlane2", "chargePerPlane3", "hitsTbPerPlane0", "hitsTbPerPlane1", "hitsTbPerPlane2", "hitsTbPerPlane3", "hitsPerPlane", "hitsPerQuad", "hitsPerFob", "hitsPerVMM", "hitsVMMPerPlane0", "hitsVMMPerPlane1", "hitsVMMPerPlane2", "hitsVMMPerPlane3", "nStripsFired", "nStripsFiredAll", "nStripsFiredOutOfTime" };
    std::vector<std::string> setLogz   = { "hitsPerPlaneQuad", "hitsFobQuadPerPlane0", "hitsFobQuadPerPlane1", "hitsFobQuadPerPlane2", "hitsFobQuadPerPlane3" };
    std::vector<std::string> showStats = { "hitsPerTb400", "hitsPerTb100", "nStripsFired" };
    std::vector<std::string> drawOutline = {};
    std::vector<std::string> drawOutlineDH = {};
    std::vector<std::string> drawOutlineDV = {};

    //////////////////////////////////////////////////////////////////////// 
    // General
    ////////////////////////////////////////////////////////////////////////
        contents.nStripsFired            = new TH1D( "nStripsFired", "sTGC; nStripsFired (-40 < tb < 300); counts", 300, 0, 15000 );
        contents.nStripsFiredAll         = new TH1D( "nStripsFiredAll", "sTGC; nStripsFired (all tb); counts", 300, 0, 15000 );
        contents.nStripsFiredOutOfTime   = new TH1D( "nStripsFiredOutOfTime", "sTGC; nStripsFiredOutOfTime (tb < -40 | tb > 300); counts", 200, 0, 10000 );

    //////////////////////////////////////////////////////////////////////// 
    // hits and adc
    ////////////////////////////////////////////////////////////////////////
        contents.hitsPerPlane             = new TH1D( "hitsPerPlane", "sTGC (hits / Plane); Plane; counts (hits)", nPlane,0.5, nPlane + 0.5 );
        contents.hitsPerQuad              = new TH1D( "hitsPerQuad", "sTGC (hits / Quadrant); Plane & Quadrant; counts (hits)", nQuad,0.5, nQuad + 0.5 );
        contents.hitsPerFob               = new TH1D( "hitsPerFob", "sTGC (hits / Fob); Fob; counts (hits)", nFob,0.5, nFob + 0.5 );
        contents.hitsPerVMM               = new TH1D( "hitsPerVMM", "sTGC (hits / VMM); VMM Index (96VMM / Plane); counts (hits)", nVMM,0.5, nVMM + 0.5 );
        contents.hitsPerTb                = new TH1D( "hitsPerTb", "sTGC (hits / Timebin); Tb; counts (hits)", 338, minTb, maxTb );
        contents.hitsPerTb400             = new TH1D( "hitsPerTb400", "sTGC (hits / Timebin); Tb; counts (hits)", 400, -200, 600 );
        contents.hitsPerTb100             = new TH1D( "hitsPerTb100", "sTGC (hits / Timebin); Tb; counts (hits)", 150, -100, 50 );
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
            contents.dhStripPerPlane[iPlane]        = new TH2D( TSF("dhStripPerPlane%d", iPlane), TSF("sTGC Diagonal Strips, Plane %d (hits / Strip); x; y", iPlane+1), 350, -700, 700, 350, -700, 700 );
            contents.dvStripPerPlane[iPlane]        = new TH2D( TSF("dvStripPerPlane%d", iPlane), TSF("sTGC Diagonal Strips, Plane %d (hits / Strip); x; y", iPlane+1), 350, -700, 700, 350, -700, 700 );

            setLogz.push_back( TSF("hStripPerPlane%d", iPlane).Data() );
            setLogz.push_back( TSF("vStripPerPlane%d", iPlane).Data() );
            setLogz.push_back( TSF("dhStripPerPlane%d", iPlane).Data() );
            setLogz.push_back( TSF("dvStripPerPlane%d", iPlane).Data() );

            drawOutline.push_back( TSF("hStripPerPlane%d", iPlane).Data() );
            drawOutline.push_back( TSF("vStripPerPlane%d", iPlane).Data() );
            // drawOutline.push_back( TSF("dhStripPerPlane%d", iPlane).Data() );
            // drawOutline.push_back( TSF("dvStripPerPlane%d", iPlane).Data() );
            drawOutlineDH.push_back( TSF("dhStripPerPlane%d", iPlane).Data() );
            drawOutlineDV.push_back( TSF("dvStripPerPlane%d", iPlane).Data() );

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

    std::vector< JLine* > lines;


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
            plots[i]->optstat=1111;
        }
        if ( contents.array[i]->GetZaxis())
            contents.array[i]->GetZaxis()->SetLabelSize(10.0 / 360.0);

        contents.array[i]->SetFillColor( TColor::GetColor("#ffd600") );

        lines.clear();
        // Draw the detector outline
        if ( std::find( drawOutline.begin(), drawOutline.end(), name ) != drawOutline.end() ){
            
            JLine* 
            l = jLine(0.0, PENT_LS, PENT_SS, PENT_LS); lines.push_back( l );
            l = jLine(PENT_SS, PENT_LS, PENT_LS, PENT_SS); lines.push_back( l );
            l = jLine(PENT_LS, PENT_SS, PENT_LS, 0); lines.push_back( l );

            l = jLine(0.0, PENT_LS, -PENT_SS, PENT_LS); lines.push_back( l );
            l = jLine(-PENT_SS, PENT_LS, -PENT_LS, PENT_SS); lines.push_back( l );
            l = jLine(-PENT_LS, PENT_SS, -PENT_LS, 0); lines.push_back( l );

            l = jLine(-60.0, 0, 60.0, 0); lines.push_back( l );
            l = jLine(-60.0, 0, -60, -PENT_LS); lines.push_back( l );
            l = jLine(60.0, -PENT_LS, 60.0, 0); lines.push_back( l );

            l = jLine(60.0, -PENT_LS, PENT_SS + 60.0, -PENT_LS); lines.push_back( l );
            l = jLine(60.0 + PENT_SS, -PENT_LS, 60.0 + PENT_LS, -PENT_SS); lines.push_back( l );
            l = jLine(60.0 + PENT_LS, -PENT_SS, 60.0 + PENT_LS, 0); lines.push_back( l );

            l = jLine(-60.0, -PENT_LS, -PENT_SS - 60.0, -PENT_LS); lines.push_back( l );
            l = jLine(-60.0 - PENT_SS, -PENT_LS, -60.0 - PENT_LS, -PENT_SS); lines.push_back( l );
            l = jLine(-60.0 - PENT_LS, -PENT_SS, -60.0 - PENT_LS, 0); lines.push_back( l );

            l = jLine(PENT_LS, 0, 60.0 + PENT_LS, 0); lines.push_back( l );
            l = jLine(-PENT_LS, 0, -60.0 - PENT_LS, 0); lines.push_back( l );

            l = jLine(0, 0, 0, PENT_LS); lines.push_back( l );
            l = jLine(0, 0, PENT_LS, 0); lines.push_back( l );
            l = jLine(0, 0, -PENT_LS, 0); lines.push_back( l );
        }
        if ( std::find( drawOutlineDH.begin(), drawOutlineDH.end(), name ) != drawOutlineDH.end() ){
            JLine* 
            l = jLine(0, PENT_LS, -PENT_SS, PENT_LS); lines.push_back( l );
            l = jLine( -PENT_SS, PENT_LS, -399.177, 388.718); lines.push_back( l );
            l = jLine(-399.177, 388.718, -162.14, 150.769); lines.push_back( l );
            l = jLine(-162.14, 150.769, -306.996, 0); lines.push_back( l );
            l = jLine(-306.996, 0, -384.362, 0); lines.push_back( l );
            l = jLine(-384.362, 0, -219.753, -161.026); lines.push_back( l );
            l = jLine(-219.753, -161.026, -456.79, -386.667); lines.push_back( l );

            l = jLine(-456.79, -386.667, -60 - PENT_SS, -PENT_LS); lines.push_back( l );
            l = jLine(-60 - PENT_SS, -PENT_LS, -60.0, -PENT_LS); lines.push_back( l );
            

            l = jLine(0, PENT_LS, PENT_SS, PENT_LS); lines.push_back( l );
            l = jLine(PENT_SS, PENT_LS, 399.177,  388.718); lines.push_back( l );
            l = jLine(399.177,  388.718,  162.14, 150.769); lines.push_back( l );
            l = jLine(162.14 ,  150.769, 306.996, 0); lines.push_back( l );
            l = jLine(306.996,        0, 384.362, 0); lines.push_back( l );
            l = jLine(384.362,        0, 219.753, -161.026); lines.push_back( l );
            l = jLine(219.753, -161.026,  456.79, -386.667); lines.push_back( l );
            l = jLine(456.79, -386.667, 60 + PENT_SS, -PENT_LS); lines.push_back( l );
            l = jLine(60 + PENT_SS, -PENT_LS, 60.0, -PENT_LS); lines.push_back( l );

            l = jLine(-60.0, 0, 60.0, 0); lines.push_back( l );
            l = jLine(-60.0, 0, -60, -PENT_LS); lines.push_back( l );
            l = jLine(60.0, -PENT_LS, 60.0, 0); lines.push_back( l );

            l = jLine(0.0, 0.0, 0.0, PENT_LS); lines.push_back( l );
            l = jLine(-386.448, 0.0, 386.448, 0.0 ); lines.push_back( l );
        }

        if ( std::find( drawOutlineDV.begin(), drawOutlineDV.end(), name ) != drawOutlineDV.end() ){
            JLine* 
            l = jLine(-PENT_LS, 0, -PENT_LS, PENT_SS); lines.push_back( l );
            l = jLine(-PENT_LS, PENT_SS, -388.09, 401.882); lines.push_back( l );
            l = jLine(-388.09, 401.882, -154.004, 160.65); lines.push_back( l );
            l = jLine(-154.004, 160.65, 0.0, 317.707); lines.push_back( l );
            l = jLine(0.0, 317.707, 154.004, 160.65); lines.push_back( l );
            l = jLine(154.004, 160.65, 386.448, 401.882); lines.push_back( l );
            l = jLine(386.448, 401.882, PENT_LS, PENT_SS); lines.push_back( l );
            l = jLine(PENT_LS, PENT_SS, PENT_LS, 0); lines.push_back( l );

            l = jLine(-60 - PENT_LS, 0, -60 - PENT_LS, -PENT_SS); lines.push_back( l );
            l = jLine(-60 - PENT_LS, -PENT_SS, -448.871, -397.776); lines.push_back( l );
            l = jLine(-448.871, -397.776, -214.784, -160.65); lines.push_back( l );
            l = jLine(-214.784, -160.65, -60, -312.575); lines.push_back( l );
            
            l = jLine(PENT_LS + 60, 0, PENT_LS + 60, -PENT_SS); lines.push_back( l );
            l = jLine(PENT_LS + 60, -PENT_SS, 448.871, -397.776); lines.push_back( l );
            l = jLine(448.871, -397.776, 214.784, -160.65); lines.push_back( l );
            l = jLine(214.784, -160.65, 60, -312.575); lines.push_back( l );

            l = jLine(PENT_LS, 0, PENT_LS + 60, 0); lines.push_back( l );
            l = jLine(-PENT_LS, 0, -PENT_LS - 60, 0); lines.push_back( l );


            l = jLine(-60.0, 0, 60.0, 0); lines.push_back( l );
            l = jLine(-60.0, 0, -60, -312.575); lines.push_back( l );
            l = jLine(60.0, -312.575, 60.0, 0); lines.push_back( l );

            l = jLine(-PENT_LS, 0, PENT_LS, 0); lines.push_back( l );
            l = jLine(0, 0, 0, 317.707); lines.push_back( l );
            
        }

        for ( auto l : lines ){
            plots[i]->addElement(l);
        }

        addPlot(plots[i]);
    }

    ////////////////////////////////////////////////////////////////////////
    // Set up the mapping for the VMM electronics
    ////////////////////////////////////////////////////////////////////////
    mHardwareMap = std::make_shared<VMMHardwareMap>();
    
} // initialize
    
void fttBuilder::startrun(daqReader *rdr) {
    LOG("JEFF", "fttBuilder starting run");
    resetAllPlots();
    // Set the "time" window for accepting data
    int tCut = 65000;
    ((daq_stgc *)rdr->det("stgc"))->xing_min = -tCut ;
    ((daq_stgc *)rdr->det("stgc"))->xing_max = tCut ;

    // Draw plane outlines
    for ( int iPlane = 0; iPlane < nPlane; iPlane++ ){
        // drawOutline( contents.hStripPerPlane[ iPlane ], VMMHardwareMap::StripOrientation::Horizontal );
        // drawOutline( contents.vStripPerPlane[ iPlane ], VMMHardwareMap::StripOrientation::Vertical );
        // drawOutline( contents.dhStripPerPlane[ iPlane ], VMMHardwareMap::StripOrientation::DiagonalH );
        // drawOutline( contents.dvStripPerPlane[ iPlane ], VMMHardwareMap::StripOrientation::DiagonalV );
    }

    // reload the map every run for fast updates
    mHardwareMap->loadMap( string(confdatadir)+"/ftt/vmm_map.dat" );
}

void fttBuilder::stoprun(daqReader *rdr) {
    // PCP;
    // fitTriggerTime(false);
    // PCP;
}


JLine * fttBuilder::jLine( double x0, double y0, double x1, double y1 ){
    JLine *l = new JLine( x0, y0, x1, y1 );
    l->SetLineColor(kGreen);
    l->SetLineWidth(2.0);
    l->SetNDC_y( 0 );
    return l;
}

void fttBuilder::fillPoint( TH2 * h2, float x, float y, float w ){
    TAxis *ax = h2->GetXaxis();
    TAxis *ay = h2->GetYaxis();
    int ix0 = ax->FindBin( x );
    int iy0 = ay->FindBin( y );
    h2->SetBinContent( ix0, iy0, h2->GetBinContent( ix0, iy0 ) + w );
}
void fttBuilder::fillLineLow( TH2 * h2, float x0, float y0, float x1, float y1 ) {// plotLineLow(x0, y0, x1, y1)
    float dx0 = 1.598173516*2;
    float dy0 = 1.598173516*2;

    float dx = x1 - x0;
    float dy = y1 - y0;
    float yi = 1;
    yi = dy0;
    if (dy < 0){
        yi = -1;
        yi = -dy0;
        dy = -dy;
    }
    float D = (2 * dy) - dx;
    float y = y0;

    // for x from x0 to x1
    for ( float x = x0; x < x1; x+=dx0 ){
        if ( ((fabs( x ) < 660.0 && y < 0 ) || ( fabs( x ) < 600.0 && y > 0 )) && fabs( y ) < 600.0 )
            fillPoint( h2, x, y );
        if (D > 0){
            y = y + yi;
            D = D + (2 * (dy - dx));
        } else {
            D = D + 2*dy;
        }
    }
}
void fttBuilder::fillLineHigh( TH2 * h2, float x0, float y0, float x1, float y1 ) {
    float dx0 = 1.598173516*2;
    float dy0 = 1.598173516*2;

    float dx = x1 - x0;
    float dy = y1 - y0;
    float xi = 1;
    xi = dx0;
    if (dx < 0){
        xi = -1;
        xi = -dx0;
        dx = -dx;
    }
    float D = (2 * dx) - dy;
    float x = x0;

    // for y from y0 to y1
    for ( float y = y0; y < y1; y+=dy0 ){
        if ( ((fabs( x ) < 660.0 && y < 0 ) || ( fabs( x ) < 600.0 && y > 0 )) && fabs( y ) < 600.0 )
            fillPoint( h2, x, y );
        if (D > 0){
            x = x + xi;
            D = D + (2 * (dx - dy));
        } else {
            D = D + 2*dx;
        }
    }
}
void fttBuilder::fillLine( TH2 * h2, float x0, float y0, float x1, float y1 ) {
    
    const float d = 1.598173516;
    const float nudge = 0;
    int nx0 = (x0 / d);
    x0 = nx0 * d;
    int nx1 = (x1 / d);
    x1 = nx1 * d;

    int ny0 = (y0 / d);
    y0 = ny0 * d;
    int ny1 = (y1 / d);
    y1 = ny1 * d;

    if (abs(y1 - y0) < abs(x1 - x0)) {
        if (x0 > x1) {
            fillLineLow(h2, x1+nudge, y1+nudge, x0-nudge, y0-nudge);
        } else {
            fillLineLow(h2, x0+nudge, y0+nudge, x1-nudge, y1-nudge);
        } 
    } else {
        if (y0 > y1){
            fillLineHigh(h2, x1+nudge, y1+nudge, x0-nudge, y0-nudge);
        } else {
            fillLineHigh(h2, x0+nudge, y0+nudge, x1-nudge, y1-nudge);
        }
    }
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

	/*   This code does nothing! ----------------------
	     Most likely it was intended to adjust the indexes for this specific
	     region of the detector, but not known if needed;   -jml 3/25/24

        if ( VMMHardwareMap::Quadrant::C == q || VMMHardwareMap::Quadrant::D == q ){
            int ix0 = ax->FindBin( x0 + (row - 1) * rLength );
            int ix1 = ax->FindBin( x0 + (row) * rLength - 1 );
        }
	*/

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
	/*   Does nothing, intened to adjust indexes?
        if ( VMMHardwareMap::Quadrant::C == q || VMMHardwareMap::Quadrant::D == q ){
            int iy0 = ay->FindBin( y0 + (row - 1) * rLength );
            int iy1 = ay->FindBin( y0 + (row) * rLength - 1 );
        }
	*/
        const int ix0 = ax->FindBin( x0 + strip * sPitch );
        const int ix1 = ax->FindBin( x0 + (strip) * sPitch );
        floodFill( h2, ix0, iy0, ix1, iy1 );
    } else if ( VMMHardwareMap::StripOrientation::DiagonalH == so || VMMHardwareMap::StripOrientation::DiagonalV == so ){
        // printf( "Diagonal @ row=%d, strip=%d\n", row, strip );
        double l0 = VMMHardwareMap::stripPitch * 5;
        double l = l0 + VMMHardwareMap::stripPitch * (strip) * (1.10);
        double x1, y1;

        if ( VMMHardwareMap::Quadrant::A == q ){
            x0 = l / sqrt(2);
            y0 = x0;
            x1 = x0*2;
            y1 = 0;
            if ( row == 3 && so == VMMHardwareMap::StripOrientation::DiagonalH){
                x1 = 0;
                y1 = y0*2;
            } else if ( row == 4 && so == VMMHardwareMap::StripOrientation::DiagonalV){
                x1 = 0;
                y1 = y0*2;
            }

            fillLine( h2, x0, y0, x1, y1 );
        } 
        else if ( VMMHardwareMap::Quadrant::B == q )
        {
            x0 = l / sqrt(2);
            y0 = -x0;

            x1 = x0*2;
            y1 = 0;
            if ( row == 3 && so == VMMHardwareMap::StripOrientation::DiagonalH){
                x1 = 0;
                y1 = y0*2;
            } else if ( row == 4 && so == VMMHardwareMap::StripOrientation::DiagonalV){
                x1 = 0;
                y1 = y0*2;
            }

            x0 += 60;
            x1 += 60;

            fillLine( h2, x0, y0, x1, y1 );
        } 
        else if ( VMMHardwareMap::Quadrant::C == q ){
            x0 = -l / sqrt(2);
            y0 = x0;

            x1 = x0*2;
            y1 = 0;
            if ( row == 3 && so == VMMHardwareMap::StripOrientation::DiagonalH ){
                x1 = 0;
                y1 = y0*2;
            } else if ( row == 4 && so == VMMHardwareMap::StripOrientation::DiagonalV ){
                x1 = 0;
                y1 = y0*2;
            }

            x0 -= 60;
            x1 -= 60;

            fillLine( h2, x0, y0, x1, y1 );
        } else if ( VMMHardwareMap::Quadrant::D == q ){
            x0 = -l / sqrt(2);
            y0 = fabs(x0);

            x1 = x0*2;
            y1 = 0;
            if ( row == 3 && so == VMMHardwareMap::StripOrientation::DiagonalH ){
                x1 = 0;
                y1 = y0*2;
            } else if ( row == 4 && so == VMMHardwareMap::StripOrientation::DiagonalV ){
                x1 = 0;
                y1 = y0*2;
            }

            fillLine( h2, x0, y0, x1, y1 );
        }

        
    }
} // drawStrip



void fttBuilder::fitTriggerTime(bool protect){
    
    // The fit is not local in root, so this causes segmentation faults
    // if occuring at the same time as any histograms plotting
    // prevent this.
    //
    // protect is on by default,  
    // in stoprun, the mutex is already owned, so turn it off
    //
    if(protect && parent && parent->imageWriter) {
	if(pthread_mutex_trylock(&parent->imageWriter->mux) != 0) {
	    LOG(DBG, "Can't take imageWriter mutex");
	    updateTimeFit = 0;  // don't try again right away!
	    return;
	}
    }
    PCP;

    if ( nullptr == f1TriggerTime )
        f1TriggerTime = new TF1( "fg", "gaus" );

    f1TriggerTime->SetLineColor( kRed );

    int ix = contents.hitsPerTb100->GetMaximumBin();
    float x = contents.hitsPerTb100->GetXaxis()->GetBinCenter( ix );

    contents.hitsPerTb100->Fit( f1TriggerTime, "RQ", "", x - 60, x + 60 );
    float m = f1TriggerTime->GetParameter(1);
    float s = f1TriggerTime->GetParameter(2);
    contents.hitsPerTb100->Fit( f1TriggerTime, "RQ", "", m - s*3, m + s*3 );
    m = f1TriggerTime->GetParameter(1);
    s = f1TriggerTime->GetParameter(2);
    contents.hitsPerTb100->Fit( f1TriggerTime, "RQ", "", m - s*2.5, m + s*2.5 );
    updateTimeFit = 0;

    PCP;
    if(protect && parent && parent->imageWriter) {
	pthread_mutex_unlock(&parent->imageWriter->mux);
    }
}

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

    contents.hitsPerTb->Fill( rawVMM.tb );
    contents.hitsPerTb400->Fill( rawVMM.tb );
    contents.hitsPerTb100->Fill( rawVMM.tb );
    contents.hitsTbPerPlane[ iPlane ] ->Fill( rawVMM.tb );


    // count hits per
    contents.hitsPerPlane->Fill( thePlane ); // disk
    contents.hitsPerQuad->Fill( iQuadPerFtt+1 ); // quad index
    contents.hitsPerFob->Fill( iFobPerFtt+1 ); // Fob index
    contents.hitsPerVMM->Fill( iVMMPerFtt+1 ); // VMM index
    
    contents.hitsPerPlaneQuad->Fill( thePlane, theQuad ); // 2D Quadule vs. Plane
    contents.hitsPerVMMPlane->Fill( iVMMPerPlane+1, iPlane + 1 );

    contents.hitsVMMPerPlane[ iPlane ]->Fill( iVMMPerPlane + 1 );
    contents.hitsFobQuadPerPlane[ iPlane ]->Fill( theFob, theQuad );
    
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


    nStripsFiredAll++;
    if ( rawVMM.tb < 300 && rawVMM.tb > -40 ){
        // global counter on strips fired
        nStripsFired++;
    } else {
        nStripsFiredOutOfTime++;
    }


    



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
        else if ( VMMHardwareMap::StripOrientation::DiagonalH == stripDir ){
            drawStrip(contents.dhStripPerPlane[ iPlane ], iRow, iStrip, quad, stripDir );
        }
        else if ( VMMHardwareMap::StripOrientation::DiagonalV == stripDir ){
            drawStrip(contents.dvStripPerPlane[ iPlane ], iRow, iStrip, quad, stripDir );
        }
    }

    // if ( updateTimeFit > fitUpdateInterval ) {
    // PCP;
    //     fitTriggerTime();
    // PCP;
    // }

    updateTimeFit++;
}

void fttBuilder::processVMM(daqReader *rdr) {
    daq_dta *dd = nullptr;
    dd = rdr->det("stgc")->get("vmm");

    nStripsFired = 0;
    nStripsFiredOutOfTime = 0;
    nStripsFiredAll = 0;

    bool vmm_found = false;
    while(dd && dd->iterate()) {    
        vmm_found = true ;

        struct stgc_vmm_t *vmm = (stgc_vmm_t *)dd->Void ;
        // Zero index to disk, module
        u_char iPlane = dd->sec - 1;
        u_char iQuad = dd->rdo - 1;

        // loop over the hits
        for(u_int iHit=0; iHit<dd->ncontent; iHit++) {
            processVMMHit( iPlane, (VMMHardwareMap::Quadrant)iQuad, vmm[iHit] );
        } // Loop over iHit
    } // iterate dd

    // printf( "nStripsFired = %d\n", nStripsFired );
    // printf( "nStripsFiredAll = %d\n", nStripsFiredAll );
    contents.nStripsFired->Fill( nStripsFired );
    contents.nStripsFiredAll->Fill( nStripsFiredAll );
    contents.nStripsFiredOutOfTime->Fill( nStripsFiredOutOfTime );
} // processVMM

void fttBuilder::event(daqReader *rdr) {
    LOG(DBG, "-------> START EVENT, #%d",rdr->seq);
    PCP;
    processVMM(rdr);
    PCP;
}

void fttBuilder::main(int argc, char *argv[])
{
    fttBuilder me;  
    me.Main(argc, argv);
}
