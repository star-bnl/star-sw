//usr/bin/env root -l -b -q  $0'('$1')'; exit $?
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TTree.h"


TFile * fData;
TTree * fwd;
TH2 * hFrame;
TCanvas *gCan;
TPad *padRZ, *padXY, *padStat;

float LegendX, LegendY;
float lineScale = 1.5;

enum ProjectionType { kXY, kRZ, kRZSigned, kXZ, kYZ };

float xx( float x, float y, float z, ProjectionType proj = kRZ ){

    if ( proj == kRZ || proj == kRZSigned){
        return z;//(TMath::ATan2( y, x ) + 2*3.1415926 )/ (2*3.14159) * 360;
    } else if ( proj == kXY ){
        return x;
    } else if ( proj == kXZ ){
        return z;
    } else if ( proj == kYZ ){
        return z;
    }

    return x;
}

float yy( float x, float y, float z, ProjectionType proj = kRZ ){

    if ( proj == kRZ ){
        float r = sqrt( pow(x, 2) + pow(y, 2) );
        return r;
    } else if ( proj == kXY ){
        return y;
    } else if ( proj == kRZSigned ){
        float r = sqrt( pow(x, 2) + pow(y, 2) );
        if ( y == 0 ) return r;
        r *= y / fabs(y);
        return r;
    } else if ( proj == kXZ ){
        return x;
    } else if ( proj == kYZ ){
        return y;
    }

    return y;
}

void viz_points(const char* name, const char* cmd, int color, int eventIndex, ProjectionType projType, bool Legend = false ){

    fwd->Draw( cmd, "", "goff", 1, eventIndex );
    int N = fwd->GetSelectedRows();
    printf( "%s : has %d results \n", cmd, N );
    printf( "Projection Mode : %d \n", projType );

    auto cmdX = fwd->GetV1();
    auto cmdY = fwd->GetV2();
    auto cmdZ = fwd->GetV3();
    auto cmdE = fwd->GetV4();
    if ( cmdE != nullptr ){
        printf( "TOWERS\n" );
    }
    float vizX;   //change from array-AGE
    float vizY;

    TText *t = new TText(.5,.5,"Hello World !");
    // t->SetTextAlign(22);
    t->SetTextColor(kBlack);
    t->SetTextFont(43);
    t->SetTextSize(20);

    int zColorStep = 90;
    int slc = color;
    int zColors[50];  //  fst1 fst2 fst3 ftt1 ftt2 ftt3 ftt4  epd ecal hcal
    float zSizes[] = {2.5, 2.5, 2.0, 1.5, 1.5, 1.5, 1.5, 1.5, 2.5, 2.0, 1.5};   //first element is for hits that don't match any positions  (only goes to ftt3--changing to allow all)
    for ( int i = 0; i < 50; i++ )
        zColors[i] = TColor::GetColorPalette(i*zColorStep % 255 );


    bool lgZ = false;
    float alpha = 0.6;
    for ( int i = 0; i < N; i++ ){

        vizX = xx( cmdX[i], cmdY[i], cmdZ[i], projType );
        vizY = yy( cmdX[i], cmdY[i], cmdZ[i], projType );
        printf( "\tpoint at (%f, %f, %f) -> (%f, %f)\n", cmdX[i], cmdY[i], cmdZ[i], vizX, vizY );

        int zIndex = 0;
        if ( fabs( cmdZ[i] - 151.75) < 2.5 ) zIndex = 1;
        if ( fabs( cmdZ[i] - 165.25) < 2.5 ) zIndex = 2;
        if ( fabs( cmdZ[i] - 178.75) < 2.5 ) zIndex = 3;

        //add locations of other detectors-AGE
        //FTT--approximate locations
        if ( fabs( cmdZ[i] - 281) < 2.5 ) zIndex = 4;
        if ( fabs( cmdZ[i] - 304) < 2.5 ) zIndex = 5;
        if ( fabs( cmdZ[i] - 325) < 2.5 ) zIndex = 6;
        if ( fabs( cmdZ[i] - 348) < 2.5 ) zIndex = 7;
        //EPD--approx.
        if ( fabs( cmdZ[i] - 375) < 2.5 ) zIndex = 8;
        //FCS--approx.
        //if ( fabs( cmdZ[i] - 721) < 2.5 ) zIndex = 9;     //wcal
        //if ( fabs( cmdZ[i] - 804) < 2.5 ) zIndex = 10;    //hcal

        TMarker *mk = new TMarker( vizX, vizY, 20 );
        
        mk->SetMarkerSize( 2.5 );
        if (zIndex >= 1 && zIndex < 50){                //see if should be changed to zIndex < 9-AGE
            slc = zColors[zIndex];
        }
        mk->SetMarkerSize( zSizes[zIndex] );
        



        // mk->SetMarkerSize( (float)(zIndex) *  0.5 + 0.5 );

        alpha = 0.6;
        if ( zIndex != 8 && (cmdE != nullptr && projType == kRZSigned) ){       //FCS for RZ
            //mk->SetMarkerStyle( 21 );   //sets marker to a square, change to use TBox instead-AGE
            //mk->SetMarkerSize( 0.5 + 0.5 * cmdE[i] );
            mk->SetMarkerSize(0);
            alpha = (cmdE[i] / 10.0);
            if (alpha>=1) alpha = 1;
            TBox *box = new TBox( vizX-0.05*cmdE[i], vizY-0.5, vizX, vizY+0.5 );
            box->SetFillColor(210);
            if ( name == "WCal Clusters" || name == "HCal CLusters" ){
                box->SetFillColor(880);
                mk->SetMarkerSize(1);
            }
            box->Draw("same");
        }
        if ( name == "FTT Clusters" && projType == kXY ){
            mk->SetMarkerSize(0);
            TLine XCluster;
            XCluster.SetLineWidth(1);
            XCluster.SetLineColor(9); //dark blue
            TLine YCluster;
            YCluster.SetLineWidth(1);
            YCluster.SetLineColor(46); //dark red
            float x0;
            float x1;
            float y0;
            float y1;
            if (vizX < 0){
                x0 = -50;
                x1 = 0;
            } else if(vizX >= 0){
                x0 = 0;
                x1 = 50;
            }
            if (vizY < 0){
                y0 = -50;
                y1 = 0;
            } else if (vizY >= 0){
                y0 = 0;
                y1 = 50;
            }

            XCluster.DrawLine(vizX, y0, vizX, y1);
            YCluster.DrawLine(x0, vizY, x1, vizY);

        }
        
        if ( cmdE != nullptr && (zIndex == 8 || projType != kRZSigned) ){   //EPD for RZ and EPD and FCS for XY
            mk->SetMarkerStyle(21);
            mk->SetMarkerSize( 0.005 * cmdE[i]);
        }

        printf( "\tzIndex = %d -> color = %d \n", zIndex, slc );
        
        mk->SetMarkerColorAlpha( slc, alpha );
        if ( zIndex >= 1 ){
            mk->SetMarkerColorAlpha( slc, alpha );
            lgZ = true;
        }

        //change marker style etc. for projected points only-AGE
        /*if( name == "Proj" ){
            mk->SetMarkerStyle(23);
            mk->SetMarkerColor(2);
            mk->SetMarkerSize(1.5);
        }*/

        mk->Draw("same");
        
    }

    if ( lgZ ){
        /*for ( int i = 1; i < 4; i++){
            TMarker *mk1 = new TMarker( LegendX, LegendY, 20 );
            mk1->SetMarkerSize( 2.5 );
            mk1->SetMarkerColorAlpha( zColors[i], 0.5 );
            mk1->Draw("same");
                    t->DrawText( LegendX + 2, LegendY - 0.5, TString::Format( "%s: %d", name, i ) );

                    LegendY -= 5;
        }*/
        if (name == "FST"){
            for ( int i = 1; i < 4; i++ ){
                TMarker *mk1 = new TMarker( LegendX, LegendY, 20 );
                mk1->SetMarkerSize( 2.5 );
                mk1->SetMarkerColorAlpha( zColors[i], 0.5 );
                mk1->Draw("same");
                    t->DrawText( LegendX + 2, LegendY - 0.5, TString::Format( "%s: %d", name, i ) );

                    LegendY -= 5;
            }
        } else if (name == "FTT"){
            for ( int i = 1; i < 5; i++ ){
                TMarker *mk1 = new TMarker( LegendX, LegendY, 20 );
                    mk1->SetMarkerSize( 2.5 );
                    mk1->SetMarkerColorAlpha( zColors[i+3], 0.5 );
                    mk1->Draw("same");
                        t->DrawText( LegendX + 2, LegendY - 0.5, TString::Format( "%s: %d", name, i ) );

                        LegendY -= 5;
            }
        } else if (name == "FCS"){
            for ( int i = 1; i < 3; i++ ){
                TMarker *mk1 = new TMarker( LegendX, LegendY, 20 );
                    mk1->SetMarkerSize( 2.5 );
                    mk1->SetMarkerColorAlpha( zColors[i], 0.5 );
                    mk1->Draw("same");
                        t->DrawText( LegendX + 2, LegendY - 0.5, TString::Format( "%s: %d", name, i ) );

                        LegendY -= 5;
            }
        }

    } else {
        TMarker *mk1 = new TMarker( LegendX, LegendY, 20 ); 
        mk1->SetMarkerSize( 2.5 );
        mk1->SetMarkerColor( color );
        mk1->Draw("same");
        t->DrawText( LegendX + 2, LegendY - 0.5, TString::Format( "%s:", name ) );

        LegendY -= 5;
    }
}

//add function for seed finding-AGE
void viz_seed( const char* name, const char* cmd, int eventIndex, ProjectionType projType = kRZSigned){
   
    fwd->Draw( "reco.mChi2", "", "goff", 1, eventIndex );
    int nTrks = fwd->GetSelectedRows();

    TLine line;
    line.SetLineWidth(2);
    line.SetLineColor(1);
    TLine proj;
    for (int i = 0; i < nTrks; i++){        //loop over number of tracks

        fwd->Draw( TString::Format("reco[%d].mProjections.mXYZ.fX:reco[%d].mProjections.mXYZ.fY:reco[%d].mProjections.mXYZ.fZ", i, i, i), "", "goff", 1, eventIndex );
        auto nHits = fwd->GetSelectedRows();
        auto projX = fwd->GetV1();
        auto projY = fwd->GetV2();
        auto projZ = fwd->GetV3();
        /*std::vector<double> projX;
        std::vector<double> projY;
        std::vector<double> projZ;

        for (int hit = 0; hit < nHits; ++hit) {
            projX.push_back(fwd->GetV1()[hit]);
            projY.push_back(fwd->GetV2()[hit]);
            projZ.push_back(fwd->GetV3()[hit]);
        }*/

        //select only the seeds that have same track id as track number
        fwd->Draw( cmd, TString::Format("seeds.mTrackId == %d", i), "goff", 1, eventIndex );
        //fwd->Draw( TString::Format("seeds[%d].mXYZ.fX:seeds[%d].mXYZ.fY:seeds[%d].mXYZ.fZ", i, i, i), "", "goff", 1, eventIndex );
        int numSeeds = fwd->GetSelectedRows();
        auto newX = fwd->GetV1();
        auto newY = fwd->GetV2();
        auto newZ = fwd->GetV3();

        for ( int j = 0; j < numSeeds - 1; j++){

            float x0 = xx( newX[j], newY[j], newZ[j], projType );
            float y0 = yy( newX[j], newY[j], newZ[j], projType );
            float x1 = xx( newX[j+1], newY[j+1], newZ[j+1], projType );
            float y1 = yy( newX[j+1], newY[j+1], newZ[j+1], projType );

            /*if ( fabs(x0 - projX[j+1]) <= 1  ){
                line.SetLineColor(1);
            }*/

            line.DrawLine(x0, y0, x1, y1);
        }

    }
}

//add function for track projection
void viz_proj( int eventIndex, ProjectionType projType = kRZSigned, bool markers = false ){

    //get number of tracks
    fwd->Draw( "reco.mChi2", "", "goff", 1, eventIndex);   //check if this is correct data to use to get nTrks
    int nTrks = fwd->GetSelectedRows();

    //create line for track
    TLine trkproj;
    trkproj.SetLineWidth(1.5);
    trkproj.SetLineColor(24);    //light green

    //loop over each track in the event
    for ( int i = 0; i < nTrks; i++ ){

        //get hits in i'th track
        fwd->Draw( TString::Format("reco[%d].mProjections.mXYZ.fX:reco[%d].mProjections.mXYZ.fY:reco[%d].mProjections.mXYZ.fZ", i, i, i), "", "goff", 1, eventIndex );
        auto nHits = fwd->GetSelectedRows();
        auto projX = fwd->GetV1();
        auto projY = fwd->GetV2();
        auto projZ = fwd->GetV3();
             

        //loop over hits in each track
        for ( int j = 0; j < nHits - 1; j++ ){

            //assign the x and y positions of the track projection
            float x0 = xx( projX[j], projY[j], projZ[j], projType );
            float y0 = yy( projX[j], projY[j], projZ[j], projType );
            float x1 = xx( projX[j+1], projY[j+1], projZ[j+1], projType );
            float y1 = yy( projX[j+1], projY[j+1], projZ[j+1], projType );

            /*trkproj.SetLineColor(i+2);
            if (i == 0 || i == 10 ){
                trkproj.SetLineColor(1);
            }*/
            trkproj.DrawLine(x0, y0, x1, y1);
        }

        //add markers 
        if (markers){
            for ( int j = 0; j < nHits; j++ ){

                float x = xx( projX[j], projY[j], projZ[j], projType );
                float y = yy( projX[j], projY[j], projZ[j], projType );

                TMarker *mk = new TMarker( x, y, 20);
                mk->SetMarkerStyle(23);
                mk->SetMarkerColor(2);
                mk->SetMarkerSize(1.5);

                mk->Draw("same");
            }
        }
    }
    if (markers){
        //add marker to the legend
        TText *t = new TText(.5,.5,"Hello World !");
        t->SetTextColor(kBlack);
        t->SetTextFont(43);
        t->SetTextSize(20);
        //make this more functional?
        if ( projType == kRZSigned ){
            LegendY = 5;
        } else if (projType == kXY ){
            LegendY = -15;
        }
        TMarker *mk1 = new TMarker( LegendX, LegendY, 23 ); 
        mk1->SetMarkerSize( 2.5 );
        mk1->SetMarkerColor( 2 );
        mk1->Draw("same");
        t->DrawText( LegendX + 2, LegendY - 0.5, TString::Format( "Projected Hits ") );
    }
    
}  //end of fn


//add function to compare lines
//float comp_lines()


float statTextY = 0.97;
void n() { statTextY -= 0.05; }
void viz_stats( int eventIndex ){
    statTextY = 0.97;
    TText text;
    text.SetTextFont(43);
    text.SetTextSize(36);


    /*fwd->Draw( "fstX:fstY:fstZ", "", "goff", 1, eventIndex );
    int numEpd = fwd->GetSelectedRows();
    fwd->Draw( "fttX:fttY:fttZ", "", "goff", 1, eventIndex );
    int numEpd = fwd->GetSelectedRows();
    fwd->Draw( "epdX:epdY:epdZ", "", "goff", 1, eventIndex );
    int numEpd = fwd->GetSelectedRows();
    fwd->Draw( "fcsX:fcsY:fcsZ", "", "goff", 1, eventIndex );
    int numEpd = fwd->GetSelectedRows();*/

    fwd->Draw( "reco.mChi2", "", "goff", 1, eventIndex );
    int numTracks = fwd->GetSelectedRows();
    fwd->Draw( "fstHits.mXYZ.fX:fstHits.mXYZ.fY:fstHits.mXYZ.fZ", "", "goff", 1, eventIndex );
    int numFst = fwd->GetSelectedRows();
    fwd->Draw( "fttPoints.mXYZ.fX:fttPoints.mXYZ.fY:fttPoints.mXYZ.fZ", "", "goff", 1, eventIndex );
    int numFtt = fwd->GetSelectedRows();
    //fwd->Draw( "EPD hits", "", "goff", 1, eventIndex );
    //int numEpd = fwd->GetSelectedRows();
    fwd->Draw( "wcalHits.mXYZ.fX:wcalHits.mXYZ.fY:wcalHits.mXYZ.fZ", "", "goff", 1, eventIndex );
    int numWcalHits = fwd->GetSelectedRows();
    fwd->Draw( "hcalHits.mXYZ.fX:hcalHits.mXYZ.fY:hcalHits.mXYZ.fZ", "", "goff", 1, eventIndex );
    int numHcalHits = fwd->GetSelectedRows();
    fwd->Draw( "wcalClusters.mXYZ.fX:wcalClusters.mXYZ.fY:wcalClusters.mXYZ.fZ", "", "goff", 1, eventIndex );
    int numWcal = fwd->GetSelectedRows();
    fwd->Draw( "hcalClusters.mXYZ.fX:hcalClusters.mXYZ.fY:hcalClusters.mXYZ.fZ", "", "goff", 1, eventIndex );
    int numHcal = fwd->GetSelectedRows();
    
    text.DrawTextNDC( 0.05, statTextY, TString::Format("Event : %d", eventIndex) ); n();
    text.DrawTextNDC( 0.05, statTextY, TString::Format("Tracks : %d", numTracks) ); n();
    text.DrawTextNDC( 0.05, statTextY, TString::Format("FST Hits : %d", numFst) ); n();
    text.DrawTextNDC( 0.05, statTextY, TString::Format("FTT Hits : %d", numFtt) ); n();
    //text.DrawTextNDC( 0.05, statTextY, TString::Format("EPD Hits : %d", numEpd) ); n();
    //text.DrawTextNDC( 0.05, statTextY, TString::Format("WCal Hits : %d", numWcalHits) ); n();
    //text.DrawTextNDC( 0.05, statTextY, TString::Format("HCal Hits : %d", numHcalHits) ); n();
    text.DrawTextNDC( 0.05, statTextY, TString::Format("WCal Clusters : %d", numWcal) ); n();
    text.DrawTextNDC( 0.05, statTextY, TString::Format("HCal Clusters : %d", numHcal) ); n();

    //print reco statistics
    /*fwd->Draw( "reco.mProjections.mXYZ:fX:reco.mProjections.mXYZ:fY:reco.mProjections.mXYZ:fZ", "", "goff", 1, eventIndex );
    int numReco = fwd->GetSelectedRows();
    statTextY = 0.92;
    text.DrawTextNDC( 0.35, statTextY, TString::Format("Reco Hits : %d", numReco) ); n();*/

}


int viz_event( int eventIndex, ProjectionType projType = kRZSigned ){

    if ( projType == kRZSigned || projType == kXZ || projType == kYZ ){
        hFrame = new TH2F( "hFrame", ";z;R", 520, -30, 900, 260, -130, 130 );
        hFrame->SetTitle( "Event Visualization (RZ Signed)" );
        LegendX = 10;
        LegendY = 60;
    } else if ( projType == kRZ ){
        hFrame = new TH2F( "hFrame", ";z;R", 500, 0, 900, 60, 0, 60 );
        hFrame->SetTitle( "Event Visualization (RZ Signed)" );
        LegendX = 10;
        LegendY = 60;
    } else if ( projType == kXY ){
        hFrame = new TH2F( "hFrame", ";x;y", 5, -50, 50, 5, -50, 50 );
        hFrame->SetTitle( "Event Visualization (XY)" );
        LegendX = -40;
        LegendY = 40;
    }

    printf( "Visualizing Event %d \n", eventIndex );

    fwd->Draw( "reco.mChi2", "", "", 1, eventIndex );
    int nTrk = fwd->GetSelectedRows();          //number of reco tracks
    printf( "Event has %d Tracks \n", nTrk );   //changed from %lld to %d to eliminate an error that occurred-AGE


    hFrame->Draw("colz");

    //add detector locations
    if (projType == kRZSigned){

        TLine *fst1 = new TLine(151.75, -28.3, 151.75, 28.3);
        fst1->SetLineWidth(2);
        fst1->SetLineColor(12);
        fst1->Draw("same");
        TLine *fst2 = new TLine(165.25, -28.3, 165.25, 28.3);
        fst2->SetLineWidth(2);
        fst2->SetLineColor(12);
        fst2->Draw("same");
        TLine *fst3 = new TLine(178.75, -28.3, 178.75, 28.3);
        fst3->SetLineWidth(2);
        fst3->SetLineColor(12);
        fst3->Draw("same");

        TLine *ftt1 = new TLine(281, -60, 281, 60);
        ftt1->SetLineWidth(2);
        ftt1->SetLineColor(12);
        ftt1->Draw("same");
        TLine *ftt2 = new TLine(304, -60, 304, 60);
        ftt2->SetLineWidth(2);
        ftt2->SetLineColor(12);
        ftt2->Draw("same");
        TLine *ftt3 = new TLine(325, -60, 325, 60);
        ftt3->SetLineWidth(2);
        ftt3->SetLineColor(12);
        ftt3->Draw("same");
        TLine *ftt4 = new TLine(348, -60, 348, 60);
        ftt4->SetLineWidth(2);
        ftt4->SetLineColor(12);
        ftt4->Draw("same");

        TLine *epd = new TLine(375, -130, 375, 130);
        epd->SetLineWidth(2);
        epd->SetLineColor(12);
        epd->Draw("same");

        //add tboxes for fcs
        TBox *wcal = new TBox( 720, -120, 735, 120 );
        wcal->SetFillColorAlpha(4, 0.2);
        wcal->Draw("same");
        TBox *hcal = new TBox( 800, -120, 815, 120 );
        hcal->SetFillColorAlpha(2, 0.2);
        hcal->Draw("same");

    }

    //viz_points( "FST", "fstX:fstY:fstZ", kGray, eventIndex, projType/*, true*/ );
    //viz_points( "EPD", "epdX:epdY:epdZ:epdE", kBlue, eventIndex, projType/*, true*/ );   //epd hits (only in fwdtree2)-AGE
    //viz_points( "FCS", "fcsX:fcsY:fcsZ:fcsE", kGreen, eventIndex, projType/*, true*/ );
    viz_points( "FST", "fstHits.mXYZ.fX:fstHits.mXYZ.fY:fstHits.mXYZ.fZ", kGray, eventIndex, projType, true );
    viz_points( "FTT", "fttPoints.mXYZ.fX:fttPoints.mXYZ.fY:fttPoints.mXYZ.fZ", kRed, eventIndex, projType );
    // viz_points( "FTT Clusters", "fttClusters.mXYZ.fX:fttClusters.mXYZ.fY:fttClusters.mXYZ.fZ", kRed, eventIndex, projType );
    viz_points( "WCal Hits", "wcalHits.mXYZ.fX:wcalHits.mXYZ.fY:wcalHits.mXYZ.fZ+705:100*wcalHits.mHit.mEnergy", kBlue, eventIndex, projType );
    viz_points( "HCal Hits", "hcalHits.mXYZ.fX:hcalHits.mXYZ.fY:hcalHits.mXYZ.fZ+785:100*wcalClusters.mClu.mEnergy", kTeal, eventIndex, projType/*, true*/ );
    viz_points( "WCal Clusters", "wcalClusters.mXYZ.fX:wcalClusters.mXYZ.fY:wcalClusters.mXYZ.fZ:100*wcalClusters.mClu.mEnergy", kViolet, eventIndex, projType/*, true*/ );
    viz_points( "HCal Clusters", "hcalClusters.mXYZ.fX:hcalClusters.mXYZ.fY:hcalClusters.mXYZ.fZ:100*wcalClusters.mClu.mEnergy", kGreen, eventIndex, projType/*, true*/ );   //add fcs hits-AGE

    viz_seed( "Seeds", "seeds.mXYZ.fX:seeds.mXYZ.fY:seeds.mXYZ.fZ", eventIndex, projType );
    viz_proj( eventIndex, projType, false);
    viz_points( "Proj", "reco.mProjections.mXYZ.fX:reco.mProjections.mXYZ.fY:reco.mProjections.mXYZ.fZ", kRed, eventIndex, projType);
    return nTrk;
}


//change to name of file being used-AGE 
void viz2( TString fn = "fwdtree.root", int view = kXY) {

    ProjectionType pjt = (ProjectionType)view;
    fData = new TFile( fn );
    fwd = (TTree*)fData->Get( "fwd" );

    gStyle->SetOptStat(0);

    float canWidth = 19 * 100;
    float canHeight = 16 * 100;
    gCan = new TCanvas( "g", "", canWidth, canHeight );
    gCan->SetMargin( 0, 0, 0, 0);
    gCan->cd();
    gCan->Draw();

    padRZ = new TPad( "padRZ", "", 0.0, 0.5, 0.95, 0.99 );
    padRZ->SetMargin( .05,.01,.05,.01 );
    padRZ->Draw("same");
    padRZ->cd();

    gCan->cd();
    padXY = new TPad( "padXY", "", 0.0, 0.0, 0.5, 0.5 );
    padXY->SetMargin( .1,.02,.05,.01 );
    padXY->Draw("same");
    padXY->cd();

    gCan->cd();
    padStat = new TPad( "padStat", "", 0.5, 0.0, 1.0, 0.5 );
    padStat->SetMargin( .1,.02,.05,.01 );
    padStat->Draw("same");
    padStat->cd();

    // gPad->SetMargin(0.1, 0.05, 0.15, 0.05);

    int nEvents = fwd->GetEntries();
    // nEvents = 1;
    // nEvents = 10;
    for ( int iEvent = 0; iEvent < nEvents; iEvent ++ ){

        printf( "Event: %d\n", iEvent );
        padRZ->cd();

        //TBox *wcal = new TBox( 720, -60, 735, 60 );
        //wcal->SetFillColor(4);
        //wcal->Draw("");
        int nTrk = viz_event( iEvent, kRZSigned );


        padXY->cd();
        viz_event( iEvent, kXY );
        if (nTrk > -1){
            padRZ->Update();
            padXY->Update();

            padStat->cd();
            padStat->Clear();
            viz_stats( iEvent );  //changed to provide number of tracks as well-AGE
            padStat->Update();
            gCan->Update();
            gCan->Print( TString::Format( "out_event%d.pdf", iEvent ) );
        }


        // cin.get();
        // if (viz_event( iEvent ) > 0 )
            // break;

        hFrame->Reset();
    }

}
