#ifndef genfit_STARField_h
#define genfit_STARField_h

#include "TVector3.h"
#include "StarMagField/StarMagField.h"
#include "GenFit/AbsBField.h"

//_______________________________________________________________________________________
// Adaptor for STAR magnetic field loaded via StarMagField Maker
class StarFieldAdaptor : public genfit::AbsBField {
  public:
    bool mUseMappedField = false;
    bool mDebugField = false;
    static size_t sNCalls;
    StarFieldAdaptor() {
        mDebugField = false;


        if ( mUseMappedField){
            hdBx = new TH1F( "hdBx", ";dBx", 100, -5e-1, 5e-1 );
            hdBy = new TH1F( "hdBy", ";dBy", 100, -5e-1, 5e-1 );
            hdBz = new TH1F( "hdBz", ";dBz", 100, -5e-2, 5e-2 );

            TFile *fileMap = new TFile("/star/u/jdb/work/ssw/MagField/FieldOn.root", "READ");
            hBx = (TH2D *)fileMap->Get("fieldX");
            hBy = (TH2D *)fileMap->Get("fieldY");
            hBz = (TH2D *)fileMap->Get("fieldZ");
        }
    };

    void WriteHistograms(){
        hdBx->Write();
        hdBy->Write();
        hdBz->Write();
    }

    TH2D *hBx, *hBy, *hBz;
    TH1 *hdBz, *hdBx, *hdBy;
    virtual TVector3 get(const TVector3 &position) const {
        double x[] = {position[0], position[1], position[2]};
        double B[] = {0, 0, 0};

        get( x[0], x[1], x[2], B[0], B[1], B[2] );

        return TVector3(B);
    };

    inline virtual void mappedGet( const double &_x, const double &_y, const double &_z, double &Bx, double &By, double &Bz ) const {
        double x[] = {_x, _y, _z};
        double B[] = {0, 0, 0};
        double BRef[] = {0, 0, 0};
        double dB[] = {0, 0, 0};
        
        // StarFieldAdaptor::sNCalls ++;
        if (StarMagField::Instance()){

            if ( mDebugField )
                StarMagField::Instance()->Field(x, BRef);

            float z = x[2];
            float r = sqrt(pow(x[0],2) + pow(x[1],2));
            if( fabs(z) < 250. && r < 50.)  { 
                B[0] = 0.; B[1] = 0.; B[2] = 4.97979927; 
                
                if ( mDebugField ){
                    dB[0] = BRef[0] - B[0];
                    dB[1] = BRef[1] - B[1];
                    dB[2] = BRef[2] - B[2];
                    hdBx->Fill( dB[0] );
                    hdBy->Fill( dB[1] );
                    hdBz->Fill( dB[2] );
                }
            } 
            else if ( 200 < z && z < 400 && r < 50  ){
                B[0] = hBx->Interpolate( z, r );
                B[1] = hBy->Interpolate( z, r );
                B[2] = hBz->Interpolate( z, r );

                if ( mDebugField ){
                    dB[0] = BRef[0] - B[0];
                    dB[1] = BRef[1] - B[1];
                    dB[2] = BRef[2] - B[2];
                    hdBx->Fill( dB[0] );
                    hdBy->Fill( dB[1] );
                    hdBz->Fill( dB[2] );
                }
            } 
            else {
                StarMagField::Instance()->Field(x, B);
            }
        }

        Bx = B[0];
        By = B[1];
        Bz = B[2];
        return;
    }

    inline virtual void get(const double &_x, const double &_y, const double &_z, double &Bx, double &By, double &Bz) const {
        double x[] = {_x, _y, _z};
        double B[] = {0, 0, 0};
        // StarFieldAdaptor::sNCalls ++;

        if ( mUseMappedField ){
            mappedGet( _x, _y, _z, Bx, By, Bz );
            return;
        } 

        if (StarMagField::Instance()){

            float z = x[2];
            float r = sqrt(pow(x[0],2) + pow(x[1],2));
            if( fabs(z) < 250. && r < 50.)  { 
                B[0] = 0.; B[1] = 0.; B[2] = 4.97979927; 
            } else if ( fabs(z) > 450 ) {
                B[0] = 0.; B[1] = 0.; B[2] = 0.; 
            } else {
                StarMagField::Instance()->Field(x, B);
            }
        }
        

        Bx = B[0];
        By = B[1];
        Bz = B[2];
        return;
    };

    // if you want histograms make sure to write them here so that they show up in the ROOT output
    // void WriteHistograms(){
    // }
};


#endif
