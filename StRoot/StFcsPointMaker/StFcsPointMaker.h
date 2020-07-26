// $Id: StFcsPointMaker.h,v 1.1 2019/10/23 13:37:11 akio Exp $

#ifndef STROOT_STFCSPOINTMAKER_STFCSPOINTMAKER_H_
#define STROOT_STFCSPOINTMAKER_STFCSPOINTMAKER_H_

#include "TMinuit.h"
#include "StMaker.h"

class StFcsCollection;
class StFcsHit;
class StFcsCluster;
class StFcsPoint;
class StFcsDbMaker;
class StMuDst;

class StFcsPointMaker : public StMaker {
public:
    
    StFcsPointMaker(const char* name = "StFcsPointMaker");
    ~StFcsPointMaker();
    Int_t InitRun(Int_t runNumber);
    Int_t Make();
    void Clear(Option_t* option = "");
    
    /** Set to read MuDST, then only this maker does is recalc point position using DB values */
    /** and does NOT perform cluster finding nor fitting */
    //void SetReadMuDst(int v=1) {mReadMuDst=v;} 
    
    void setDebug(int v) {mDebug=v;}
    void setMinuitPrintLevel(int v) {mMinuit.SetPrintLevel(v);}

    void setShowerShape(int v) {mShowerShape=v;}
    void setShowerShapeScale(float v) {mShowerShapeScale=v;}

    //1 photon fit switches and parameters
    void set_PH1_FixEnergy(int v)  {m_PH1_FixEnergy=v;}
    void set_PH1_DELTA_X(double v) {m_PH1_DELTA_X=v;}
    void set_PH1_DELTA_E(double v) {m_PH1_DELTA_E=v;}
    
    //2 photon fit switches and parameters
    void set_PH2_FixEnergy(int v)    {m_PH2_FixEnergy=v;}
    void set_PH2_FixTheta(int v)     {m_PH2_FixTheta=v;}
    void set_PH2_DELTA_X(double v)   {m_PH2_DELTA_X=v;}
    void set_PH2_DELTA_E(double v)   {m_PH2_DELTA_E=v;}
    void set_PH2_LOW_DGG(double v)   {m_PH2_LOW_DGG=v;}
    void set_PH2_HIGH_DGG(double v)  {m_PH2_HIGH_DGG=v;}
    void set_PH2_MAXTHETA_F(double v){m_PH2_MAXTHETA_F=v;}
    void set_PH2_StartDggFactor(double v) {m_PH2_StartDggFactor=v;}

 private:
    void setShowerShapeParameters(int det);
    void fitClusters(int det);  // fitting for a detector
    double fit1PhotonCluster(StFcsCluster* c, StFcsPoint* p);  //fit with 1 photon
    double fit2PhotonCluster(StFcsCluster* c, StFcsPoint* p1, StFcsPoint* p2); //fit with 2 photon

    //TMinuit minimization function. This has to be void(*)(Int_t &, Double_t *, Double_t &f, Double_t *, Int_t) 	 
    static void minimizationFunctionNPhoton(Int_t& npar, Double_t* grad, Double_t& fval, Double_t* par, Int_t flag); 
    static void minimizationFunction2Photon(Int_t& npar, Double_t* grad, Double_t& fval, Double_t* par, Int_t flag); 
    static Double_t energyDepositionInTower(Double_t x, Double_t y,Double_t xun, Double_t yun);
    
    StFcsDbMaker* mDb=0;               //!
    StFcsCollection* mFcsCollection=0; //!
        
    Int_t mDebug=0;                    //! debug opption 
    Int_t mShowerShape=0;              // shower shape choice
    Float_t mShowerShapeScale=1.0;     // shower shape trasnverset scale

    TMinuit mMinuit;                  //! Minuit interface

    //1 photon fit switch and parameters
    Int_t    m_PH1_FixEnergy=1;
    Double_t m_PH1_DELTA_X=0.5; 
    Double_t m_PH1_DELTA_E=1.15; 

    //2 photon fit switch and parameters
    Int_t    m_PH2_FixEnergy=1;
    Int_t    m_PH2_FixTheta=1;
    Double_t m_PH2_DELTA_X=0.2; 
    Double_t m_PH2_DELTA_E=1.05; 
    Double_t m_PH2_LOW_DGG=0.8; 
    Double_t m_PH2_HIGH_DGG=3.0; 
    Double_t m_PH2_MAXTHETA_F=TMath::PiOver2(); 
    Double_t m_PH2_StartDggFactor=1.1;

    //Int_t readMuDst();
    //Int_t mReadMuDst=0;  //! 0= Do Fitting
    //                     //! 1= Just read Mudst

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
    ClassDef(StFcsPointMaker, 0)
};

// single shower shape compornent integral
inline Double_t showerShapeComponent(double x, double y, double a, double b) {
    if(a==0.0) return 0.0;
    return a * atan(x * y / (b * sqrt(b * b + x * x + y * y)));
}

// adding 3 compporments of shower shape integrals
inline Double_t energyDepositionDistribution(double x, double y, double* parameters){
    constexpr double ootwopi = 1.0/2.0/3.14159265358979323846;
    return (     showerShapeComponent(x, y, parameters[1], parameters[4])
	       + showerShapeComponent(x, y, parameters[2], parameters[5])
	       + showerShapeComponent(x, y, parameters[3], parameters[6]) ) * ootwopi;
}

// getting shower shape integrals to corners and subtract to get integral in a cell
inline Double_t energyDepositionInTowerSingleLayer(Double_t x, Double_t y, Double_t* parameters){
    return  energyDepositionDistribution(x-0.5, y-0.5, parameters)
	  - energyDepositionDistribution(x-0.5, y+0.5, parameters)
	  - energyDepositionDistribution(x+0.5, y-0.5, parameters)
	  + energyDepositionDistribution(x+0.5, y+0.5, parameters);
}


#endif  // STROOT_STFCSPOINTMAKER_STFCSPOINTMAKER_H_

