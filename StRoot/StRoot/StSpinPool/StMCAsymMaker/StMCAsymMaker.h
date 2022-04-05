//class  StMCAsymMaker
//author R.Fatemi
//date   2007/Feb/04 
//This class allows the calculation of "asymmetries" in MC data by using partonic kinematic information from the pythia record
//along with polarized and unpolarized pdfs and LO partonic asymmetries. Documentation on the Method of Asymmetry Weights (MAW)
//can be found in the preprint on SPHINX (hep-0005320)

#ifndef STAR_StMCAsymMaker
#define STAR_StMCAsymMaker

#ifndef StMaker_H
#include "StMaker.h"
extern "C" void polar_(int*,double*,double*,double*,int*);
extern "C" Double_t ctq5pd_(int*,int*,double*,double*,int*);
extern "C" void num_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" void denom_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" void polpar_(double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" void dssvini2009a_(int*);
extern "C" void dssvfit2009a_(double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" void lss2010_(int*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" { extern struct { int iini; } intini_; }
extern "C" void polpdf_(int*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" double dinteg_(double (*)(double&),double&,double&,double&);
//extern "C" void parpol_(int*, double*, double*, double* ,double* , double*, double*, double*, double*, double*, double*);
//extern "C" void parpol2_(int*, double*, double*, double* ,double* , double*, double*, double*, double*, double*, double*);
//extern "C" void unpolar_(int*, double*, double*, double*, int*);
//extern "C" void grv98pa_(int*, double*, double*, double*, double*, double*, double*, double*, double*);
//extern "C" void grv98f2_(int*, double*, double*, double*, double*, double*, double*);
#endif

class StChain;
class StMuDstMaker;
class StMuEvent;
class StMcEvent;
class StEventInfo;
class St_particle;
class St_g2t_event;
class St_g2t_pythia;

#include "StPythiaEvent.h"

class StMCAsymMaker : public StMaker
{
private:
    StPythiaEvent * mEvent;
    void fillPythiaEvent(StPythiaEvent * pythia);
    
    //pointers to makers
    StMuDstMaker *muDstMaker;
    StMuEvent *muEvent;
    StMcEvent *mcEvent;
    St_particle *particleTabPtr;
    St_g2t_event *Pg2t_event;
    St_g2t_pythia *Pg2t_pythia;

    int geantPID;                        //sub process id from GEANT table
    int geantID;                         //event number from GEANT table
    int evtid;                           //event number from MuDst
    int pid;                             //subprocess id 
    Double_t parton1[11],parton2[11];    //pythia record entry of scattered partons
    int flavor1,flavor2,flavor3,flavor4; //flavor of scattered partons before/after hard interaction
    float s,t,u,hard_p,cos_th,x1,x2;     //partonic kinematics
    int pol_id_flag, unpol_id_flag;      //flags for *.F 
    double partonic_all,Q2;              //partonic a_LL from LO calculations and hard scale

    double df1_NLO_GSA,df1_NLO_GSB,df1_NLO_GSC;         //NLO Gehrmann & Stirling Scenario A/B/C PDF
    double df2_NLO_GSA,df2_NLO_GSB,df2_NLO_GSC;         //NLO Gehrmann & Stirling Scenario A/B/C PDF
    double weight_NLO_GSA,weight_NLO_GSB,weight_NLO_GSC;//NLO Gehrmann & Stirling Scenario A/B/C weight
    
    double df1_LO,df2_LO,f1_LO,f2_LO,weight_LO;         //Leading Order polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO,df2_NLO,f1_NLO,f2_NLO,weight_NLO;    //NLO STD polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_g0,df2_NLO_g0,weight_NLO_g0;         //NLO G0  polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_gmax,df2_NLO_gmax,weight_NLO_gmax;   //NLO GMAX polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_gmin,df2_NLO_gmin,weight_NLO_gmin;   //NLO GMIN polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_m015,df2_NLO_m015,weight_NLO_m015;   //NLO M015 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_m030,df2_NLO_m030,weight_NLO_m030;   //NLO M030 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_m045,df2_NLO_m045,weight_NLO_m045;   //NLO M045 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_m060,df2_NLO_m060,weight_NLO_m060;   //NLO M060 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_m075,df2_NLO_m075,weight_NLO_m075;   //NLO M075 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_m090,df2_NLO_m090,weight_NLO_m090;   //NLO M090 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_m105,df2_NLO_m105,weight_NLO_m105;   //NLO M105 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_p030,df2_NLO_p030,weight_NLO_p030;   //NLO P030 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_p045,df2_NLO_p045,weight_NLO_p045;   //NLO P045 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_p060,df2_NLO_p060,weight_NLO_p060;   //NLO P060 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
    double df1_NLO_p070,df2_NLO_p070,weight_NLO_p070;   //NLO P070 polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2

    double df1_NLO_DSSV,df2_NLO_DSSV,weight_NLO_DSSV;   //NLO de Florian, Sassot, Stratman, & Vogelsang PDF
    double df1_NLO_DSSV2009a,df2_NLO_DSSV2009a,weight_NLO_DSSV2009a;   //NLO de Florian, Sassot, Stratman, & Vogelsang PDF with RHIC Run 9 data

    double df1_NLO_LSS1,df2_NLO_LSS1,weight_NLO_LSS1;   //NLO Leader, Sidorov, & Stamenov PDF Scenario 1/2/3
    double df1_NLO_LSS2,df2_NLO_LSS2,weight_NLO_LSS2;   //NLO Leader, Sidorov, & Stamenov PDF Scenario 1/2/3
    double df1_NLO_LSS3,df2_NLO_LSS3,weight_NLO_LSS3;   //NLO Leader, Sidorov, & Stamenov PDF Scenario 1/2/3
    double df1_NLO_LSS2010_delGpos,df2_NLO_LSS2010_delGpos,weight_NLO_LSS2010_delGpos;
    double df1_NLO_LSS2010_chsign_delG,df2_NLO_LSS2010_chsign_delG,weight_NLO_LSS2010_chsign_delG;

    double df1_NLO_AAC1,df2_NLO_AAC1,weight_NLO_AAC1;   //NLO Asymmetry Analysis Collaboration PDF Scenario 1/2/3
    double df1_NLO_AAC2,df2_NLO_AAC2,weight_NLO_AAC2;   //NLO Asymmetry Analysis Collaboration PDF Scenario 1/2/3
    double df1_NLO_AAC3,df2_NLO_AAC3,weight_NLO_AAC3;   //NLO Asymmetry Analysis Collaboration PDF Scenario 1/2/3

    double df1_NLO_BB1,df2_NLO_BB1,weight_NLO_BB1;      //NLO Blumlein & Bottcher PDF Scenario 1/2
    double df1_NLO_BB2,df2_NLO_BB2,weight_NLO_BB2;      //NLO Blumlein & Bottcher PDF Scenario 1/2
    double df1_NLO_BB2010,df2_NLO_BB2010,weight_NLO_BB2010;

    double df1_NLO_DNS1,df2_NLO_DNS1,weight_NLO_DNS1;   //NLO de Florian, Navarro, & Sassot PDF Scenario 1/2
    double df1_NLO_DNS2,df2_NLO_DNS2,weight_NLO_DNS2;   //NLO de Florian, Navarro, & Sassot PDF Scenario 1/2

public: 

    //Gehrmann&Stirling pDF
    static Double_t get_polPDF_NLO_GSA(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_GSB(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_GSC(int flavor, double x1, double Q2);

    //GRSV PDF
    static Double_t get_polPDF_LO(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_g0(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_gmax(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_gmin(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_m015(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_m030(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_m045(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_m060(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_m075(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_m090(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_m105(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_p030(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_p045(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_p060(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_p070(int flavor, double x1, double Q2);
    
    //DSSV PDF
    static Double_t get_polPDF_NLO_DSSV(int flavor, double x1, double Q2);

    //DSSV 2009a PDF
    static Double_t get_polPDF_NLO_DSSV2009a(int flavor, double x1, double Q2);

    //LSS PDF
    static Double_t get_polPDF_NLO_LSS1(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_LSS2(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_LSS3(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_LSS2010_delGpos(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_LSS2010_chsign_delG(int flavor, double x1, double Q2);

    //AAC PDF
    static Double_t get_polPDF_NLO_AAC1(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_AAC2(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_AAC3(int flavor, double x1, double Q2);

    //BB PDF
    static Double_t get_polPDF_NLO_BB1(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_BB2(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_BB2010(int flavor, double x1, double Q2);

    //DNS PDF
    static Double_t get_polPDF_NLO_DNS1(int flavor, double x1, double Q2);
    static Double_t get_polPDF_NLO_DNS2(int flavor, double x1, double Q2);

    static Double_t get_unpolPDF_LO(int flavor, double x1, double Q2); 
    static Double_t get_unpolPDF_NLO(int flavor, double x1, double Q2);

    static Double_t getPartonicALL(double s, double t, double u, int pid, int flavor1, int flavor2, int flavor3, int flavor4);

    static Double_t getProtonA1(double x, double Q2);
    static double get_polPDF_firstMoment(int pdf, int flavor, double Q2, double xmin, double xmax, double epsilon = 1.0e-3);

    StMCAsymMaker(const char *name="MCAsym");
    virtual  ~StMCAsymMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    void Clear(const Option_t* option = "");
    void Zero();

    const StPythiaEvent* pythiaEvent() const { return mEvent; }
    
    const St_particle* particleTable() const { return particleTabPtr; }
    
    virtual const char *GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: StMCAsymMaker.h,v 1.14 2014/08/06 11:43:39 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
        return cvs;
    }

    ClassDef(StMCAsymMaker,0)   //StAF chain virtual base class for Makers
};

#endif
