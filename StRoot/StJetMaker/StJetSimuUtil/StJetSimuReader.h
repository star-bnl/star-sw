//StJetSimuReader.h
//M.L. Miller
//MIT Software
//6/04

#ifndef StJetSimuReader_HH
#define StJetSimuReader_HH

#include "StMaker.h"
#include "StJetMaker/StJetMaker.h"

class TTree;
class TFile;
class StMuDstMaker;
class TH1F;
class TH2F;
class TH3F;
class TProfile;

/*!
  \class StJetSimuReader
  \author M.L. Miller (MIT Software)
  StJetSimuReader is a utility maker that reads the jet tree from file.  Currently it only supports
  single file reading, but additions to multiple files and chain reading are planned.  Also,
  we are still debugging the InitTree() method, which would allow for the jet tree to be
  a "friend" of the StMuDst, and would therefore make the StMuDstMaker responsible for the reading
  from file (a great savings in effort and bookkeeping!).
  An example block of analysis code is given in the exampleEventAna() method.
*/
class StJetSimuReader : public StMaker
{

  TObjArray  *HList; /// output histo access point

public:

    ///A useful typedef for the StJets map
    typedef map<string, StJets*, less<string> > JetBranchesMap;

    ///The constructor requires a valid instance of StMuDstMaker
    StJetSimuReader(const char* name, StMuDstMaker* uDstMaker);
    virtual ~StJetSimuReader();
    
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    
    ///Recover the TTree from file and prepare for reading
    virtual void InitFile(const char* file,const char *simufile);

    ///Prepare for reading the TTree, but as a "friend" of the StMuDst tree
    virtual void InitTree(TTree* tree);
    
    ///Access to the StJetsMap
    JetBranchesMap& jetsMap();
    
    //Renee's Analysis
    StMuTrack *primTrack;
    StBemcTables *myTable;

    //Add histos
    void EventHisto();
    void SimuHisto2004();
    void SimuHisto2005();
    void ScaleHisto();
    void ConeYieldHisto();
    void CheckPDFHisto();
    void AsymBiasHisto();
    void SetHList(TObjArray * x){HList=x;}

    void SetEventSwitch(int i);
    void SetSimuSwitch(int i);
    void SetConeYieldSwitch(int i);
    void SetEnergyScaleSwitch(int i);
    void SetCheckPDFSwitch(int i);
    void SetAsymBiasSwitch(int i);

    void JetAssociator();
    void EnergyScale();
    void PythiaAna();
    void EventAna();
    void SimuAna();
    void ConeYieldAna();
    void CheckPDF();
    void AsymBias();

    void setBemc2004Trig(int i);
    void getBemc2004Trig(float j[20], int *i);
    void getSoftTrig(float j[20], int *i);
    void setSoftTrig(float thresh);

    //Check PDF
    TH2F *pol[10];
    TH2F *unpol[10];
    TH2F *part[10];
    TH1F *Q2hist;

    //AsymBias
    TH2F *HTverNeu,*jet3D[6];
    TH1F *x1H[16], *x2H[16], *Q2H[16], *df1H[16], *df2H[16], *f1H[16], *f2H[16], *aLLH[16], *dR, *PYIN;
    TH1F *PYETAjpTA[5];
    TH1F *PYETAjpTB[5];
    TH1F *PYETAjpTC[5];
    TH1F *PYASCjpTA[5];
    TH1F *PYASCjpTB[5];
    TH1F *PYASCjpTC[5];
    TH1F *PYDIAjpTA[5];
    TH1F *PYDIAjpTB[5];
    TH1F *PYDIAjpTC[5];
    TH1F *PYBBCjpTA[5];
    TH1F *PYBBCjpTB[5];
    TH1F *PYBBCjpTC[5];
    TH1F *PYHTjpTA[5];
    TH1F *PYHTjpTB[5];
    TH1F *PYHTjpTC[5];
    TH1F *DIAjpTA[5];
    TH1F *DIAjpTB[5];
    TH1F *DIAjpTC[5];
    TH1F *DWDIAjpTA[5];
    TH1F *DWDIAjpTB[5];
    TH1F *DWDIAjpTC[5];
    TH1F *UPDIAjpTA[5];
    TH1F *UPDIAjpTB[5];
    TH1F *UPDIAjpTC[5];

    //SimuAna
    TH1F *jo[20];
    TH1F *HTfreq;
    TH1F *Count[20];
    TH1F *jpT[20];
    TH1F *jpTrk[20];
    TH1F *jpTow[20];

    TH1F *partpT;
    TH1F *HTpartpT;
    TH1F *JPpartpT;

    TH1F *PYjpTA[20];
    TH1F *PYjpTB[20];
    TH1F *PYjpTC[20];
    TH1F *PYjpTD[20];
    TH1F *PYjpTE[20];

    TH1F *RAWjpTA[20];
    TH1F *RAWjpTB[20];
    TH1F *RAWjpTC[20];

    TH1F *jpTA[20];
    TH1F *jpTB[20];
    TH1F *jpTC[20];
    TH1F *jpTD[20];
    TH1F *jpTE[20];

    TH1F *HTjpTA[20];
    TH1F *HTjpTB[20];
    TH1F *HTjpTC[20];
    TH1F *HTjpTD[20];

    TH1F *JPjpTA[20];
    TH1F *JPjpTB[20];
    TH1F *JPjpTC[20];

    TH1F *qqjpTA[20];
    TH1F *qqjpTB[20];
    TH1F *qqjpTC[20];
    TH1F *qqjpTD[20];
    TH1F *qqPYjpTA[20];
    TH1F *qqRAWjpTA[20];
    TH1F *qqHTjpTA[20];
    TH1F *qqJPjpTA[20];
    TH1F *qqHTjpTB[20];
    TH1F *qqHTjpTC[20];
    TH1F *qqHTjpTD[20];

    TH1F *qgjpTA[20];
    TH1F *qgjpTB[20];
    TH1F *qgjpTC[20];
    TH1F *qgjpTD[20];
    TH1F *qgPYjpTA[20];
    TH1F *qgRAWjpTA[20];
    TH1F *qgHTjpTA[20];
    TH1F *qgJPjpTA[20];
    TH1F *qgHTjpTB[20];
    TH1F *qgHTjpTC[20];
    TH1F *qgHTjpTD[20];

    TH1F *ggjpTA[20];
    TH1F *ggjpTB[20];
    TH1F *ggjpTC[20];
    TH1F *ggjpTD[20];
    TH1F *ggPYjpTA[20];
    TH1F *ggRAWjpTA[20];
    TH1F *ggHTjpTA[20];
    TH1F *ggJPjpTA[20];
    TH1F *ggHTjpTB[20];
    TH1F *ggHTjpTC[20];
    TH1F *ggHTjpTD[20];

    TH2F *hs[4];
    TH2F *qqs[4];
    TH2F *qgs[4];
    TH2F *ggs[4];
    TH2F *RAWs[4];
    TH2F *qqRAWs[4];
    TH2F *qgRAWs[4];
    TH2F *ggRAWs[4];
    TH2F *HTs[4];
    TH2F *JPs[4];
    TH2F *qqHTs[4];
    TH2F *qgHTs[4];
    TH2F *ggHTs[4];
    TH2F *qqJPs[4];
    TH2F *qgJPs[4];
    TH2F *ggJPs[4];

    TH2F *ht[4];
    TH2F *qqt[4];
    TH2F *qgt[4];
    TH2F *ggt[4];
    TH2F *RAWt[4];
    TH2F *qqRAWt[4];
    TH2F *qgRAWt[4];
    TH2F *ggRAWt[4];
    TH2F *HTt[4];
    TH2F *JPt[4];
    TH2F *qqHTt[4];
    TH2F *qgHTt[4];
    TH2F *ggHTt[4];
    TH2F *qqJPt[4];
    TH2F *qgJPt[4];
    TH2F *ggJPt[4];

    TH2F *hu[4];
    TH2F *qqu[4];
    TH2F *qgu[4];
    TH2F *ggu[4];
    TH2F *RAWu[4];
    TH2F *qqRAWu[4];
    TH2F *qgRAWu[4];
    TH2F *ggRAWu[4];
    TH2F *HTu[4];
    TH2F *JPu[4];
    TH2F *qqHTu[4];
    TH2F *qgHTu[4];
    TH2F *ggHTu[4];
    TH2F *qqJPu[4];
    TH2F *qgJPu[4];
    TH2F *ggJPu[4];

    TH2F *hhard[4];
    TH2F *qqhard[4];
    TH2F *qghard[4];
    TH2F *gghard[4];
    TH2F *RAWhard[4];
    TH2F *qqRAWhard[4];
    TH2F *qgRAWhard[4];
    TH2F *ggRAWhard[4];
    TH2F *HThard[4];
    TH2F *JPhard[4];
    TH2F *qqHThard[4];
    TH2F *qgHThard[4];
    TH2F *ggHThard[4];
    TH2F *qqJPhard[4];
    TH2F *qgJPhard[4];
    TH2F *ggJPhard[4];

    TH2F *hx1[4];
    TH2F *qqx1[4];
    TH2F *qgx1[4];
    TH2F *ggx1[4];
    TH2F *RAWx1[4];
    TH2F *qqRAWx1[4];
    TH2F *qgRAWx1[4];
    TH2F *ggRAWx1[4];
    TH2F *HTx1[4];
    TH2F *JPx1[4];
    TH2F *qqHTx1[4];
    TH2F *qgHTx1[4];
    TH2F *ggHTx1[4];
    TH2F *qqJPx1[4];
    TH2F *qgJPx1[4];
    TH2F *ggJPx1[4];

    TH2F *hx2[4];
    TH2F *qqx2[4];
    TH2F *qgx2[4];
    TH2F *ggx2[4];
    TH2F *RAWx2[4];
    TH2F *qqRAWx2[4];
    TH2F *qgRAWx2[4];
    TH2F *ggRAWx2[4];
    TH2F *HTx2[4];
    TH2F *JPx2[4];
    TH2F *qqHTx2[4];
    TH2F *qgHTx2[4];
    TH2F *ggHTx2[4];
    TH2F *qqJPx2[4];
    TH2F *qgJPx2[4];
    TH2F *ggJPx2[4];

    TH2F *costh[4];
    TH2F *qqcosth[4];
    TH2F *qgcosth[4];
    TH2F *ggcosth[4];
    TH2F *RAWcosth[4];
    TH2F *qqRAWcosth[4];
    TH2F *qgRAWcosth[4];
    TH2F *ggRAWcosth[4];
    TH2F *HTcosth[4];
    TH2F *JPcosth[4];
    TH2F *qqHTcosth[4];
    TH2F *qgHTcosth[4];
    TH2F *ggHTcosth[4];
    TH2F *qqJPcosth[4];
    TH2F *qgJPcosth[4];
    TH2F *ggJPcosth[4];

    TH2F *HTshapeTow,*HTshapeTrk;
    TH2F *qqHTshapeTow,*qqHTshapeTrk;
    TH2F *qgHTshapeTow,*qgHTshapeTrk;
    TH2F *ggHTshapeTow,*ggHTshapeTrk;
    TH2F *JPshapeTow,*JPshapeTrk;
    TH2F *qqJPshapeTow,*qqJPshapeTrk;
    TH2F *qgJPshapeTow,*qgJPshapeTrk;
    TH2F *ggJPshapeTow,*ggJPshapeTrk;

    TH2F *TScale[5];
    TH2F *qqTScale[5];
    TH2F *qgTScale[5];
    TH2F *ggTScale[5];
    TH2F *AScale[5];
    TH2F *qqAScale[5];
    TH2F *qgAScale[5];
    TH2F *ggAScale[5];
    TH1F *js[20];

    TH1F *AssocA[2];
    TH2F *Assoc2A[2];
    TH1F *AssocB[2];
    TH2F *Assoc2B[2];
    TH1F *AssocC[2];
    TH2F *Assoc2C[2];
    TH1F *AssocD[2];
    TH2F *Assoc2D[2];
    TH2F *matrix[4];


   //Event histograms
    TH2F *trig;
    TH1F *tk[20];
    TH1F *tkA[10];
    TH1F *tkB[10];
    TH1F *tkC[10];
    TH1F *tkD[10];
    TH1F *py[40];
    TH1F *pyB[40];
    TH1F *pyH[40];
    TH1F *pyBH[40];
    TH1F *pyBHJ[40];
    TH1F *jet[10];
    TH2F *multEtSum[5];
    TH1F *pidH;

 private:
    JetBranchesMap mStJetsMap;
    TFile* mFile;
    TFile* sFile;
    TTree* mTree;
    TTree *stree;
    StMuDstMaker* mDstMaker;
    int mCounter;
    int EveNum1;
    int EveNum2;
    int EveNum3;
    int EveNum4;
    int EveNum5;
    int EveNum6;

    static const int HTthres1=320; //Corresponds to DSM=10
    static const int nbins=13;
    static const int abins=200;
    float HTETthres1;//Corresponds to software theshold of 4 GeV
    int SimuSwitch;
    int EventSwitch;
    int ConeYieldSwitch;
    int EnergyScaleSwitch;
    int CheckPDFSwitch;
    int AsymBiasSwitch;
    float GEta[10];
    float GPhi[10];
    float GpT[10];
    float PEta[10];
    float PPhi[10];
    float PpT[10], PEt[10],PNeu[10];
    char cond;
    
    //Simu tree variables
    int evtid;//event id from MuDst
    int pid;//subprocess id
    int BHTmax;//Barrel HT in event
    int BJPmax;//Barrel max JP in event
    int BJPsum;//Barrle JP sum in event
    int EHTmax;//Endcap HT in event
    int EJPmax;//Endcap max JP in event
    int EJPsum;//Endcap JP sum in event
    int BJP[12];//Barrel JP Sums
    int EJP[6];//EEMC JP Sums
    int bbc;//1 if BBC trigger is met
    int Badc[48];//BBC pmt adc
    float TowHtEt[20];//Highest Et tower in an eta ring
    float ss,tt,uu,cos_th,hard_p,x1,x2;
    float EtThr[20];//Effective Et Trigger Thresh for 2004 BEMC
    float Max_Part_Et,Max_pT,Max_id,Max_eta;
    float Max_A_Et,Max_A_id,Max_A_pT,Max_A_eta;
    float Max_C_Et,Max_C_id,Max_C_pT,Max_C_eta;
    float Max_N_Et,Max_N_id,Max_N_pT,Max_N_eta;
    float Max_P_Et,Max_P_id,Max_P_pT,Max_P_eta;
    float Sum_pT,Sum_Et,Alex_ht_Et;
    int Alex_ht_id, Alex_ht_DSM, mult;
    int JP1_2004,JP1_2004_Patch,JP1_2004_DSM;
    int HT1_2004,HT1_2004_Tow,HT1_2004_DSM;
    int nTrk1, nTrk2, nTrk3, nTrk4;
    double weight,df1,df2,f1,f2,partonic_all,Q2;
    int flavor1,flavor2,flavor3,flavor4;
    int Row[50];
    float NeuEt[50];

 //temp, MLM
    ofstream* mOfstream; //!

    
    ClassDef(StJetSimuReader,1)
	};

//inlines ---

inline StJetSimuReader::JetBranchesMap& StJetSimuReader::jetsMap()
{
    return mStJetsMap;
}





#endif
