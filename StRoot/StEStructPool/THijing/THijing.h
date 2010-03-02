/**********************************************************************
 *
 * Author: Chunhui Han
 *
 **********************************************************************/
extern "C" {
  void hijset_(float *efrm, char *frame, char *proj, char *targ, int *iap, int *izp, int *iat, int *izt);
  void hijing_(char *frame, float *bmin, float *bmax);
  void rluxgo_(int *lux,int *iseed, int *k1, int *k2);
  void rluxat_(int *lux,int *iseed, int *k1, int *k2);
  void rluxut_(int *ivec);
  void rluxin_(int *ivec);
  void hijev_();
  // definition for the common blocks in Hijing
  extern struct {
    int  length;
    char fileName[256];
  } hfilename_;
  extern struct {
    int natt;
    float eatt;
    int jatt;
    int nt;
    int np;
    int n0;
    int n01;
    int n10;
    int n11;
  } himain1_;
  extern struct {
    int katt[6][130000];
    float patt[5][130000];
  } himain2_;
  extern struct{
    float hipr1[100];
    int ihpr2[50];
    float hint1[100];
    int ihnt2[50];
  } hiparnt_;
  extern struct {
    int npj[300];
    int kfpj[500][300];
    float pjpx[500][300];
    float pjpy[500][300]; //
    float pjpz[500][300];
    float pjpe[500][300];
    float pjpm[500][300];
    int ntj[300];
    int kftj[500][300];
    float pjtx[500][300];
    float pjty[500][300];
    float pjtz[500][300];
    float pjte[500][300];
    float pjtm[500][300];
  } hijjet1_;
  extern struct {
    int nsg;
    int njsg[900];
    int iasg[3][900];
    int k1sg[100][900];
    int k2sg[100][900];
    float pxsg[100][900];
    float pysg[100][900];
    float pzsg[100][900];
    float pesg[100][900];
    float pmsg[100][900];
  } hijjet2_;
  // definition of the common block used in hijev.f
  extern struct {
    float psshep[5];
    float vsshep[4];
    int ifirst;
    int irun;
  } headpss_;
}

class THijing {
  void init();
  int mNevent;
 public:
  THijing( const char *paramFile );
  ~THijing();
  void SetRandomSeed(int iseed, int k1=0, int k2=0);
  void GetRandomSeed(int *lux, int *iseed, int *k1, int *k2);
  void SaveRandomSeeds(int *ivec);
  void RestoreRandomSeeds(int *ivec);
  void GenerateEvent();
  int EventsToDo()      const { return int(headpss_.vsshep[3]); }
  int EventsGenerated() const { return mNevent; }
  void writeOut() const;
  int GetNParticles() const { return himain1_.natt; }
  float GetPx(int i) const { return   (i < himain1_.natt) ? himain2_.patt[0][i] : 0.; }
  float GetPy(int i) const { return   (i < himain1_.natt) ? himain2_.patt[1][i] : 0.; }
  float GetPz(int i) const { return   (i < himain1_.natt) ? himain2_.patt[2][i] : 0.; }
  float GetVx(int i) const { return 0.; }
  float GetVy(int i) const { return 0.; }
  float GetVz(int i) const { return 0.; }
  float GetE(int  i) const { return   (i < himain1_.natt) ? himain2_.patt[3][i] : 0.; }
  int GetPdg(int  i) const { return   (i < himain1_.natt) ? himain2_.katt[0][i] : 0;  }
  int GetStatus(int i) const { return (i < himain1_.natt) ? himain2_.katt[3][i] : 0;  }
  int GetOrigin(int i) const { return (i < himain1_.natt) ? himain2_.katt[1][i] : -1; }
  int GetNStableParticles() const { return himain1_.natt; }
  int GetNWoundedTargNucleons() const { return himain1_.nt; }
  int GetNWoundedProjNucleons() const { return himain1_.np; }
  int GetParticipants() const { return himain1_.np+himain1_.nt; }
  int GetBinaryCollisions() const {
      return himain1_.n0+himain1_.n01+himain1_.n10+himain1_.n11;
  }
  float GetImpactParameter() const { return hiparnt_.hint1[18]; }
  float GetOrientation() const { return hiparnt_.hint1[19]; }
};
