// here all counters & histogram for evaluation of the Leading Particle analysis are defined
#define MX_LPA 2 // I consider the leading & next to leading charged particle
#define MX_GLPT 15 //  LP pT range bins

struct RecStep
{
  int n;
  TH1D *h;
};

struct LeadPartAnal
{
  struct  RecStep Inp, Trig, Acc, dPT, dPsi; // keep them alike
};
//, Vert, Det, Rec,Prim, Lead, DrPt

static  char *tt1[]={"Inp","Trig","Acc", "dPT", "dPsi"};

