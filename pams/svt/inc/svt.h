# define     VERTEX_LENGTH     30
# define     VERTEX_WIDTH       1
# define     STEP              60
# define     PI         3.1415926
# define  NEV    10    // number of events treated at a time
# define  NIT   200    // maximal allowed number of iterations

struct Point
  {
   float X, Y, Z;
   int nl, nw, np;
  };


class waffer
{
  public:
  float dx, dy, dz, alpha, beta, gamma;
  float R, X0;
  int num, max;
  float* points;
  waffer(float r, float x0) { init(r, x0); }
  void init(float r, float x0);
  ~waffer();
  void Clear();
  int NumPoints()  { return num;  }
  int PutPoint(float x, float y);
  void SetX(int n, float x) { if(n < num) points[2*n] = x; }
  void SetY(int n, float y) { if(n < num) points[2*n+1] = y; }
  float GetX(int n) { return n < num ? points[2*n] : 0; }
  float GetY(int n) { return n < num ? points[2*n+1] : 0; }
  Point GetPoint(int n);
  Point WhichPoint(Point P1, Point P2);
  void SetParam(float* par);
  void ReadData(FILE* f);
  void WriteData(FILE* f);
  void ReadParam(FILE* f);
  void WriteParam(FILE* f);
};


class ladder
{
  public:
  float R;
  int num_waf;
  waffer* waffers;
  ladder(float r, int n_waf) { init(r, n_waf); }
  void init(float r, int n_waf);
  ~ladder();
  void Clear();
  int NumPoints(int k)
        { return k < num_waf ? waffers[k].NumPoints() : 0; }
  int PutPoint(int k, float x, float y)
		  { return k < num_waf ? waffers[k].PutPoint(x, y) : 0; }
  void SetX(int k, int n, float x)
		  { if(k < num_waf) waffers[k].SetX(n, x); }
  void SetY(int k, int n, float y)
		  { if(k < num_waf) waffers[k].SetY(n, y); }
  float GetX(int k, int n)
		  { return k < num_waf ? waffers[k].GetX(n) : 0; }
  float GetY(int k, int n)
		  { return k < num_waf ? waffers[k].GetY(n) : 0; }
  Point GetPoint(int k, int n);
  Point WhichWaffer(Point P1, Point P2);
  void SetParam(int k, float* par)
        { if(k < num_waf) waffers[k].SetParam(par); }
  void ReadData(FILE* f);
  void WriteData(FILE* f);
  void ReadParam(FILE* f);
  void WriteParam(FILE* f);
};

class barrel
{
  public:
  int num_lad;
  float* CosTheta;
  float* SinTheta;
  ladder* ladders;
  barrel(float r_odd, float r_even, int n_lad, int n_waf)
     { init(r_odd, r_even, n_lad, n_waf); }
  void init(float r_odd, float r_even, int n_lad, int n_waf);
  ~barrel();
  void Clear();
  int NumPoints(int l, int k)
        { return l < num_lad ? ladders[l].NumPoints(k) : 0; }
  int PutPoint(int l, int k, float x, float y)
		  { return l < num_lad ? ladders[l].PutPoint(k, x, y) : 0; }
  void SetX(int l, int k, int n, float x)
		  { if(l < num_lad) ladders[l].SetX(k, n, x); }
  void SetY(int l, int k, int n, float y)
		  { if(l < num_lad) ladders[l].SetY(k, n, y); }
  float GetX(int l, int k, int n)
		  { return l < num_lad ? ladders[l].GetX(k, n) : 0; }
  float GetY(int l, int k, int n)
		  { return l < num_lad ? ladders[l].GetY(k, n) : 0; }
  Point GetPoint(int l, int k, int n);
  int WhichLadder(Point P1, Point P2);
  Point WhichWaffer(Point P1, Point P2, int n_lad);
  void SetParam(int l, int k, float* par)
      { if(l < num_lad) ladders[l].SetParam(k, par); }
  void ReadData(FILE* f);
  void WriteData(FILE* f);
  void ReadParam(FILE* f);
  void WriteParam(FILE* f);
};


  struct  track
{
  Point A, B, P[3];
  float chi2, w;
  int flag;
};

  class event
{
  public:
  barrel* Bars;
  Point Vertex;
  int num, max;
  track*  tracks;
 // event();


 event(float r1, float r2, float r3, float r4, float r5, float r6)
      { init(r1, r2, r3, r4, r5, r6); }
  void init(float r1, float r2, float r3, float r4, float r5, float r6);




  ~event();
  void Clear()  { num = 0;  }
  void FindAllTracks();
  void FindTracks(int nl, int nw, Point P);
  int AddTrack(Point PP1, Point PP2);
  int IsTrackGood(int n);
  void DeleteTrack(int n);
  void DeleteLastTrack() { if(num) num--; }
  void OptimizeTracks();
  void SelectTracks();
  void SelectTracks(float cat);
  void SelectTracks3();
  void OptimizeTracks2();
  void OptimizeTracks3();
  void FindVertex();
  float CorrectTracks(int bn, int ln, int wn, float* par);
};

class work
{
  public:
  event* Events;
  float chi0;           // current value of total chi^2
  float chin;           // the value of total chi^2 with
  float derm;           // maximal derivatives value
  float  rx[NEV];
  float  ry[NEV];
  float  rz[NEV];       // vertex parameters (true value)

  //float  par0[8][4][6]; float  par1[12][6][6]; float  par2[16][7][6]; arrays of parameters values
   
   float par[3][16][7][6]; 
    
  float  der0[8][4][6]; 
  float  der1[12][6][6];
  float  der2[16][7][6]; // arrays of derivetives values
  
 

//  work();


  work(float r1, float r2, float r3, float r4, float r5, float r6)
      { init(r1, r2, r3, r4, r5, r6); }
  void init(float r1, float r2, float r3, float r4, float r5, float r6);



  ~work();
  void InstallParam();
  void MakeAlign();

  void SigmaVertex();
  void ClearParam();
  float TotalChi();
  void ShiftParams(float sh);
  float CorrectTracks(int bn, int ln, int wn, float* pp);
  float TakeDerivatives(float st);

int myrandom( int n );
float normal();
float Check(float a);
void GenerateEvent(int n_ev);
void ChangeGeom(int i);


};
