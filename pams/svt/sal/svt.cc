# include <stdio.h>
# include <stdlib.h>
# include <time.h>
# include <math.h>

# include "svt.h"


//*******************************************************************
//     Class waffer
//*******************************************************************
// init       : initializes wafer object and reserves memory for 30
//              points
// parameters : r  - distance from the wafer center
//              x0 - wafer center position
//*******************************************************************
  void waffer::init(float r, float x0)
{
  dx = dy = dz = alpha = beta = gamma = 0;
  R = r; X0 = x0;
  num = 0;  max = 30;
  points = (float*)malloc(2*max*sizeof(float));
  if(!points) { printf("Waffer : Memory Error !\n"); exit(0); }
}

//*******************************************************************
// ~waffer     : destructor of wafer object, frees the memory
//*******************************************************************
 waffer::~waffer()
{
  if(points) free(points);
}

//*******************************************************************
// Clear      : clears the memory for cases of sequential treatment
//              of several events
//*******************************************************************
 void waffer::Clear()
{
	num = 0;  max = 10;
	points =(float*)realloc(points, 2*max*sizeof(float));
	if(!points)  { printf("Waffer : Memory Error !\n");  exit(0); }
}

//*******************************************************************
  int waffer::PutPoint(float x, float y)
{
	if(num == max)
	  {
		  max += 10;
		  points = (float*)realloc(points, 2*max*sizeof(float));
	  }
	points[2*num] = x;
	points[2*num+1] = y;
	return num++;
}

//*******************************************************************
// PutPoint   : adds points to the wafer data array using local
//              coordinate system
// parameters : x - the value of x coordinate
//              y - the value of y coordinate
// return     : number of the points in the wafer data array
//*******************************************************************
  Point waffer::GetPoint(int n)
{
   Point P = {0,0,0,0,0,0};

	if(n >= num) return P;
	P.X = X0 + GetX(n) + dx - GetY(n)*alpha;
	P.Y = GetY(n) + dy + GetX(n)*alpha;
	P.Z = R + dz - GetX(n)*beta - GetY(n)*gamma;
   P.np = n;
   return P;
}

//*******************************************************************
// WhichPoint   : defines local coordinates of track and wafer
//                intersection point
// parameters   : P1, P2 - points belonging to the track
// return       : Point P
//*******************************************************************
  Point waffer::WhichPoint(Point P1, Point P2)
{
   Point P = {0,0,0,0,0,0};

	P.X = P1.X + (P2.X - P1.X)*(R - P1.Z)/(P2.Z - P1.Z) - X0;
	P.Y = P1.Y + (P2.Y - P1.Y)*(R - P1.Z)/(P2.Z - P1.Z);

   P.X = P1.X + (P2.X - P1.X)*(R + dz - beta*P.X - gamma*P.Y - P1.Z)/
       (P2.Z - P1.Z) - X0 - dx;
   P.Y = P1.Y + (P2.Y - P1.Y)*(R + dz - beta*P.X - gamma*P.Y - P1.Z)/
       (P2.Z - P1.Z) - dy;

   P.X += P.Y*alpha;  P.Y -= P.X*alpha;
   return P;
}

//*******************************************************************
// SetParam   : gives values to the current alignment parameters
// parameters : par    - array of  parameters,
//              par[0] - shift along x axis in the local coordinates
//                       system
//              par[1] - shift along  y axis
//              par[2] - shift along the axis perpendicular to the
//                       wafer plane
//              par[3] - rotation in wafer plane from x axis to y
//                       one
//              par[4] - rotation around y axis of the local
//                       coordinate system from z axis of
//                       intermediate system to x axis of local
//                       system
//              par[5] - rotation around x axis of the local
//                       coordinate system from z axis of
//                       intermediate system to y axis of local
//                       system
//*******************************************************************
  void waffer::SetParam(float* par)
{
   dx = par[0]; dy = par[1]; dz = par[2];
   alpha = par[3]; beta = par[4]; gamma = par[5];
}

//*******************************************************************
// ReedData   : reading data (point coordinates of each wafer) from
//              the file
// parameter   : opened file pointer
//
//*******************************************************************
  void waffer::ReadData(FILE* f)
{
	char s[40];

	fgets(s, 40, f);
	sscanf(s, "%d", &num);
	if(num > 10)
	  {
		 max = num;
		 points = (float*)realloc(points, 2*max*sizeof(float));
	  }
	for(int i = 0; i < num; i++)
	  {
		  fgets(s, 40, f);
		  sscanf(s, "%f %f", &points[2*i], &points[2*i+1]);
	  }
}

//*******************************************************************
//  WriteData : writes all wafer points in the file; format is the
//              same as in the reading procedure
//  parameter : opened file pointer
//*******************************************************************
  void waffer::WriteData(FILE* f)
{
	fprintf(f, "%d\n", num);
	for(int i = 0; i < num; i++)
		fprintf(f, "%.4f %.4f\n", points[2*i], points[2*i+1]);
}

//*******************************************************************
// ReadParam   : reading initial values of alignment parametrers for
//               each wafer
// parameter   : opened file pointer
//*******************************************************************
  void waffer::ReadParam(FILE* f)
{
   char s[80];

   fgets(s, 80, f);
   sscanf(s, "%f %f %f %f %f %f", &dx, &dy, &dz, &alpha, &beta, &gamma);
}

//*******************************************************************
// WriteParam  : writeng of the found alignment parameters
//               in the file
// parameter   : opened file pointer
//*******************************************************************
  void waffer::WriteParam(FILE* f)
{
   fprintf(f, "%.4f %.4f %.4f %.4f %.4f %.4f\n",
           dx, dy, dz, alpha, beta, gamma);
}


//*******************************************************************
//    Class ladder
//*******************************************************************
// init      : initializes  ladder object and all its wafers
// parameters: r     - radial distance from the ladder center
//             n_waf - number of wafers in the ladder
//**********************************************
  void ladder::init(float r, int n_waf)
{
  R = r;  num_waf = n_waf;
  waffers = (waffer*)malloc(n_waf*sizeof(waffer));
  if(!waffers) { printf("Ladder %d : Memory Error !\n", n_waf); exit(0); }
  float x0 = - 0.5*STEP*(num_waf - 1);
  for(int i = 0; i < n_waf; i++)
	 waffers[i].init(r, x0 + i*STEP);
}

//*******************************************************************
// ~ladder     : destructor of ladder object, calls destructors
//               of all its wafers
//*******************************************************************
  ladder::~ladder()
{
  if(waffers)
	{
	  for(int i = 0; i < num_waf; i++) waffers[i].~waffer();
	  free(waffers);
	}
}

//*******************************************************************
// Clear   : clears the memory for sequential treatment of several
//           events, calls Clear() procedure for each wafer
//*******************************************************************
  void ladder::Clear()
{
	for(int i = 0; i < num_waf; i++)  waffers[i].Clear();
}

//*******************************************************************
// GetPoint   : puts intermediate coordinates of n-th point from k-th
//              wafer in Point P, calls GetPoint function of the
//              needed wafer
// parameters : k - wafer number
//              n - ordinal number of the point on this wafer
//*******************************************************************
  Point ladder::GetPoint(int k, int n)
{
   Point P = {0,0,0,0,0,0};

   if(k < num_waf)
     {
        P = waffers[k].GetPoint(n);
        P.nw = k;
     }
   return P;
}

//*******************************************************************
// WhichWaffer   : defines which of its wafers is intersected by the
//                 track, calls WhichPoint function of this wafer
// parameters    :   P1, P2 - points belonging to the track
// return        : Point P - local coordinates of intersection point(?)
//*******************************************************************
  Point ladder::WhichWaffer(Point P1, Point P2)
{
   int n_waf;
	Point P = {0,0,0,0,0,0};

	n_waf = ( P1.X + (P2.X - P1.X)*(R - P1.Z)/(P2.Z - P1.Z) +
				 0.5*num_waf*STEP)/STEP;

   if(n_waf >= num_waf) n_waf = num_waf - 1;
   if(n_waf < 0)  n_waf = 0;

	P = waffers[n_waf].WhichPoint(P1, P2);
   P.nw = n_waf;
	return P;
}

//*******************************************************************
// ReadData    : data reading from the file, calls procedure with the
//               same name for each of its wafer
// parameter   : opened file pointer
//*******************************************************************
  void ladder::ReadData(FILE* f)
{
	for(int i = 0; i < num_waf; i++)
		waffers[i].ReadData(f);
}

//*******************************************************************
// WriteData   : writes data in the file, calls procedure with  the
//               same name for each of its wafer
// parameter   : opened file pointer
//*******************************************************************
  void ladder::WriteData(FILE* f)
{
	for(int i = 0; i < num_waf; i++)
		waffers[i].WriteData(f);
}

//*******************************************************************
// ReadParam  : reades initial values of alignment parametrs for each
//              of its wafer, calls procedure with the same name for
//              each of its wafers
// parameter  : opened file pointer
//*******************************************************************
  void ladder::ReadParam(FILE* f)
{
	for(int i = 0; i < num_waf; i++)
		waffers[i].ReadParam(f);
}

//*******************************************************************
// WriteParam  : writes the found alignment parameters in the file,
//               calls procedure with the same name for each of
//               its wafers
// parameter   : opened file pointer
//*******************************************************************
  void ladder::WriteParam(FILE* f)
{
	for(int i = 0; i < num_waf; i++)
		waffers[i].WriteParam(f);
}

//*******************************************************************
//  Class barrel
//*******************************************************************
// init       : initializes barrel object, reserves memory for all
//              its ladders and wafers, makes up the table for sinus
//              and cosines
// parameters : r_odd  - radial distance  from the center of odd
//                       ladders
//              r_even - radial distance from the center of even
//                       ladders
//              n_lad - number of ladders in the  barrel
//              n_waf - number of  wafers in the each ladder
//*******************************************************************
  void barrel::init(float r_odd, float r_even, int n_lad, int n_waf)
{
  int i;

  num_lad = n_lad;
  ladders = (ladder*)malloc(n_lad*sizeof(ladder));
  if(!ladders) { printf("Barrel : Memory Error !\n"); exit(0); }
  for(i = 0; i < n_lad/2; i++)
	 {
		ladders[2*i].init(r_even, n_waf);
		ladders[2*i+1].init(r_odd, n_waf);
	 }

  CosTheta = (float*)malloc(2*n_lad*sizeof(float));
  SinTheta = (float*)malloc(2*n_lad*sizeof(float));
  if (!CosTheta || !SinTheta)
	 { printf("Theta : Memory Error !\n"); exit(0); }
  for(i = 0; i < 2*n_lad; i++)
	 {
		 CosTheta[i] = cos(PI*i/n_lad);
		 SinTheta[i] = sin(PI*i/n_lad);
	 }
}

//*******************************************************************
//~barrel   : destruktor of barrel objekt, calls destructor procedure
//            for all its ladders
//*******************************************************************
  barrel::~barrel()
{
  if(ladders)
	 {
		for(int i = 0; i < num_lad; i++)  ladders[i].~ladder();
		free(ladders);
	 }
  if(SinTheta) free(SinTheta);
  if(CosTheta) free(CosTheta);
}

//*******************************************************************
// Clear     : clears the memory for sequential treatment of several
//             events, calls Clear procedure for each wafer
//*******************************************************************
  void barrel::Clear()
{
	for(int i = 0; i < num_lad; i++)  ladders[i].Clear();
}

//*******************************************************************
// GetPoint   : defines the coordinates of point from one of its
//              wafers in the global coordinate system, calls
//              GetPoint function of the needed ladder and
//              recalculates coordinates from intermediate to global
//              system
// parameters:  l -  ladder number
//              k -  wafer number
//              n -  ordinary number of polint
// return     : Point P - coordinates of n-th point in the global
//              coordinate system
//*******************************************************************
  Point barrel::GetPoint(int l, int k, int n)
{
	Point P, P0 = {0,0,0,0,0,0};

	if(l > num_lad) return P0;
	P0 = ladders[l].GetPoint(k, n);
	P.X = P0.X;
	P.Y = P0.Y*SinTheta[2*l] + P0.Z*CosTheta[2*l];
	P.Z = - P0.Y*CosTheta[2*l] + P0.Z*SinTheta[2*l];
   P.nl = l; P.nw = P0.nw;  P.np = P0.np;
   return P;
}

//*******************************************************************
// WhichLadder : defines which of its ladders is intersected by the
//               track
// parametry   : P1, P2 - points belonging the track (the coordinates
//                        of this point is in the global coordinate
//                        system
// return      : ladder number
//*******************************************************************
  int barrel::WhichLadder(Point P1, Point P2)
{
	int i;
	float c, s, r;

	r = sqrt((P2.Z - P1.Z)*(P2.Z - P1.Z) + (P2.Y - P1.Y)*(P2.Y - P1.Y));
	c = (P2.Y - P1.Y)/r;
	s = (P2.Z - P1.Z)/r;
	for(i = 0; (i < num_lad/2) && (c < CosTheta[2*i+1]); i++);
	if(i == 0) return i;
	else if(s > 0) return i;
	else  return (num_lad - i);
}

//*******************************************************************
// WhichWaffer  : defines which of its wafers is intersected by the
//                track, and local coordinates of intersection point
//                (recalculates the coordinates of point P1, P2 from
//                global to intermediate system and calls WhichWaffer
//                function of the needed ladder)
// parameters   : P1, P2 -  points belonging to the track (the
//                          coordinates of this point is in the
//                          global coordinate system
//                n       - number of ladder which is intersected by
//                          the track
// return       : P - local coordinates of intersection point
//*******************************************************************
  Point barrel::WhichWaffer(Point P1, Point P2, int n_lad)
{
	Point P3, P4, P;

	P3.X = P1.X;
	P3.Y = P1.Y*SinTheta[2*n_lad] - P1.Z*CosTheta[2*n_lad];
	P3.Z = P1.Y*CosTheta[2*n_lad] + P1.Z*SinTheta[2*n_lad];
	P4.X = P2.X;
	P4.Y = P2.Y*SinTheta[2*n_lad] - P2.Z*CosTheta[2*n_lad];
	P4.Z = P2.Y*CosTheta[2*n_lad] + P2.Z*SinTheta[2*n_lad];

	P = ladders[n_lad].WhichWaffer(P3, P4);
   P.nl = n_lad;
   return P;
}

//*******************************************************************
// ReadData   : reads data from the file, calls procedure with the
//              same name for each of its ladders
// parameter  : opened file pointer
//*******************************************************************
  void barrel::ReadData(FILE* f)
{
	for(int i = 0; i < num_lad; i++)
	  ladders[i].ReadData(f);
}

//*******************************************************************
// WriteData   : writes data in the file, calls procedure with the
//               same name for each of its ladders
// parameter   : opened file pointer
//*******************************************************************
  void barrel::WriteData(FILE* f)
{
	for(int i = 0; i < num_lad; i++)
	  ladders[i].WriteData(f);
}

//*******************************************************************
// ReadParam   : reads initial value of alignment parameters for each
//               of its ladders calls procedure with the same name
//               for each of its ladders
// parameter    : opened file pointer
//*******************************************************************
  void barrel::ReadParam(FILE* f)
{
	for(int i = 0; i < num_lad; i++)
	  ladders[i].ReadParam(f);
}

//*******************************************************************
// WriteParam  : writes found alignment parameters in the file, calls
//               procedure with the same name for each of its ladders
// parameter   : opened file pointer
//*******************************************************************
  void barrel::WriteParam(FILE* f)
{
	for(int i = 0; i < num_lad; i++)
	  ladders[i].WriteParam(f);
}

//*******************************************************************
//      Class event
//*******************************************************************
//  event  :  constructor of event class, calls init procedure of
//            needed number of barrels, reserves memory for 2000
//            tracks
//*******************************************************************
 void event::init(float r1, float r2, float r3, float r4, float r5, float r6)

{
	num = 0;  max = 2000;
	tracks = (track*)malloc(max*sizeof(track));
	if(!tracks)  { printf("Event : memory error !\n");  exit(0); }

   Bars = (barrel*)malloc(3*sizeof(barrel));

   Bars[0].init(r2, r1, 8, 4);
   Bars[1].init(r4, r3, 12, 6);
   Bars[2].init(r6, r5, 16, 7);
}





//*******************************************************************
// ~event    : destructor of event class, frees the memory
//*******************************************************************
  event::~event()
{
	if (tracks)  free(tracks);
   Bars[0].~barrel();
   Bars[1].~barrel();
   Bars[2].~barrel();
   free(Bars);
}

//*******************************************************************
//  FindTracks :  auxilary procedure. Seeks tracks passing throught
//                the given point on the 3-rd barrel and the
//                predefined wafer of neede ladder on  the 1-st
//                barrel. If track is found it is added to the array.
// parameters :  nl - number of ladder from the 1-st barrel
//               nw - number of wafer from the 1-st barrel
//               P  - point from the 3-rd barrel
//*******************************************************************

  void event::FindTracks(int nl, int nw, Point P)
{
  int j, l, k, kf;
  float x, y, d, df;
  Point PP1, PP2;

  for(j = 0; j < Bars[0].NumPoints(nl, nw); j++)
  {
    PP1 = Bars[0].GetPoint(nl, nw, j);
    l = AddTrack(PP1, P);
    if(!IsTrackGood(l))
	{ DeleteLastTrack(); continue; }

    PP2.nl = Bars[1].WhichLadder(PP1, P);
    PP2 = Bars[1].WhichWaffer(PP1, P, PP2.nl);
    
    df = 10.0;
    for(k = 0; k < Bars[1].NumPoints(PP2.nl, PP2.nw); k++)
    {
      x = Bars[1].GetX(PP2.nl, PP2.nw, k);
      y = Bars[1].GetY(PP2.nl, PP2.nw, k);  
      d =  sqrt((x-PP2.X)*(x-PP2.X) + (y-PP2.Y)*(y-PP2.Y)); 
      if((d < 1.0)&&(df > d)) {df = d; kf = k;}
     }
   if(df > 1.0) DeleteLastTrack();
     else tracks[l].P[1] = Bars[1].GetPoint(PP2.nl, PP2.nw, kf);
  }
}

//*******************************************************************
//  FindAllTracks :  finds all tracks of the given event. For this
//                   purpose it investigates all points of the 3-rd
//                   barrel, for each point one or two wafers of the
//                   needed ladder (??)  on 1-st barrel are found,
//                   calls FindTracks
//*******************************************************************
  void event::FindAllTracks()
{
	int i, nl3, nw3, nw1;
	Point PP0, PP3, PP;

	Clear();

	PP0.Y = PP0.Z = 0;

	for(nl3 = 0; nl3 < 16; nl3++)
	  for(nw3 = 0; nw3 < 7; nw3++)
		 for(i = 0; i < Bars[2].NumPoints(nl3, nw3); i++)
			{
			  PP0.X = VERTEX_LENGTH;
			  PP3 = Bars[2].GetPoint(nl3, nw3, i);
			  PP.nl = Bars[0].WhichLadder(PP0, PP3);
			  PP = Bars[0].WhichWaffer(PP0, PP3, PP.nl);
			  FindTracks(PP.nl, PP.nw, PP3);
			  PP0.X = - VERTEX_LENGTH;
           nw1 = PP.nw;
			  PP = Bars[0].WhichWaffer(PP0, PP3, PP.nl);
			  if(nw1 != PP.nw)  FindTracks(PP.nl, PP.nw, PP3);
			}
}


//*******************************************************************
// AddTrack   :  adds new tracks, if necessary reserves supplementary
//               memory. Defines vector A, B, where track point
//               defined as A+lambda*B. Vector A is the minimal
//               distance to the beam axis.
// parameters :  PP1, PP2 - points belonging to the track from the
//                            1-st and 3-rd barrel respectively
//*******************************************************************
  int event::AddTrack(Point PP1, Point PP2)
{
  float lambda;
  track tmp;

  if(num == max)
	 {
		 max += 50;
		 tracks = (track*)realloc(tracks, max*sizeof(track));
	 }

  tmp.B.X = PP2.X - PP1.X;
  tmp.B.Y = PP2.Y - PP1.Y;
  tmp.B.Z = PP2.Z - PP1.Z;

  lambda = - (PP1.Y*tmp.B.Y + PP1.Z*tmp.B.Z)/
				 (tmp.B.Y*tmp.B.Y + tmp.B.Z*tmp.B.Z);

  tmp.A.X = PP1.X + lambda*(PP2.X - PP1.X);
  tmp.A.Y = PP1.Y + lambda*(PP2.Y - PP1.Y);
  tmp.A.Z = PP1.Z + lambda*(PP2.Z - PP1.Z);

  tmp.P[0] = PP1;
  tmp.P[2] = PP2;
  tmp.chi2 = 0;
  tracks[num] = tmp;
  return num++;
}

//*******************************************************************
//  IsTrackGood  : auxiliary procedure. Track considered to be good
//                 if it passes beam axis not farther than
//                 VERTEX_WIDHT distance and abs(x) coordinate is not
//                 more  then VERTEX_LENGTH
// parameter    :  n - track number
//*******************************************************************
  int event::IsTrackGood(int n)
{
	if(n >= num)  return 0;
	Point tmp = tracks[n].A;

	return  ((sqrt(tmp.Y*tmp.Y + tmp.Z*tmp.Z) < VERTEX_WIDTH) &&
			  (fabs(tmp.X) < VERTEX_LENGTH));
}

//*******************************************************************
//  DeleteTrack  :  removes  track from track data array
//  parameter    :  n - track number
//*******************************************************************
  void event::DeleteTrack(int n)
{
	if(n >= num)  return;
	for(int i = n; i < num-1; i++)  tracks[i] = tracks[i+1];
	num--;
}


//*******************************************************************
//  SelectTracks  :  preliminary vertex search. Interval on X axis
//                   divided into regions of 2 mm widht, then the
//                   region that is crossed by the maximal number of
//                   tracks is defined. All other tracks are removed.
//*******************************************************************
  void event::SelectTracks()
{
	int i, j, m, n[VERTEX_LENGTH];
	float a;

	for(i = 0; i < VERTEX_LENGTH; i++)  n[i] = 0;

	for(i = 0; i < num; i++)
	  n[(int)((tracks[i].A.X + VERTEX_LENGTH)/2)]++;

	j = 0; m = 0;
	for(i = 0; i < VERTEX_LENGTH; i++)
	  if(m < n[i]) {j = i; m = n[i]; }

	a = - VERTEX_LENGTH + 2*j + 1;

	for(i = 0; i < num; i++)   //bylo 1.8
//	  if((fabs(tracks[i].A.X - a) > 5) || (tracks[i].chi2 > 50))
//            DeleteTrack(i--);


         if((fabs(tracks[i].A.X - a) > 5) || (tracks[i].chi2 > 40))
              tracks[i].flag = 0;
            else tracks[i].flag =1;    
}
//*********************************************************************
//
//*********************************************************************
  void event::SelectTracks(float cat)
{
	int i, j, m, n[VERTEX_LENGTH];
	float a, c;
        
       
             c = 40+ 4*sqrt(fabs(cat-150));
        
	for(i = 0; i < VERTEX_LENGTH; i++)  n[i] = 0;

	for(i = 0; i < num; i++)
	  n[(int)((tracks[i].A.X + VERTEX_LENGTH)/2)]++;

	j = 0; m = 0;
	for(i = 0; i < VERTEX_LENGTH; i++)
	  if(m < n[i]) {j = i; m = n[i]; }

	a = - VERTEX_LENGTH + 2*j + 1;

	for(i = 0; i < num; i++)   
         if((fabs(tracks[i].A.X - a) > 5) || (tracks[i].chi2 > c))
             tracks[i].flag = 0;
            else tracks[i].flag = 1;    
}

//*******************************************************************
//  FindVertex  :  finedes the point with the minimal sum of the
//                 square of the distances to each track. After
//                 vertex finding is done, square of the distance
//                 between the vertex and track is added to chi^2
//                 of this track.
//*******************************************************************
  void event::FindVertex()
{
 int i;
 Point a,b;
 float c[6], d[3], e, g;

 for(i = 0; i < 6; i++)  c[i] = 0;
 d[0] = d[1] = d[2] = 0;
 for(i = 0; i < num; i++)
  {
		
    if(tracks[i].flag == 0) continue;
           
     a = tracks[i].A;
     b = tracks[i].B;
     e = b.X*b.X + b.Y*b.Y + b.Z*b.Z;
     g = a.X*b.X + a.Y*b.Y + a.Z*b.Z;
     c[0] += 1 - b.X*b.X/e;
     c[1] -= b.X*b.Y/e;
     c[2] -= b.X*b.Z/e;
     c[3] += 1 - b.Y*b.Y/e;
     c[4] -= b.Y*b.Z/e;
     c[5] += 1 - b.Z*b.Z/e;
     d[0] += a.X - g*b.X/e;
     d[1] += a.Y - g*b.Y/e;
     d[2] += a.Z - g*b.Z/e;
  }
 e = c[0]*c[3]*c[5] + 2*c[1]*c[2]*c[4] -
	 c[0]*c[4]*c[4] - c[3]*c[2]*c[2] - c[5]*c[1]*c[1];

 Vertex.X = (d[0]*(c[3]*c[5]-c[4]*c[4]) -
			d[1]*(c[5]*c[1] - c[2]*c[4]) +
			d[2]*(c[1]*c[4] - c[3]*c[2]))/e;
 Vertex.Y = (- d[0]*(c[1]*c[5] - c[2]*c[4]) +
			d[1]*(c[0]*c[5] - c[2]*c[2]) -
			d[2]*(c[0]*c[4] - c[1]*c[2]))/e;
 Vertex.Z = (d[0]*(c[1]*c[4] - c[2]*c[3]) -
			d[1]*(c[0]*c[4] - c[1]*c[2]) +
			d[2]*(c[0]*c[3] - c[1]*c[1]))/e;

 for(i = 0; i < num; i++)
  {
//    if(tracks[i].flag == 0) continue;
  
    a.X = Vertex.X - tracks[i].A.X;
    a.Y = Vertex.Y - tracks[i].A.Y;
    a.Z = Vertex.Z - tracks[i].A.Z;
    b = tracks[i].B;
    tracks[i].chi2 += 1000*(a.X*a.X + a.Y*a.Y + a.Z*a.Z -
	  		 (a.X*b.X + a.Y*b.Y + a.Z*b.Z)*
			 (a.X*b.X + a.Y*b.Y + a.Z*b.Z)/
			 (b.X*b.X + b.Y*b.Y + b.Z*b.Z));

  }

}


//*******************************************************************
//  OptimizeTracks  : optimizes all track in the track data array
//                    using three found track point, defines A and B
//                    vectors with the help of least square method.
//                    Saves  new value of A and B, calculates the
//                    track chi^2 for every tracks.
//*******************************************************************
  void event::OptimizeTracks()
{
  Point a, b, pp1, pp2, pp3;
  float l[3], c, e, g, f, h;
  float s[3] = {1, 1, 1};// {7.457, 2.933, 1.676};

  for(int i = 0; i < num; i++)
   {
     a = tracks[i].A;
     b = tracks[i].B;
     pp1 = tracks[i].P[0];
     pp2 = tracks[i].P[1];
     pp3 = tracks[i].P[2];
     
     pp1 = Bars[0].GetPoint(pp1.nl, pp1.nw, pp1.np);
     pp2 = Bars[1].GetPoint(pp2.nl, pp2.nw, pp2.np);
     pp3 = Bars[2].GetPoint(pp3.nl, pp3.nw, pp3.np);
  
     c = b.Y*b.Y + b.Z*b.Z;
     l[0] = s[0]*(pp1.Y*b.Y + pp1.Z*b.Z)/c;
     l[1] = s[1]*(pp2.Y*b.Y + pp2.Z*b.Z)/c;
     l[2] = s[2]*(pp3.Y*b.Y + pp3.Z*b.Z)/c;

     c = l[0]*l[0] + l[1]*l[1] + l[2]*l[2];
     e = l[0] + l[1] + l[2];
     g = 3*c - e*e;

     f = l[0]*pp1.X + l[1]*pp2.X + l[2]*pp3.X;
     h = pp1.X + pp2.X + pp3.X;
     a.X = (c*h - f*e)/g;
     b.X = (3*f - e*h)/g;

     f = l[0]*pp1.Y + l[1]*pp2.Y + l[2]*pp3.Y;
     h = pp1.Y + pp2.Y + pp3.Y;
     a.Y = (c*h - f*e)/g;
     b.Y = (3*f - e*h)/g;

     f = l[0]*pp1.Z + l[1]*pp2.Z + l[2]*pp3.Z;
     h = pp1.Z + pp2.Z + pp3.Z;
     a.Z = (c*h - f*e)/g;
     b.Z = (3*f - e*h)/g;

     c = (a.Y*b.Y + a.Z*b.Z)/(b.Y*b.Y + b.Z*b.Z);
     a.Y -= c*b.Y;  a.Z -= c*b.Z; a.X -= c*b.X;

     c = b.Y*b.Y + b.Z*b.Z;
     l[0] = (pp1.Y*b.Y + pp1.Z*b.Z)/c;
     l[1] = (pp2.Y*b.Y + pp2.Z*b.Z)/c;
     l[2] = (pp3.Y*b.Y + pp3.Z*b.Z)/c;

      g = 1000*((a.X + l[0]*b.X - pp1.X)*(a.X + l[0]*b.X - pp1.X) +
  	        (a.Y + l[0]*b.Y - pp1.Y)*(a.Y + l[0]*b.Y - pp1.Y) +
	        (a.Z + l[0]*b.Z - pp1.Z)*(a.Z + l[0]*b.Z - pp1.Z)) +
	  1000*((a.X + l[1]*b.X - pp2.X)*(a.X + l[1]*b.X - pp2.X) +
	        (a.Y + l[1]*b.Y - pp2.Y)*(a.Y + l[1]*b.Y - pp2.Y) +
		(a.Z + l[1]*b.Z - pp2.Z)*(a.Z + l[1]*b.Z - pp2.Z)) +
	  1000*((a.X + l[2]*b.X - pp3.X)*(a.X + l[2]*b.X - pp3.X) +
	        (a.Y + l[2]*b.Y - pp3.Y)*(a.Y + l[2]*b.Y - pp3.Y) +
		(a.Z + l[2]*b.Z - pp3.Z)*(a.Z + l[2]*b.Z - pp3.Z));


 //           + 200*(a.Z*a.Z + a.Y*a.Y);

tracks[i].P[0] = pp1;
tracks[i].P[1] = pp2;
tracks[i].P[2] = pp3;


		  tracks[i].A = a;
		  tracks[i].B = b;
        tracks[i].chi2 = g;
	  }
}

//*******************************************************************
// CorrectTraks   : recalculates tracks parameter in accodance with
//                  the found alignment parameters for all tracks
//                  which cross one wafer
// parameters     : bn -  barrel number
//                  ln -  ladder number
//                  wn -  wafer number
//                  par - the velue of 6 aligment parameters for this
//                        wafer
// return         : alteration of chi^2
//*******************************************************************
  float event::CorrectTracks(int bn, int ln, int wn, float* par)
{
   int k = 0;
   Point a, b, pp[3];
   float l[4], c, e, g, f, h, chi = 0;

   if(num == 0)  return 0;
   Bars[bn].SetParam(ln, wn, par);
   for(int i = 0; i < num; i++)
    {
      a = tracks[i].A;
      b = tracks[i].B;
      pp[0] = tracks[i].P[0];
      pp[1] = tracks[i].P[1];
      pp[2] = tracks[i].P[2];
      
     if((pp[bn].nl != ln) || (pp[bn].nw != wn)) continue;
     pp[bn] = Bars[bn].GetPoint(ln, wn, pp[bn].np);

     c = b.Y*b.Y + b.Z*b.Z;
     l[0] = (pp[0].Y*b.Y + pp[0].Z*b.Z)/c;
     l[1] = (pp[1].Y*b.Y + pp[1].Z*b.Z)/c;
     l[2] = (pp[2].Y*b.Y + pp[2].Z*b.Z)/c;

     c = l[0]*l[0] + l[1]*l[1] + l[2]*l[2];
     e = l[0] + l[1] + l[2];
     g = 3*c - e*e;

     f = l[0]*pp[0].X + l[1]*pp[1].X + l[2]*pp[2].X;
     h = pp[0].X + pp[1].X + pp[2].X;
     a.X = (c*h - f*e)/g;
     b.X = (3*f - e*h)/g;

     f = l[0]*pp[0].Y + l[1]*pp[1].Y + l[2]*pp[2].Y;
     h = pp[0].Y + pp[1].Y + pp[2].Y;
     a.Y = (c*h - f*e)/g;
     b.Y = (3*f - e*h)/g;

     f = l[0]*pp[0].Z + l[1]*pp[1].Z + l[2]*pp[2].Z;
     h = pp[0].Z + pp[1].Z + pp[2].Z;
     a.Z = (c*h - f*e)/g;
     b.Z = (3*f - e*h)/g;

     c = (a.Y*b.Y + a.Z*b.Z)/(b.Y*b.Y + b.Z*b.Z);
     a.Y -= c*b.Y;  a.Z -= c*b.Z; a.X -= c*b.X;

     c = b.Y*b.Y + b.Z*b.Z;
     l[0] = (pp[0].Y*b.Y + pp[0].Z*b.Z)/c;
     l[1] = (pp[1].Y*b.Y + pp[1].Z*b.Z)/c;
     l[2] = (pp[2].Y*b.Y + pp[2].Z*b.Z)/c;
     l[3] = (Vertex.Y*b.Y + Vertex.Z*b.Z)/c;

     g = 1000*((a.X + l[0]*b.X - pp[0].X)*(a.X + l[0]*b.X - pp[0].X) +
  	       (a.Y + l[0]*b.Y - pp[0].Y)*(a.Y + l[0]*b.Y - pp[0].Y) +
	       (a.Z + l[0]*b.Z - pp[0].Z)*(a.Z + l[0]*b.Z - pp[0].Z))+
 	 1000*((a.X + l[1]*b.X - pp[1].X)*(a.X + l[1]*b.X - pp[1].X) +
		(a.Y + l[1]*b.Y - pp[1].Y)*(a.Y + l[1]*b.Y - pp[1].Y) +
		(a.Z + l[1]*b.Z - pp[1].Z)*(a.Z + l[1]*b.Z - pp[1].Z))+
	 1000*((a.X + l[2]*b.X - pp[2].X)*(a.X + l[2]*b.X - pp[2].X) +
	       (a.Y + l[2]*b.Y - pp[2].Y)*(a.Y + l[2]*b.Y - pp[2].Y) +
	       (a.Z + l[2]*b.Z - pp[2].Z)*(a.Z + l[2]*b.Z - pp[2].Z));

     a.X = Vertex.X - a.X;
     a.Y = Vertex.Y - a.Y;
     a.Z = Vertex.Z - a.Z;

     g += 1000*(a.X*a.X + a.Y*a.Y + a.Z*a.Z -
	       (a.X*b.X + a.Y*b.Y + a.Z*b.Z)*
	       (a.X*b.X + a.Y*b.Y + a.Z*b.Z)/
	       (b.X*b.X + b.Y*b.Y + b.Z*b.Z));


// B.b!!    if(fabs(a.X) < 1) { chi += g - tracks[i].chi2; k++; }
	  
            if((fabs(a.X) < 1)&&(tracks[i].flag == 1)) 
             { 
              chi += g - tracks[i].chi2; 
              k++; 
             }
      }

  return (k > 0) ? chi/k : 0;
}

//*******************************************************************
//      Class work
//*******************************************************************
//  work  :  constructor of class work
//*******************************************************************
 void work::init(float r1, float r2, float r3, float r4, float r5, float r6)
{
  int i;
 
  Events = (event*)malloc(NEV*sizeof(event));
  for(i = 0; i < NEV; i++)
      Events[i].init(r1, r2, r3, r4, r5, r6);
  
}




//********************************************************************
// ~work  : destructor of class work, frees the memory
//********************************************************************
  work::~work()
{
  for(int i = 0; i < NEV; i++)  Events[i].~event();
  free(Events); 
// delete[] Events;
}

//*******************************************************************
//   MakeAlign :  alignment parameters are defined with the help of 
//                gradient decent method
//*******************************************************************

  void work::MakeAlign()
{
  float Step = 1.5; 
  
  chi0 = TotalChi();
  SigmaVertex();
  if(chi0 == 0) { printf("Nothing to do !\n"); exit(0); }

  int i = 0;

  while((Step > 0.2) && (i < NIT))
   {
	i++;
	printf("\nN %d, st : %.2f  ", i, Step);
	derm = TakeDerivatives(Step);
	if(derm == 0) break;
	ShiftParams(-Step/derm);
	chin = TotalChi();
	if(chin < chi0)
	 {
	  chi0 = chin;
	  SigmaVertex();
	 }
	else
	  {
	    ShiftParams(0.5*Step/derm);
	    chin = TotalChi();
	    if(chin < chi0)
		 {
		  chi0 = chin;
		  SigmaVertex();
		  }
	     else
		 {
		  ShiftParams(0.5*Step/derm);
		  chi0 = TotalChi();
		   Step /= 2;
		 }
            }
      }
}


//*******************************************************************
// InstallParam : installation of alignment parameters
//*******************************************************************
void work::InstallParam()
{
  int i, j, k, l;
  for(i = 0; i < NEV; i++)
   for(l = 0; l < 3; l++)
    for(j = 0; j < Events[i].Bars[l].num_lad; j++)
     for(k = 0; k < Events[i].Bars[l].ladders[j].num_waf; k++)
	Events[i].Bars[l].SetParam(j, k, par[l][j][k]);
//Prover' !!!!!!!
}


//*******************************************************************
// SigmaVertex   : The mean square deviation of vertex founding
//                 error is defined
//*******************************************************************
  void work::SigmaVertex()
{
	float s[6] = {0,0,0,0,0,0};

  	for(int i = 0; i < NEV; i++)
	  {
		  s[0] += (rx[i] - Events[i].Vertex.X);
		  s[1] += (ry[i] - Events[i].Vertex.Y);
		  s[2] += (rz[i] - Events[i].Vertex.Z);
		  s[3] += (rx[i] - Events[i].Vertex.X)*(rx[i] - Events[i].Vertex.X);
		  s[4] += (ry[i] - Events[i].Vertex.Y)*(ry[i] - Events[i].Vertex.Y);
		  s[5] += (rz[i] - Events[i].Vertex.Z)*(rz[i] - Events[i].Vertex.Z);
	  }
	s[3] = sqrt(s[3]/NEV - (s[0]/NEV)*(s[0]/NEV) +
					s[4]/NEV - (s[1]/NEV)*(s[1]/NEV) +
					s[5]/NEV - (s[2]/NEV)*(s[2]/NEV));
   	printf(" Sgm : %.1f", s[3]*1000);
}

//*******************************************************************
// ClearParam  : initial values for alignment parameters are equaled
//               to zero
//*******************************************************************
  void work::ClearParam()
{
  int i, j, k, l;

 for(l = 0; l < 3; l++) 
  for(i = 0; i < Events[0].Bars[l].num_lad; i++)
   for(j = 0; j < Events[0].Bars[l].ladders[i].num_waf; j++)
    for(k = 0; k < 6; k++)
      par[l][i][j][k] = 0;
//Prover'!!!
}

//*******************************************************************
// TotalChi    : calculates the value of total chi^2. For this
//               purpose clears the memory for every events, find all
//               event tracks, optimize them, defines vertex
//               coordinates and calculates chi^2 for all founded
//               tracks. After that it calculates total chi^2 for all
//               events.
// return      : total chi^2 / total number of tracks for all events
//*******************************************************************
  float work::TotalChi()
{
	 float chi = 0;
	 long nt = 0;

	 for(int i = 0; i < NEV; i++)
		{
//B b!!!
//              Events[i].Clear();
//      	Events[i].FindAllTracks();
		
                Events[i].OptimizeTracks();
		Events[i].SelectTracks( chi0 );
		Events[i].FindVertex();

		for(int j = 0; j < Events[i].num; j++)
		    if(Events[i].tracks[j].flag == 1)
                 	{
                          chi += Events[i].tracks[j].chi2;
                          nt++;
                        } 		
//B. b.!!         nt += Events[i].num;
		}
	printf("chi : %.1f (%d)", chi/nt, nt/NEV);
	return (nt > 0) ? chi/nt : 0;
}

//*******************************************************************
// ShiftParams  : recalculates alignment parameters in accordance
//                with  derivatives values
// parameter    : ratio the current step which defined derivatives
//                and the maximal value of founded  derivatives
//*******************************************************************
  void work::ShiftParams(float sh)
{
    int i, j, k;
    float  steps[6] = {0.05, 0.05, 0.05, 0.01, 0.01, 0.01};// steps value of diferent kinds of parameters
    float  w[6] = {1,1,1,0.3,0.5,0.5};// weights,that consider influence of diferent parameter on chi^2 alteration

	for(i = 0; i < 8; i++)
	  for(j = 0; j < 4; j++)
		{
		  for(k = 0; k < 6; k++)
			  par[0][i][j][k] += sh*w[k]*steps[k]*der0[i][j][k];
		  for(k = 0; k < NEV; k++)
			Events[k].Bars[0].SetParam(i, j, par[0][i][j]);
                
		}

	for(i = 0; i < 12; i++)
	  for(j = 0; j < 6; j++)
		{
		  for(k = 0; k < 6; k++)
			 par[1][i][j][k] += sh*w[k]*steps[k]*der1[i][j][k];
		  for(k = 0; k < NEV; k++)
         	   Events[k].Bars[1].SetParam(i, j, par[1][i][j]);
                }

	for(i = 0; i < 16; i++)
	  for(j = 0; j < 7; j++)
		{
		  for(k = 0; k < 6; k++)
			 par[2][i][j][k] += sh*w[k]*steps[k]*der2[i][j][k];
		  for(k = 0; k < NEV; k++)
	               Events[k].Bars[2].SetParam(i, j, par[2][i][j]);
                }
}

//*******************************************************************
// CorrectTracks  : realculates  tracks parameters in accordance with
//                  the found alignment parameters for all the tracks
//                  which cross one wafer. Alteration of total chi^2
//                  are defined.
// parameters     : bn - barrel number
//                  ln - ladder number
//                  wn - wafer number
//                  pp - array of alignment parameters values
// return         : alteration of total chi^2
//*******************************************************************
float work::CorrectTracks(int bn, int ln, int wn, float* pp)
{
	float chi = 0;

	for(int i = 0; i < NEV; i++)
	  chi += Events[i].CorrectTracks(bn, ln, wn, pp);

	return chi;
}

//*******************************************************************
// TakeDerivatives  : defines the derivatives values for all
//                    alignment parameters
// parameter        : step for derivatives definition
// return           : maximal derivative value
//*******************************************************************
float work::TakeDerivatives(float st)
{
	int i, j, k;
	float dm = 0, chir, wpar[6]; 
 float  steps[6] = {0.05, 0.05, 0.05, 0.01, 0.01, 0.01};// steps value of diferent kinds of parameters
   
//  float  s[6] = {0.04, 0.04, 0.04, 0.005, 0.005, 0.005};// steps value of diferent kinds of parameters
//  float  w[6] = {1,1,1,0.3,0.5,0.5};// weights,that consider influence of diferent parameter on chi^2 alteration


	for(i = 0; i < 8; i++)
	  for(j = 0; j < 4; j++)
		 {
			for(k = 0; k < 6; k++)  wpar[k] = par[0][i][j][k];
			for(k = 0; k < 6; k++)
			  {
				 wpar[k] += st*steps[k];
				 chir = CorrectTracks(0, i, j, wpar);

				 wpar[k] -= st*steps[k];
				 der0[i][j][k] = chir/(st*steps[k]);


				 if(dm < fabs(der0[i][j][k])) dm = fabs(der0[i][j][k]);
			  }
			for(k = 0; k < NEV; k++)
			  Events[k].Bars[0].SetParam(i, j, wpar);
		 }


	for(i = 0; i < 12; i++)
	  for(j = 0; j < 6; j++)
		 {
			for(k = 0; k < 6; k++)  wpar[k] = par[1][i][j][k];
			for(k = 0; k < 6; k++)
			  {
				 wpar[k] += st*steps[k];
				 chir = CorrectTracks(1, i, j, wpar);

				 wpar[k] -= st*steps[k];
				 der1[i][j][k] = chir/(st*steps[k]);

				 if(dm < fabs(der1[i][j][k])) dm = fabs(der1[i][j][k]);
			  }
			for(k = 0; k < NEV; k++)
			  Events[k].Bars[1].SetParam(i, j, wpar);
		 }


	for(i = 0; i < 16; i++)
	  for(j = 0; j < 7; j++)
		 {
			for(k = 0; k < 6; k++)  wpar[k] = par[2][i][j][k];
			for(k = 0; k < 6; k++)
			  {
				 wpar[k] += st*steps[k];
				 chir = CorrectTracks(2, i, j, wpar);

				 wpar[k] -= st*steps[k];
				 der2[i][j][k] = chir/(st*steps[k]);


				 if(dm < fabs(der2[i][j][k])) dm = fabs(der2[i][j][k]);
			  }
			for(k = 0; k < NEV; k++)
			  Events[k].Bars[2].SetParam(i, j, wpar);
		 }

	return dm;
}

//********************************************************************
 int work::myrandom( int n )
 {
	return rand()/(RAND_MAX/n);
 }

//********************************************************************
 float work::normal()
  {
	 int i;
	 float a = 0;

	 for(i = 0; i < 12; i++) a += (myrandom(100)*0.01 - 0.5);
	 return a;
  }
//*******************************************************************
// Check
//******************************************************************* 
 float work::Check(float a)
  {
	 if (a > 30) a = 30;	 if (a < -30) a = -30;
	 return a;
  }


//*******************************************************************
//   GenerateEvent
//*******************************************************************

  void work::GenerateEvent(int n_ev)
{

 int level = 20, n_track = 2000, nsh;
 int nl3, nw3, i, num3;
 float  x3, y3, shift = 0.02, angle = 0.001, dx, dy;
 float d, fi, rs;
 Point P1, P2, P3, PP1, PP2;

 nsh = (int)n_track*level/100;
 
 Events[n_ev].Bars[0].Clear();
 Events[n_ev].Bars[1].Clear();
 Events[n_ev].Bars[2].Clear();

 rx[n_ev] = Events[n_ev].Vertex.X = 0;//myrandom(60) - 30;
 ry[n_ev] = Events[n_ev].Vertex.Y = 0;//(myrandom(200) - 100)*0.005;
 rz[n_ev] = Events[n_ev].Vertex.Z = 0;//(myrandom(200) - 100)*0.005;
	 
for(i = 0; i < n_track; i++)
 {
   nl3 = myrandom(16); 	  nw3 = myrandom(7);
   x3 = myrandom(60) - 30;   y3 = myrandom(60) - 30;

   num3 = Events[n_ev].Bars[2].PutPoint(nl3, nw3, x3, y3);
   P3 = Events[n_ev].Bars[2].GetPoint(nl3, nw3, num3);
  
   PP2.nl = Events[n_ev].Bars[1].WhichLadder(Events[n_ev].Vertex, P3);
   PP2 = Events[n_ev].Bars[1].WhichWaffer(Events[n_ev].Vertex, P3, PP2.nl);
   PP2.np = Events[n_ev].Bars[1].PutPoint(PP2.nl, PP2.nw, PP2.X, PP2.Y);

   PP1.nl = Events[n_ev].Bars[0].WhichLadder(Events[n_ev].Vertex, P3);
   PP1 = Events[n_ev].Bars[0].WhichWaffer(Events[n_ev].Vertex, P3, PP1.nl);
   PP1.np = Events[n_ev].Bars[0].PutPoint(PP1.nl, PP1.nw, PP1.X, PP1.Y);

   P2 = Events[n_ev].Bars[1].GetPoint(PP2.nl, PP2.nw, PP2.np);
   P1 = Events[n_ev].Bars[0].GetPoint(PP1.nl, PP1.nw, PP1.np);

/*   PP1.X += normal()*shift; 	PP1.Y += normal()*shift;
   Events[n_ev].Bars[0].SetX(PP1.nl, PP1.nw, PP1.np, Check(PP1.X));
   Events[n_ev].Bars[0].SetY(PP1.nl, PP1.nw, PP1.np, Check(PP1.Y));

   d = angle*normal();  fi = 6.28*myrandom(100)/100;
   rs = sqrt((P1.X - P2.X)*(P1.X - P2.X) +		
        (P1.Y - P2.Y)*(P1.Y - P2.Y) + (P1.Z - P2.Z)*(P1.Z - P2.Z));

   dy = rs*tan(d)*sin(fi);  dx = rs*tan(d)*cos(fi);
   PP2.X += normal()*shift + dx; PP2.Y += normal()*shift + dy;
   Events[n_ev].Bars[1].SetX(PP2.nl, PP2.nw, PP2.np, Check(PP2.X));
   Events[n_ev].Bars[1].SetY(PP2.nl, PP2.nw, PP2.np, Check(PP2.Y));

   d = angle*normal();  fi = 6.28*myrandom(100)/100;
   rs = sqrt((P3.X - P2.X)*(P3.X - P2.X) +	(P3.Y - P2.Y)*(P3.Y - P2.Y) +
        (P3.Z - P2.Z)*(P3.Z - P2.Z));
   x3 += normal()*shift + dx + rs*tan(d)*cos(fi);
   y3 += normal()*shift + dy + rs*tan(d)*sin(fi);
   Events[n_ev].Bars[2].SetX(nl3, nw3, num3, Check(x3));
   Events[n_ev].Bars[2].SetY(nl3, nw3, num3, Check(y3));
  */
  }
 i = 0;
 while(i < nsh)
  {
   Events[n_ev].Bars[0].PutPoint(myrandom(8),myrandom(4),
                   myrandom(60) - 30,myrandom(60) - 30);
   i++;
  }

 i = 0;
 while (i < nsh)
  {
   Events[n_ev].Bars[1].PutPoint(myrandom(12),myrandom(6),
                   myrandom(60) - 30,myrandom(60) - 30);
   i++;
  }

 i = 0;
 while(i < nsh)
  { 
   Events[n_ev].Bars[2].PutPoint(myrandom(16),myrandom(7),
                   myrandom(60) - 30,myrandom(60) - 30);
   i++;
  }

}

//*******************************************************************
//   Changing of geometry
//*******************************************************************
  void work::ChangeGeom(int i)
{
  int  j, k, l, m; 
  Point points[150];
  Point P, P3, P4;

  for(m = 0; m < 3; m++)
   for(j = 0; j < Events[i].Bars[m].num_lad; j++)
    for(k = 0; k < Events[i].Bars[m].ladders[j].num_waf; k++)
     {
       for(l = 0; l < Events[i].Bars[m].ladders[j].waffers[k].num; l++)
         points[l] = Events[i].Bars[m].GetPoint(j, k, l);                        
       Events[i].Bars[m].SetParam(j, k, par[m][j][k]);
       for(l = 0; l < Events[i].Bars[m].ladders[j].waffers[k].num; l++) 
        { 
            P3.X = points[l].X;
	    P3.Y = points[l].Y*Events[i].Bars[m].SinTheta[2*j] - 
                   points[l].Z*Events[i].Bars[m].CosTheta[2*j];
	    P3.Z = points[l].Y*Events[i].Bars[m].CosTheta[2*j] + 
                   points[l].Z*Events[i].Bars[m].SinTheta[2*j];
	    
            P.X = Events[i].Vertex.X;
	    P4.Y = Events[i].Vertex.Y*Events[i].Bars[m].SinTheta[2*j] - 
                   Events[i].Vertex.Z*Events[i].Bars[m].CosTheta[2*j];
	    P4.Z = Events[i].Vertex.Y*Events[i].Bars[m].CosTheta[2*j] + 
                   Events[i].Vertex.Z*Events[i].Bars[m].SinTheta[2*j];
             

            P = Events[i].Bars[m].ladders[j].waffers[k].
                        WhichPoint(P4,P3);              
            Events[i].Bars[m].SetX(j, k, l, P.X);  
            Events[i].Bars[m].SetY(j, k, l, P.Y); 
         }
     }
}

