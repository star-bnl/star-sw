// ------- System includes -------------- 
#include <math.h>
#include <assert.h>
// ------- Local includes -------  
#include "Histogram.hh"


//>--------------------------------------------------------------------
// ROUTINE:     sca_Histogram.cc
//
// DESCRIPTION: This is the Histogram class used by the Dataset class
//              to do bin counting at various scales.
//
// AUTHOR:      Dhammika Weerasundara -- University of Washington
//              dhammika@gibbs.npl.washington.edu
//
// HISTORY:    
//      06/19/1995       SJ Bailey  Orginal was written to run
//                       JG Reid    in NA49/DSPACK environment.
//
//      06/26/1998       DSW        Adapted to run in STAF/ROOT.
//
//      08/10/1998       LDC        Worked on debugging.
//
//      08/13/1998       DSW        Put debug print statements on a 
//                                  DEBUG switch. 
//      
//      09/01/1998       DSW        Added function overloading for
//                                  AddDatum to loop over xdata points.
//
//>--------------------------------------------------------------------

////////////////////////////////////////////////////////////
//
// Histogram.cc
//
// This is the Histogram class used by the Dataset class
// to do bin counting at various scales.
//
// The conceptually straightforward manner of implementing
// this class would be to make an array with each element
// representing a bin.  But at very small scales you spend
// most of your time walking through empty bins and this
// becomes very inefficient.
//
// For efficiency reasons, the histogram information is
// stored only for occupied bins.  The histogram is kept as
// an array, one element per point, representing the bin to
// which this point belongs.  This bin number list is then
// sorted, and bin counting becomes simply a matter of walking
// through that array counting contiguous bin #s.
//
// e.g. if the 1D data comes in unsorted (which is a ridiculously
//      inefficient way to do things, but is good for the example)
//      the histogram array might appear as:
//
//	2 1 5 4 3 2 2 4 1 7
//
//	Which after sorting would be:
//
//	1 1 2 2 2 3 4 4 5 7
//
//	indicating bin 1 has two points, 2 has 3, etc.
//
// This method grows as N Log N (from the Heapsort) with the
// number of hits but does not take significantly more time
// for 2D than it does for 1D.
//


////////////////////////////////////////////////////////////
// Histogram class constructor
// sets defaults for bin sizes, etc.

Histogram::Histogram(void)
{
  binlist = NULL;		// The Histogram array
  NMax = HistMAXPOINTS;
  N = 0;			// Number of points in histogram
  sorted = FALSE;

  nbinsx = nbinsy = 1;		// Number of the bins in x and y
  binsize_x = binsize_y = BINSIZE;	// Size of bins in x and y
  xmin = ymin = 0.0;		// Histogram ranges
  xmax = ymax = BINSIZE;

  AllocHisto();

}  // end Histogram constructor



////////////////////////////////////////////////////////////
// public: SetBinSize()
// sets the number of bins in the x and y directions

void Histogram::SetBinSize(double size)
{
  SetBinSize(size, size);
}

void Histogram::SetBinSize(double xsize, double ysize)
{
  binsize_x = xsize;
  binsize_y = ysize;
  SetBinNum();
}


////////////////////////////////////////////////////////////
// public: SetRange()
// Sets the range of the histogram that will be used for
// the data set.

void Histogram::SetRange(double xminl, double xmaxl, double yminl, double ymaxl)
{
  Histogram::xmin = xminl;
  Histogram::xmax = xmaxl;
  Histogram::ymin = yminl;
  Histogram::ymax = ymaxl;
}  // end SetRange()


////////////////////////////////////////////////////////////
// public: AddDatum()
// This function adds one data point to the histogram.  It
// assumes that the histogram range and number of bins has
// already been set up.

void Histogram::AddDatum(sca_data_t &datum)
{
  AddDatum(datum.x, datum.y);
}  // end AddDatum()

void Histogram::AddDatum(double x, double y)
{
  // locate which bin
  int binx = (int) ((x - xmin) / binsize_x);
  int biny = (int) ((y - ymin) / binsize_y);

  if(0 <= binx && binx < nbinsx && 0 <= biny && biny < nbinsy)
    {
      binlist[N] = biny * nbinsx + binx + 1;
      N++;
    }
}  // end AddDatum()

void Histogram::AddDatum(sca_data_t **xdata, int numvalid, 
			 double xoff, double yoff)
{

  for (int iloop=0; iloop<numvalid ; iloop++){
    double x = xdata[iloop]->x + xoff;
    double y = xdata[iloop]->y + yoff;
    // locate which bin
    int binx = (int) ((x - xmin) / binsize_x);
    int biny = (int) ((y - ymin) / binsize_y);
    
    if(0 <= binx && binx < nbinsx && 0 <= biny && biny < nbinsy)
      {
	binlist[N] = biny * nbinsx + binx + 1;
	N++;
      }
  }
}  // end AddDatum()

////////////////////////////////////////////////////////////
// public: GetVolume()
// Returns the rank non-dithered volume for this histogram.1
// If rank == 1, the entropy S1 = -Sum[p_i * Log[p_i]] is returned.
// The somewhat cryptic forms of this loop is for speed.
// This routine takes the majority of the entropy calculation time.

double Histogram::GetVolume (double rank,
			     int bin_num,
			     int num_dim, 
			     sca_prior_t *prior_p)
{
  double prior;
  double vol = 0.0;		// volume
  double n_i;

  int currentbin = 0;		// current bin we are counting
  int hitcount = 0;		// hitcount for that bin
  int cur_marker = 1;

  //JP
  if (!prior_p) {
    cerr << "Unable to load sca_prior.\n";
    exit (1);
  }
 
  if(!sorted && num_dim != 1)
    SortBinList();

  prior = 1.0;
  currentbin = binlist[0];
  double min_occ = (0.1/(double) N);
  for(int i = 0; i < N; i++)
    {
      if(binlist[i] == currentbin)
	hitcount++;
      else			// add volume from last bin
	{
	  while (currentbin > cur_marker) {
	      prior_p[prior_p[0].marker++].marker = cur_marker;
	      cur_marker++;
	   }

	  if (prior_p[0].w != 0) 
	    prior_p[prior_p[0].marker].w += ((double) hitcount/(double) N);
	  else 
	    prior = prior_p[prior_p[0].marker].w;

	  prior_p[prior_p[0].marker++].marker = currentbin;
	  cur_marker++;
	  
	  if ( prior > min_occ ) {
	    n_i = (double)hitcount/(double) (N*prior);
	    if( rank == 1.0 )
	      vol -= prior * n_i * log10(n_i);   // minus b/c log < 0
	    else
	      vol += prior * pow(n_i,rank);
	  }

	  hitcount = 1;			  // reset the hitcount for new bin
	  currentbin = binlist[i];	  // reset which bin this is
	}
    }  // end for i loop through occupied bins

  // Don't forget to count the very last bin!

  while (currentbin > cur_marker) {
    prior_p[prior_p[0].marker++].marker = cur_marker;
    cur_marker++;
  }

  if (prior_p[0].w != 0) 
    prior_p[prior_p[0].marker].w += ((double) hitcount/(double) N);
  else 
    prior = prior_p[prior_p[0].marker].w;

  prior_p[prior_p[0].marker++].marker = currentbin;
  cur_marker++;

  if ( prior > min_occ ) {
    n_i = (double)hitcount/(double) (N*prior);
    if( rank == 1.0 )
      vol -= prior * n_i * log10(n_i);  // minus b/c log < 0
    else
      vol += prior * pow(n_i,rank);
  }

  while (bin_num > cur_marker) {
    prior_p[prior_p[0].marker++].marker = cur_marker;
    cur_marker++;
  }

  return vol;

}  // end GetVolume()


////////////////////////////////////////////////////////////
// public: GetBinSizeX() and Y()
// returns the binsizes

double Histogram::GetBinSizeX(void)  {  return binsize_x; }
double Histogram::GetBinSizeY(void)  {  return binsize_y; }


////////////////////////////////////////////////////////////
// public: ClearHisto()
// Clears an already allocated histogram to 0.0
// This is done with a memset for speed instead of a loop 

void Histogram::ClearHisto(void)
{
  sorted = FALSE;
  if (N)
    memset(binlist, 0, N * sizeof(int));
  N = 0;
}  // end ClearHisto()


////////////////////////////////////////////////////////////
// public: SetMaxPoints()
// Set the maximum number of points that will be filled into
// the histogram.  This call is not required, but it will make
// the memory usage more efficient.

void Histogram::SetMaxPoints(int n)
{
  NMax = n;
  AllocHisto();
}  // end SetMaxPoints()


////////////////////////////////////////////////////////////
// private: SortBinList()
// This is a Heapsort based upon Numerical Recipes.  A
// Heapsort is used rather than a Quicksort because our
// data is often already nearly sorted.

void Histogram::SortBinList(void)
{
  unsigned int i,ir,j,k;
  int bin;
  int *list = binlist-1;	// so as to not disturb the NR code too much
				// which expects list[1..N] not binlist[0..N-1]

  if (N < 2) return;
  k = (N >> 1)+1;
  ir = N;
  for (;;) {
    if (k > 1) {
      bin = list[--k];
    } else {
      bin = list[ir];
      list[ir] = list[1];
      if (--ir == 1) {
	list[1] = bin;
	break;
      }
    }
    i=k;
    j=k+k;
    while (j <= ir) {
      if (j < ir && list[j] < list[j+1]) j++;
      if (bin < list[j]) {
	list[i] = list[j];
	i=j;
	j <<= 1;
      } else j = ir + 1;
    }
    list[i] = bin;
  }

  sorted = TRUE;

}  // end SortBinList()


////////////////////////////////////////////////////////////
// private: SetBinNum()
// This is to be called whenever the range or the size of
// the bins changes.

void Histogram::SetBinNum(void)
{
  if( binsize_x > 0 )
    nbinsx = (int) ((xmax-xmin)/binsize_x + 3);
		// the +3 is because: +1 for rounding up, and
		// +2 for dithering by a bin starting away from the edge.
  if( binsize_y > 0 )
    nbinsy = (int) ((ymax-ymin)/binsize_y + 3);
}  // end SetBinNum()


////////////////////////////////////////////////////////////
// private: FreeHisto()
// Frees the memory currently occupied by the histogram

void Histogram::FreeHisto(void)
{
  if(binlist)
    delete [] binlist;

  binlist = NULL;
  N = 0;
}  // end FreeHisto()


////////////////////////////////////////////////////////////
// private: AllocHisto()
// Allocates memory for the histogram if necessary.
// This will downsize the allocation only if it is significant.

void Histogram::AllocHisto(void)
{
  if(binlist)
    delete [] binlist;

  binlist = new int[NMax];	// histo kept as a 1D array for speed
  assert(binlist != NULL);
  ClearHisto();
}  // end AllocHisto()













