#include <math.h>
#include <string.h>
#include "Dataset.hh"
#include "utility.hh"
Dataset::Dataset(
  TABLE_HEAD_ST   *sca_const_h,     SCA_CONST_ST     *sca_const,
  TABLE_HEAD_ST   *sca_prior_h,     SCA_PRIOR_ST     *sca_prior,  
  TABLE_HEAD_ST   *sca_in_h,        SCA_IN_ST        *sca_in,
  char            *input_data_name,	   
  char            *output_sca_name) 
{
  data      = NULL;
  xdata     = NULL;
  sca       = NULL;
  info      = NULL;
  prior_p   = NULL;
  constants = NULL;
  constants = new sca_const_t;
  prior_p   = new sca_prior_t[PRIOR_SIZE];
  data      = new sca_data_t[sca_in_h->nok];
  getConstants(sca_const_h, sca_const);
  getPrior(sca_prior_h, sca_prior);
  getinData(sca_in_h,   sca_in);
  strcpy(sca_name, input_data_name); //JP changed from output_sca_name
  strcpy(sca_info_name, output_sca_name);
  strcat(sca_info_name, "_info");
  if( constants->auto_size )
    findrange();		    // find range of data set
  else
    {
      xmin = constants->xmin;      // use user supplied ranges
      xmax = constants->xmax;
      ymin = constants->ymin;
      ymax = constants->ymax;
    }
  datanum = sca_in_h->nok;
  if(data && datanum) {
    xdata = new sca_data_t* [datanum];
    ydata = new sca_data_t* [datanum];
    validnum = 0;
    for(int i = 0; i < datanum; i++)
      {
	if( xmin <= data[i].x && data[i].x <= xmax &&
	    ymin <= data[i].y && data[i].y <= ymax )
	  {
	    xdata[validnum] = &data[i];
	    ydata[validnum] = &data[i];
	    validnum++;
	  }
      }
    qsort(xdata, validnum, sizeof(sca_data_t *), xcomp);
  }
  num_ranks = constants->num_ranks;  // num_scales will get calculated later
  for(int i = 0; i < MAX_RANKS; i++)
    vol_x[i] = vol_y[i] = vol_xy[i] = 0.0;
  cout << debug("Trace") << "dataSet successfully initialized.\n";
}  // end Dataset constructor
Dataset::~Dataset()
{
  if(constants) delete constants;
  if (prior_p) delete [] prior_p;
  if(data) delete [] data;
  if(xdata) delete [] xdata;
  if(ydata) delete [] ydata;
  if (sca) delete [] sca;
  if (info) delete [] info;
}
int Dataset::CalcEntropy()
{
  static int callcount=0;
  double rank;			// for shorter notation
  sca_info_t *cur_info;
  printf("Dataset::CalcEntropy, call count = %d\n",++callcount);
  cout << debug("Trace") << "CalcEntropy() called.\n";
  if( !data || !validnum )
    {
      cerr << error(HIGH) <<
	"There is no valid data in the server to analyze.\n";
      return 1;
    }
  alloc_info();
  double maxbinsize_x = constants->maxbinsize_x;
  double maxbinsize_y = constants->maxbinsize_y;
  double binsize_x = constants->minbinsize_x;
  double binsize_y = constants->minbinsize_y;
  if (!prior_p) { 
      cout << "Fail CalcEntropy: prior cantains no data!" << endl;
      return 1;
  }
  int is = 0;			// index to the scale value
  while(binsize_x <= maxbinsize_x && binsize_y <= maxbinsize_y)
    { 
      CalcVolume(is, binsize_x, binsize_y);
      for(int ir = 0; ir < num_ranks; ir++)
	{
	  rank = sca[ir].rank;			// for shorter notation
	  cur_info = &(sca[ir].info[is]);	// also for shorter notation
	  cur_info->rank = rank;
	  cur_info->scale_x = log10(binsize_x/(xmax-xmin));
	  cur_info->scale_y = log10(binsize_y/(ymax-ymin));
	  cur_info->evn = data[ir].evn;
	  cur_info->run = data[ir].run;
	  if (prior_p[0].w != 0) {
	    if( rank == 1.0 )
	      cur_info->entropy = cur_info->volume;
	    else
	      cur_info->entropy = 1.0/(1.0 - (float)rank) *
		                  log10( cur_info->volume );
	  }
	  else {
	    if( rank == 1.0 )
	      cur_info->info = cur_info->volume;
	    else
	      cur_info->info = (float)1/(float)(1-rank) *
		               log10( cur_info->volume );
	  } 
	}// end for r loop through ranks at the given scale
      binsize_x *= constants->step_binsize_x;
      binsize_y *= constants->step_binsize_y;
      is++;
    }  // end while loop over scale
  return 0;
}  // end CalcEntropy()
int Dataset::CalcDimension(void)
{
  int L, R;
  double rise;
  sca_info_t *cur_info;
  cout << debug("Trace") << "CalcDimension() called.\n";
  if (!sca) {
      cerr << "Fail CalcDiminsion: sca contains no data!" << endl;
      return 1;
  }
  for(int r = 0; r < num_ranks; r++)
    {
      cur_info = sca[r].info;
      double run = cur_info[1].scale_x - cur_info[0].scale_x;
      for(int i = 0; i < num_scales; i++)
	{
	  L = i - 2; R = i + 2;
	  if(L < 0) L = 0;
	  if(R >= num_scales) R = num_scales-1;
	  rise = cur_info[L].info - cur_info[R].info;
	  cur_info[i].dim_lower = rise/(run * (R-L));
	}  // end for i loop through scales
    }  // end for r loop through ranks
  return 0;
}  // end CalcDimension()
void Dataset::findrange(void)
{
  xmin = xmax = data[0].x;
  ymin = ymax = data[0].y;
  for(int i = 1; i < datanum; i++)
    {
      double x = data[i].x;
      double y = data[i].y;
      if(x < xmin)
	xmin = x;
      else if(x > xmax)
	xmax = x;
      if(y < ymin)
	ymin = y;
      else if(y > ymax)
	ymax = y;
    }  // end for loop through all the data
}  // end findrange()
int Dataset::alloc_info()
{
  cout << debug("Trace") << "alloc_info() called.\n";
  num_scales = 0;
  double binsize_x    = constants->minbinsize_x;   // start at smallest value
  double binsize_y    = constants->minbinsize_y;   // start at smallest value
  double maxbinsize_x = constants->maxbinsize_x;
  double maxbinsize_y = constants->maxbinsize_y;
  double binstep_x    = constants->step_binsize_x;
  double binstep_y    = constants->step_binsize_y;
  while(binsize_x <= maxbinsize_x && binsize_y <= maxbinsize_y)
    {
      num_scales++;
      binsize_x *= binstep_x;
      binsize_y *= binstep_y;
    }
  num_ranks = constants->num_ranks;
  if (sca) delete [] sca;
  sca = new sca_t[num_ranks];
  memset (sca, 0, sizeof(sca_t) * num_ranks);
  if(!sca) {
    cerr << error(HIGH) << "Failed to allocate " << sca_name << ".\n";
    return 1;
  }
  int infonum = num_scales * num_ranks;
  if (info) delete [] info;
  info = new sca_info_t[infonum];
  memset (info, 0, sizeof(sca_info_t) * infonum);
  if(!info) {
    cerr << error(HIGH) << "Failed to allocate " << sca_info_name << ".\n";
    return 1;
  }
  for(int i = 0; i < num_ranks; i++) {
    sca[i].info = info + i * num_scales;
    sca[i].num_scales = num_scales;
  }
 return 0;
}  // end alloc_info()
int Dataset::getConstants(
   TABLE_HEAD_ST *sca_const_h, SCA_CONST_ST *sca_const)
{
  constants->num_dim          = sca_const->num_dim       ;
  constants->minbinsize_x     = sca_const->minbinsize_x  ;
  constants->maxbinsize_x     = sca_const->maxbinsize_x  ;
  constants->minbinsize_y     = sca_const->minbinsize_y  ;
  constants->maxbinsize_y     = sca_const->maxbinsize_y  ;
  constants->step_binsize_x   = sca_const->step_binsize_x;
  constants->step_binsize_y   = sca_const->step_binsize_y;
  constants->auto_size        = sca_const->auto_size     ;
  constants->xmin             = sca_const->xmin          ;
  constants->xmax             = sca_const->xmax          ;
  constants->ymin             = sca_const->ymin          ;
  constants->ymax             = sca_const->ymax          ;
  constants->zmin             = sca_const->zmin          ;
  constants->zmax             = sca_const->zmax          ;
  constants->num_ranks        = sca_const->num_ranks     ;
  for (int i=0; i<10; i++){
    constants->rank[i]        = sca_const->rank[i]       ;
  }
  constants->dithmin          = sca_const->dithmin       ;
  constants->dithscale        = sca_const->dithscale     ;
  constants->perfectdither    = sca_const->perfectdither ;
  return 0;
} // end getConstants
int Dataset::getPrior(
   TABLE_HEAD_ST *sca_prior_h, SCA_PRIOR_ST *sca_prior) 
{
  for (int i=0; i<PRIOR_SIZE; i++) {
    prior_p[i].w      = sca_prior[i].w;
    prior_p[i].marker = sca_prior[i].marker;
  }
  return 0;
} // end getPrior
int Dataset::fillPrior(
   TABLE_HEAD_ST *sca_prior_h, SCA_PRIOR_ST *sca_prior) 
{
  for (int i=0; i<PRIOR_SIZE; i++) {
    sca_prior[i].w      = prior_p[i].w;
    sca_prior[i].marker = prior_p[i].marker;
  }
  return 0;
} // end fillPrior
int Dataset::getinData(
   TABLE_HEAD_ST *sca_in_h, SCA_IN_ST  *sca_in)
{
  for (int ii=0; ii<sca_in_h->nok; ii++) {
    data[ii].iflag = ii;
    data[ii].iflag = sca_in[ii].iflag ;
    data[ii].x     = sca_in[ii].x;
    data[ii].y     = sca_in[ii].y;
    data[ii].z     = sca_in[ii].z;
    data[ii].evn   = sca_in[ii].evn;
    data[ii].run   = sca_in[ii].run;
  }
  return 0;
} // end getinData
int Dataset::fillScaOutTable(TABLE_HEAD_ST  *sca_out_h, SCA_OUT_ST *sca_out)
{
  int irow=0, irank = 0,iscale = 0 ;
  sca_info_t *cur_info;
  sca_out_h->nok = 0;
  for (irank=0; irank < num_ranks; irank++) {
    for(iscale=0; iscale < num_scales; iscale++) {
      cur_info = &(sca[irank].info[iscale]);
      sca_out[irow].rank      = cur_info->rank         ;
      sca_out[irow].scale_x   = cur_info->scale_x      ;
      sca_out[irow].scale_y   = cur_info->scale_y      ;
      sca_out[irow].volume    = cur_info->volume       ; 
      sca_out[irow].entropy   = cur_info->entropy      ;
      sca_out[irow].dim       = cur_info->dim          ; 
      sca_out[irow].info      = cur_info->info         ;  
      sca_out[irow].dim_lower = cur_info->dim_lower    ;
      sca_out[irow].evn       = cur_info->evn          ;   
      sca_out[irow].run       = cur_info->run          ;   
      irow++;
    }
  }
  sca_out_h->nok = irow;
  return 0;
} // end fillScaOutTable
void Dataset::CalcVolume(int is, double binsize_x, double binsize_y)
{
 double *tempvol;
     tempvol = DithVolume(binsize_x, binsize_y);
     for(int ir = 0; ir < num_ranks; ir++)
       {
         sca[ir].info[is].volume = tempvol[ir];
       }
/*    }  // end calculation of dithered volume
  else		// for scale > 0 we can do perfect fast dithering
    {
      tempvol = DithVolume(binsize_x, binsize_y);
          for(int ir = 0; ir < num_ranks; ir++)
            {
              sca[ir].info[is].volume = tempvol[ir];
            }
    }  // end fast volume calculation
*/
}  // end CalcVolume()
double *Dataset::DithVolume(double binsize_x, double binsize_y)
{
  static double *vol = NULL;
  double xoff, yoff;
  int r;
  if( !vol )
    vol = new double[num_ranks];
  for(r = 0; r < num_ranks; r++)
    vol[r] = 0.0;
  Hist.SetRange(xmin, xmax, ymin, ymax);
  Hist.SetBinSize(binsize_x, binsize_y);
  int dither_x = (int)(constants->dithmin +
		       constants->dithscale * sqrt(binsize_x/(xmax-xmin)));
  int dither_y = (int)(constants->dithmin +
		       constants->dithscale * sqrt(binsize_y/(ymax-ymin)));
  int nbinsx = (int) ((xmax-xmin)/binsize_x + 3);
  double edge_x = binsize_x/dither_x/2;
  double edge_y = binsize_y/dither_y/2;
  if( constants->num_dim == 1 )
    {
      dither_y = 1;
      edge_y = 0.0;
    }
  for(int dx = 0; dx < dither_x; dx++)
    {
      xoff = (double) dx / (double) dither_x * binsize_x + edge_x;
      for(int dy = 0; dy < dither_y; dy++)
	{
	  yoff = (double) dy / (double) dither_y * binsize_y + edge_y;
	  Hist.ClearHisto();
	  for(int i = 0; i < validnum; i++)
	    Hist.AddDatum(xdata[i]->x + xoff, xdata[i]->y + yoff);
	  for(r = 0; r < num_ranks; r++)
	    vol[r] += Hist.GetVolume(constants->rank[r],
			             nbinsx,
				     constants->num_dim,
				     prior_p);
	}  // end y dithering loop
    }  // end x dithering loop
  for(r = 0; r < num_ranks; r++)
    vol[r] /= (dither_x * dither_y);
  return vol;
}  // end DithVolume()
