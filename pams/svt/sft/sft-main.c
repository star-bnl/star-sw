/*  find the vertex position that leads to
 *  the maximum number of straight tracks
 *  all distance units in cm            
 */

#include <sys/types.h>
#include <time.h>

#include <math.h>   
#include <stdio.h>  
#include <stdlib.h> 
#include <string.h> 
#include "sft.h"


SPACEPOINT 	 spt[10000];
int              PixelOcc[PIX_NMAX];
int              iNumOccupiedPixels;
int             *OccupiedPixels[SPT_PTR_NMAX];
VF_NTRACK 	 vf_ntrack[40];

int 		 vf_ntrack_n;  /* number of elements actually in the list */
long  		 spt_n     = 0;


static  int	 very_first=1;
static  double   x_scale, 
		 z_scale;
static  long   	 x_size, 
		 z_size, 
		 xz_size,
		 iptr;
static  double   x_min,
		 x_max,
		 z_min,
		 z_max;


int
sft_main (TABLE_HEAD_ST *sft_par_h,   SFT_PAR_ST    *sftpar,
          TABLE_HEAD_ST *spt_h,       SCS_SPT_ST    *staf_spt,
          TABLE_HEAD_ST *sft_vertex,  SFT_VERTEX_ST *vertex)
{
   sft_init ();

   sft_process_event (sft_par_h, sftpar, spt_h, staf_spt, sft_vertex, vertex);

   return 0;
}



/*  svt vertex finding
 *  analyze all available events in file
 *  calculate average difference and rms
 */

int 
sft_process_event (TABLE_HEAD_ST *sft_par_h,   SFT_PAR_ST    *sftpar,
                   TABLE_HEAD_ST *spt_h,       SCS_SPT_ST    *staf_spt,
                   TABLE_HEAD_ST *sft_vertex,  SFT_VERTEX_ST *vertex)
{
   double   z,
            diff, 
            z_low, 
            z_high,
            z_actual;
   int      layer;
   float    wafer;
   long     ispt,
            jspt;
 
   spt_n = spt_h->nok;
   if (spt_n <= 0)
      return -1;
 
   /*  Start by grabbing the contents of the scs_spt
    *  array and copying them into our local array. 
    */

   ispt = 0;
   for (jspt = 0; jspt < spt_n; jspt++, ispt++)
   {
       if (staf_spt[jspt].id_wafer < 3000)
          layer = 1;
       else if (staf_spt[jspt].id_wafer < 5000)
          layer = 2;
       else
	  layer = 3;
 
       spt[ispt].layer = layer;
       spt[ispt].track = staf_spt[jspt].id_track;
       spt[ispt].wafer = staf_spt[jspt].id_wafer;
       spt[ispt].de    = staf_spt[jspt].de[0];
       spt[ispt].x[0]  = staf_spt[jspt].x[0];
       spt[ispt].x[1]  = staf_spt[jspt].x[1];
       spt[ispt].x[2]  = staf_spt[jspt].x[2];
   }

   spt_n = ispt + 1;

   z_actual = 0;
   z_low = -0.5;
   z_high = 0.5;
   z = sft_find_vertex (0.0, 1.0);
   diff = z_actual - z;

   printf ("Actual: %12.6f    Computed: %12.6f    Difference: %12.6f\n",
            z_actual, z, diff);

}


void 
sft_analyse (double z)
{
   int       ispt1, 
             ispt2, 
             ispt3, 
             icall,
             n;
   long      track;
   double    xc1,
             zc1, 
             xc2, 
             zc2, 
             xc3, 
             zc3, 
             rc1, 
             rc2,
             rc3,
             cosa,
             a,
             ratio;
   char      file_name[40];
   FILE     *fout;

   fout = fopen ("feature.dat", "w");

   icall = 1;

   /* What's going on here? */

   track = sft_vf_ntrack (z);

   /* What the hell is this loop for? */

   for (ispt1 = 0; ispt1 < spt_n; ispt1++)
   {
      if (spt[ispt1].layer==1)
      {
         track = spt[ispt1].track;
         n = 1;
         for (ispt2 = 0; ispt2 < spt_n; ispt2++)
            if (spt[ispt2].track == track && ispt2 != ispt1)  
               n++;

         for (ispt2 = 0; ispt2 < spt_n; ispt2++)
         {
            if (spt[ispt2].layer==2 && spt[ispt2].track==track)
            {
               for (ispt3 = 0; ispt3 < spt_n; ispt3++)
               {
                  if (spt[ispt3].layer == 3 && spt[ispt3].track == track)
                  {
                     xc1 = spt[ispt2].xc - spt[ispt1].xc;
                     zc1 = spt[ispt2].zc - spt[ispt1].zc;
                     xc2 = spt[ispt3].xc - spt[ispt2].xc;
                     zc2 = spt[ispt3].zc - spt[ispt2].zc;
                     xc3 = spt[ispt3].xc - spt[ispt1].xc;
                     zc3 = spt[ispt3].zc - spt[ispt1].zc;
                     rc1 = sqrt (xc1 * xc1 + zc1 * zc1);
                     rc2 = sqrt (xc2 * xc2 + zc2 * zc2);
                     rc3 = sqrt (xc3 * xc3 + zc3 * zc3);
                     cosa = (xc1 * xc2 + zc1 * zc2) / (rc1 * rc2);
                     a = 180 * acos (cosa) / 3.1415;
                     ratio = rc2 / rc1;

                     fprintf (fout, "%5d %12.6f %12.6f %12.6f %12.6f ",
                              track, rc1, rc2, rc3, cosa);

                     fprintf (fout, "%12.6f %12.6f %d\n",
                              a, ratio, n);
                  }
               }
            }
         }
      }
   }
}



int icall = 1;

/* calculate the number of straight lines  */
/* compatible with the vertex position z0   */

float
sft_vf_ntrack (double z0)
{
   double     xc,
              zc, 
              zc0, 
              x,
              y,
              z,
              r,
              rx,
              rz;
   long       ix,
              iz,
              ii,
              off,
              ntrack;
   int        ispt,
              jspt;
   char       fname[40];
   FILE      *fout;
   SPT_REF   *ptr,
            **occ_ptr;

   if (icall == 1)
   {
      icall = 0;
 
      /* build or update the list in feature space */
      /* loop on all space points                  */
 
#ifdef VF_IO
      if (fout) 
         fclose (fout);
      strcpy (fname,"test.%d",icall);
      fout = fopen (fname, "w");
#endif

      for (ispt = 0; ispt < spt_n; ispt++)
      {
         x   = spt[ispt].x[0];
         y   = spt[ispt].x[1];
         z   = spt[ispt].x[2];
         r   = sqrt (x * x + y * y);          /*xc = fatan2(y,x)   */

         rx  = x_scale / r;
         rz  = z_scale / r;
         xc  = x * rx;
         zc0 = z * rz;                        /* the constant part */
         zc  = zc0 - z0 * rz;                 /* the variable part */

#ifdef DPRINT
         printf("\n %d %f  %f %f %f  %f",ispt,x,y,z,xc,zc);
#endif

         iz  = zc - z_min;
         ix  = xc - x_min;
         spt[ispt].off = x_size*ix;
         ii = spt[ispt].off + iz;

         if (ix < 0 || ix > x_size - 1 || iz < 0 || iz > z_size - 1)
         {
#ifdef DPRINT
	      printf("\n out of domain %d  %d ",ix,iz);
#endif
	      continue;
	 }

	 spt[ispt].rx  = rx;
         spt[ispt].rz  = rz;
         spt[ispt].xc  = xc;
         spt[ispt].zc0 = zc0;
         spt[ispt].zc  = zc;
         spt[ispt].ix  = ix;
         spt[ispt].iz  = iz;
         spt[ispt].ii  = ii;

	 /* ii     is a pixel index                               */
	 /* ilist  is the position of the pixel index in the list */

	 if (ii >= 0 && ii < xz_size)
	 {
            PixelOcc[ii]++;

            if (PixelOcc[ii] == 1)
            {
               OccupiedPixels[iNumOccupiedPixels] = PixelOcc + ii;
               iNumOccupiedPixels++;
            }
         }
         else
         {
            printf("\n error: out of bounds ii=%d",ii);
         }
      }
   }
   else
   {
      for (ispt = 0; ispt < spt_n; ispt++)
      {
         /* the cte part - the variable part */
         /* zc = spt[ispt].zc0 - z0/spt[ispt].rz;  */
         zc = spt[ispt].zc0 - z0 * spt[ispt].rz;

         iz  = zc-z_min;
         if (iz < 0 || iz > z_size)
         {
            /*printf("\n out of domain %d  %d ",ix,iz);*/
            continue;
         }

         spt[ispt].zc  = zc;
         spt[ispt].iz  = iz;
         spt[ispt].ii  = ii  = spt[ispt].off + iz;

         /* ii     is a pixel index                               */
         /* ilist  is the position of the pixel index in the list */

         if (ii >= 0 && ii < xz_size)
         {
            PixelOcc[ii]++;

            if (PixelOcc[ii] == 1)
            {
               OccupiedPixels[iNumOccupiedPixels] = PixelOcc + ii;
               iNumOccupiedPixels++;
            }
         }
         else
         {
            printf("\n error: out of bounds ii=%d",ii);
         }
      }
   }

   /* count the number of groups */

   ntrack = 0;
   for (iptr = 0; iptr < iNumOccupiedPixels; iptr++)
   {
      if (*(OccupiedPixels[iptr]) > 1)
         ntrack++;
      
      *OccupiedPixels[iptr] = 0;
      OccupiedPixels[iptr] = NULL;
   }
   iNumOccupiedPixels = 0;

#ifdef DEBUG
   printf ("z0 = %12.5f  ntracks = %i\n", z0, ntrack);
#endif

   return -ntrack;
}



/* range x : minimum z, y: maximum z   */
/* ev    an event structure            */
/* returns the z yielding the maximum of tracks  */

double 
sft_vf_max (double *zlow, double *zhigh)
{
   int      ipass,              /* pass index                               */
            istep,
            i_max,
            icall;
   long     ntracks[50],        /* array to keep results of the calculation */
            ntrack,
            ntracks_max,
            ntracks_min,        /* peak found                               */
            m0,
            m1;                 /* 0th, 1st moment                          */
   double   step_size,          /* increment                                */
            z,                  /* current vertex position assumption       */
            zmax,               /* z at the maximum found                   */
            a;

   ntracks_max = 0;
   i_max       = -1;
   step_size   = (*zhigh - *zlow) / 3;
   icall       = 1;
   z           = *zlow;

  /* do NO_STEPS no matter what */

   for (istep = 0; istep < NO_STEPS; istep++)
   {
      vf_ntrack[istep].prev = istep-1;
      vf_ntrack[istep].next = istep+1;
      vf_ntrack[istep].z    = z;
      vf_ntrack[istep].n    = sft_vf_ntrack (z);

      /*printf("\n z=%f   n=%d",z, vf_ntrack[istep].n );*/

      if (vf_ntrack[istep].n > ntracks_max)
      {
         i_max       = istep;
         ntracks_max = vf_ntrack[istep].n;
      }

      z += step_size; 
      icall++;
   }

   /* check if the max is at either edges and do a few more steps */

   if (i_max == 0)
   {
      z = *zlow - step_size;
      for (istep = NO_STEPS; istep < 40; istep++)
      {
         vf_ntrack[istep].prev = istep+1;

         if (istep == NO_STEPS)
            vf_ntrack[istep].next = 1;
         else
            vf_ntrack[istep].next = istep-1;

         vf_ntrack[istep].z    = z;
         vf_ntrack[istep].n    = sft_vf_ntrack (z);

         if (vf_ntrack[istep].n > ntracks_max)
         {
            i_max = istep;
            ntracks_max = vf_ntrack[istep].n;
         }
         else
            break;

         z -= step_size;
      }

      if (i_max == 39)
         return (-1000);  /* failure to find the vertex */
   }

   if (i_max == NO_STEPS - 1)
   {
      for (istep = NO_STEPS; istep < 40; istep++)
      {
         vf_ntrack[istep].prev = istep-1;
         vf_ntrack[istep].next = istep+1;
         vf_ntrack[istep].z    = z;
         vf_ntrack[istep].n    = sft_vf_ntrack(z);

         if (vf_ntrack[istep].n > ntracks_max)
         {
            i_max = istep;
            ntracks_max = vf_ntrack[istep].n;
         }
         else
            break;
         z += step_size;
      }

      if (i_max == 39)
         return (-1000);  /* failure to find the vertex */
   }

   /* zoom in - use the old points - essentially a binary search */

   while (step_size > MIN_STEP_SIZE)
   {
      istep++;
      step_size /= 2.5;
      z = vf_ntrack[i_max].z - step_size;
      vf_ntrack[istep].prev = vf_ntrack[i_max].prev;
      vf_ntrack[istep].next = i_max;
      vf_ntrack[istep].z    = z;
      vf_ntrack[istep].n    = sft_vf_ntrack(z);
      vf_ntrack[i_max].prev = istep;
      if (vf_ntrack[istep].n > ntracks_max)
      {
         i_max = istep;
         ntracks_max = vf_ntrack[istep].n;

         /*
         printf ("\n  istep %i imax %d zmax %f",
         istep, i_max, vf_ntrack[i_max].z);
         */

         continue;
      }

      istep++;
      z = vf_ntrack[i_max].z+step_size;
      vf_ntrack[istep].prev = i_max;
      vf_ntrack[istep].next = vf_ntrack[i_max].next;
      vf_ntrack[istep].z    = z;
      vf_ntrack[istep].n    = sft_vf_ntrack(z);
      vf_ntrack[i_max].next = istep;
      if (vf_ntrack[istep].n>ntracks_max)
      {
         i_max = istep;
         ntracks_max = vf_ntrack[istep].n;
      }
   }

   printf ("sft_vf_max() : %12.5f  %d    %d\n",
            vf_ntrack[i_max].z, vf_ntrack[i_max].n, istep);

   return (vf_ntrack[i_max].z);
}


/* just a one scan */

void
sft_z_scan (double z_low, double z_high)
{
   int     ipass,            /* pass index                                */
           istep,
           icall;
   long    ntracks[50],      /* array to keep results of the calculation  */
           ntrack;
   double  step_size,        /* increment                                 */
           z,                /* current vertex position assumption        */
           zmax;             /* z at the maximum found                    */
   char    fname[40];
   FILE   *fout;

   fout = fopen ("fine_sace.dat", "w");

   icall = 1;
   step_size = (z_high - z_low) / (NO_STEPS - 1);
   z = z_low;
   for (istep = 0; istep < NO_STEPS; istep++)
   {
      ntrack = sft_vf_ntrack (z);
      fprintf (fout,"\n %f   %d",z, ntrack);
      z += step_size;
      icall++;
   }

   fclose (fout);
}



/* reset sft_lookup for new event */


void
sft_vf_init (void)
{
   int iptr;

   x_size  = SPT_X;
   z_size  = SPT_Z;
   xz_size = x_size * z_size;
   x_min   = -3.3;
   z_min   = -2.;
   x_max   =  3.3;
   z_max   =  2.;
   x_scale = x_size / (x_max - x_min);
   z_scale = z_size / (z_max - z_min);
   x_min   = x_scale * x_min;
   z_min   = z_scale * z_min;

   for (iptr = 0; iptr < iNumOccupiedPixels; iptr++) 
   {
      *OccupiedPixels[iptr] = 0;
      OccupiedPixels[iptr] = 0;
   }

   iNumOccupiedPixels = 0;
}


int
sft_which_octant (long ispt)
{
   /*  printf ("\n x,y,z: %d %f %f %f",
               ispt, spt[ispt].x[0], spt[ispt].x[1], spt[ispt].x[2]);
   */

   if (spt[ispt].x[0] >= 0.)
   {
      if (spt[ispt].x[1] >= 0.)
      {
         if (spt[ispt].x[2] >= 0.)
            return(0);
         else
            return(1);
      }
      else
      {
         if (spt[ispt].x[2]>=0.)
            return(2);
         else
            return(3);
      }
   }
   else
   {
      if (spt[ispt].x[1]>=0.)
      {
         if (spt[ispt].x[2]>=0.)
            return(4);
         else
            return(5);
      }
      else
      {
         if (spt[ispt].x[2]>=0.)
            return(6);
         else
            return(7);
      }
   }
}


/* split the events in octants and write them to file */


void 
sft_split_one (FILE *fout)
{
   static int    iev;
          int    octant[8],
                 oct[8],
                 ioct,
                 the_oct;
          long   ispt;
 
   for (ispt = 0; ispt < 8; ispt++)
   {
      oct[ispt]=0;
      octant[ispt]=0;
   }

   /* first count the number of particles in each octant */
   for (ispt = 0; ispt < spt_n; ispt++)
   {
      ioct = sft_which_octant (ispt);
      octant[ioct]++;
   }

   /* write the 8 octants as separate events */

   printf("sft_split_one() : %5d %5d %5d %5d  %5d %5d %5d %5d\n",
	  octant[0],octant[1],octant[2],octant[3],
	  octant[4],octant[5],octant[6],octant[7]);

   for (ioct = 0; ioct < 8; ioct++)
   {
      iev++;
      fprintf (fout,"%d   %d\n", iev, octant[ioct]);
      for (ispt = 0; ispt < spt_n; ispt++)
      {
         the_oct = sft_which_octant (ispt);
         if (ioct == the_oct)
         {
            oct[ioct]++;
            sft_split_print_one (fout, oct[ioct], ispt);
         }
      }
   }
} 

/* end of sft_split_one() */


/* shift the event in the z direction by z */


void 
sft_shift (double offset)
{
   int ispt;

   for (ispt = 0; ispt < spt_n; ispt++)
      spt[ispt].x[2] += offset;
}


