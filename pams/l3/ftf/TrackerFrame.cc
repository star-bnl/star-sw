//:>------------------------------------------------------------------
//: FILE:       TrackerFrame.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       TrackerFrame
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FTFinder.h"
#include "TrackerFrame.h"

extern FTFinder      tracker ;
FILE	             *datafile;


//********************************************************************
//     Closes input ascii file
//********************************************************************
int TrackerFrame::CloseAscii ( )
{
  fclose(datafile);
  return 0;
}
int TrackerFrame::Done ( ){
  return 0 ;
}
//********************************************************************
//       Called when an event is finished
//********************************************************************
void TrackerFrame::EventReset ( ) {
   if ( tracker.hit      != 0 ) {
      delete []tracker.hit ;
      tracker.hit = 0 ;
   }	
   if ( tracker.track    != 0 ) {
	delete []tracker.track ;
	tracker.track = 0 ;
   }
   if ( tracker.mc_track != 0 ) {
	delete []tracker.mc_track ;
	tracker.mc_track = 0 ;
   }
   return ;
}
//********************************************************************
//        Read Event
//********************************************************************
int TrackerFrame::GetEvent( ) 
{
  int   row, id_kine ;
  int   id, pid  ;
  int   i, index, index1 ;
  float x,y,z,dx,dy,dz,q;
  float px, py, pz, pt ;
  float r ;
  float phi, phi_min, phi_max ;
  int   bye = -1 ;
  int nhits, max_tracks, n_mc_tracks, max_pnt_kine ;
//
//   Get # hits
//
  if ( datafile == 0 ) {
	printf ( " No file open \n " ) ;
	return 1 ;
  }
//  
  int ok = fscanf(datafile, "%d ", &nhits) ;
  if( ok == EOF || ok != 1 ) {
	  printf ( " End of file  \n " ) ;
	  return 1 ;
  }
//
//    Reset event
//
  EventReset ( ) ;
//
//   Allocate memory
//
  max_tracks   = nhits / 2 ;
  max_pnt_kine = min(nhits * 500,50000) ;

  FTF_Hit*   hit         = new FTF_Hit[nhits] ;
  FTF_Track* track       = new FTF_Track[max_tracks] ;
  int*       pnt_kine    = new int[max_pnt_kine] ;
     
//  memset ( hit,   0, n_hits*sizeof(FTF_Hit) ) ;
//  memset ( track, 0, max_tracks*sizeof(FTF_Track) ) ;
  memset ( pnt_kine, bye, max_pnt_kine*sizeof(int) ) ;
//
//   Loop over hits
//
  i = 0 ;
  phi_min = 2.F * Pi ;
  phi_max = 0.F ;
  while ( i < nhits ) 
  {
    if ( fscanf ( datafile, "%e %e %e %e %e %e %e %d %d",
               &x,&y,&z,&dx,&dy,&dz,&q,&row,&id_kine) == EOF )
    {
       printf ( " \n Unexpected EOF \n " ) ;
       return 1 ;
    }
//
//   Check errors
//
	if ( dx <= 0 ) {
		printf ( " Hit %d has x xerror = %f \n", i, dx ) ;
		dx = 0.01F ;
	}
//
	if ( dy <= 0 ) {
		printf ( " Hit %d has y error = %f \n", i, dy ) ;
		dy = 0.01F ;
	}
//
	if ( dz <= 0 ) {
		printf ( " Hit %d has z error = %f \n", i, dz ) ;
		dz = 0.01F ;
	}
//
//    Store values
//
    hit[i].i_r      = (short)fmod(row,100);
    hit[i].x        = x;
    hit[i].y        = y;
    hit[i].z        = z - tracker.para->z_vertex ;
    hit[i].dx       = dx ;
    hit[i].dy       = dy ;
    hit[i].dz       = dz ;
    r               = (float)sqrt(x*x+y*y+z*z);
    hit[i].r        = r ;
//
//   If the deposit energy is negative
//   then it is a link to a mirror hit
//   This is not an ideal solution I know
//
	if ( q > 0.F ) {
		hit[i].q        = q;
		hit[i].nxghit   = 0 ;
	}
	else {
		int index = int(-1*q) ;
		if ( index > -1 && index < nhits ){
			hit[i].nxghit = &hit[index] ;
		}
		else {
			printf ( " GetEvent: wrong mirror hit %d ", index ) ;
		}
	}
    hit[i].eta      = (float)seta ( r, z ) ;
    phi             = (float)atan2 ( y, x ) ;
    if ( phi < 0       ) phi = phi + 2.F * Pi ;
    if ( phi > 2. * Pi ) phi = 2.F * Pi - phi ;
    if ( phi > phi_max ) phi_max = phi ;
    if ( phi < phi_min ) phi_min = phi ;
    hit[i].phi      = phi ;
    hit[i].mc_track = id_kine ;
    hit[i].nxmhit   = 0    ;
    i++;
  }
  
//
//    Get mc tracks (mkine)
//
  ok = fscanf(datafile, "%d ", &n_mc_tracks) ;
  if ( ok == EOF || ok != 1 ){
	  printf ( " End of file \n " )  ;
	  return 1 ;
  }

  
  int* last_hit = new int[n_mc_tracks] ;

  FTF_Mc_Track* mc_track = new FTF_Mc_Track[n_mc_tracks] ;
  memset ( last_hit, bye, n_mc_tracks*sizeof(int) ) ;
//  memset ( mc_track, 0, n_mc_tracks*sizeof(FTF_Mc_Track) ) ;
//--
//--   Loop over mc tracks 
//
  i = 0 ;
  while ( i < n_mc_tracks )
  {
    if ( fscanf(datafile, "%d %d %e %e %e", &id, &pid, &px, &py, &pz ) == EOF ) {
       printf ( " \n Unexpected EOF when reading mkine \n " ) ;
       return 1 ;
    }
    pt                     = (float)sqrt ( px * px + py * py ) ;
    mc_track[i].pid        = (short)fmod(pid,100) ;
	mc_track[i].parent_pid = (short)fmod(pid,10000)/100 ;
    mc_track[i].p[0]       = px  ;
    mc_track[i].p[1]       = py  ;
    mc_track[i].p[2]       = pz  ;
    mc_track[i].pt         = pt  ;
    if ( id > 0 && id < max_pnt_kine )
       pnt_kine[id] = i ;
    else
       printf ( " \n Kine key too high %d \n ", id ) ;
//
//   Read space point info if it is available
//  
	if ( int(pid/10000) > 0 ) {
		int n_mc_spc ;
		ok = fscanf(datafile, "%d ", &n_mc_spc) ;
        if ( ok == EOF || ok != 1 ){
			printf ( " \n Reading MC space points:: EOF  " )  ;
	        return 1 ;
		}
		if ( mc_track[i].hit != 0 ) delete mc_track[i].hit ;
		mc_track[i].hit = new FTF_Mc_Space_Point[n_mc_spc] ;
		mc_track[i].n_mc_hits = n_mc_spc ;
//
//    Read space points now
//
		for ( int j = 0 ; j<n_mc_spc ; j++ ) {
		   if ( fscanf(datafile, "%e %e %e", &x, &y, &z ) == EOF ) {
                printf ( " \n Unexpected EOF when reading  " ) ;
			    printf ( " \n MC Space Points  " ) ;
				return 1 ;
		   }
           mc_track[i].hit[j].x = x ;
		   mc_track[i].hit[j].y = y ;
		   mc_track[i].hit[j].z = z ;
        }
	}		
    i++;
  }
/*--
 *--     Set pointers for mc tracks
*/

  for ( i=0 ; i<nhits ; i++ )
  {
     index1       = hit[i].mc_track ;
     if ( index1 > 0 && index1 < max_pnt_kine )
     {
        index = pnt_kine[index1] ;
        hit[i].mc_track = index ;
        if ( index == bye ) continue ; 
        if ( index < bye || index > n_mc_tracks )
        {
           printf ( "\n fft_read_star: index1 index %6d %6d ", index1, index ) ;
           continue ; 
        }
     }
     else
        continue ;

     if ( index > n_mc_tracks || index < 0 )
     {
        printf ( "\n crazy index %d ", index ) ; 
     }

     if (mc_track[index].first_hit == 0 )
        mc_track[index].first_hit = &hit[i] ;
     else
        hit[last_hit[index]].nxmhit = &hit[i] ;
          last_hit[index] = i ;
  }

 tracker.n_hits      = nhits ;
 tracker.n_mc_tracks = n_mc_tracks ;
 tracker.max_tracks  = max_tracks ;
 tracker.hit         = hit ;
 tracker.track       = track ;
 tracker.mc_track    = mc_track ;

 delete []pnt_kine ;
 delete []last_hit ;
 

  return 0 ;
}
//*********************************************************************
//       Initializes Framework
//*********************************************************************
void TrackerFrame::Init (  ) 
{ 
//
//      Set event counter to zero
//
   event = 0 ;
   tracker.n_hits = 0 ;
   tracker.Reset ( );

   return ;
}
//*******************************************************************
//     Open Input Ascii File
//*******************************************************************
int TrackerFrame::OpenAscii (char *datafile_name)
{
  int nhits ;
  nhits = 0 ;
  datafile = fopen(datafile_name, "r");
  if (datafile == NULL)
  {
    printf ( " \n Error opening %s \n ", datafile_name ) ;
    return 1 ;
  }
  return 0 ;
}
