/*

 FILE:       l3totphit.cxx
 HISTORY: 
                clean up 27. January 2000
                written September 1999

 Author :    flierl@bnl.gov      

*/  

#include "l3totphit.h"
#ifndef __CC5__
#include "sl3CoordinateTransform.h"
#else
#include <sl3CoordinateTransform.h>
#endif
#include <Rtypes.h> /* use ROOT variables: ..._t */

long type_of_call l3totphit_(
			     TABLE_HEAD_ST       *hitarray_h,       HITARRAY_ST         *hitarray ,
			     TABLE_HEAD_ST          *tpHit_h,      TCL_TPHIT_ST            *tpHit )
{
  /*
    ROUTINE:    l3totphit_
    ARGUMENTS:
    IN:
    hitarray    - PLEASE FILL IN DESCRIPTION HERE
    hitarray_h   - header Structure for hitarray
    OUT:
    tpHit    - PLEASE FILL IN DESCRIPTION HERE
    tpHit_h   - header Structure for tpHit
  */

  Int_t index;
  Int_t sector,supersector;
  Int_t rbindex,rboffset;
  Int_t mzindex,mzoffset,mzlength;
  UInt_t* bank = NULL ;    // DATAWORDS (=4 BYTE) to be written out in DAQ-Format 
  Int_t hitcounter = 0;
  Int_t cluspersec = 0;
    
  /* find start value in tphit array */
  /* bad way ... not very secure !   */
  for( index = 0 ; index < 50000 ; index ++ )
    {
      if ( tpHit[index].x == 0 && tpHit[index].y == 0 && tpHit[index].z == 0 )
	{
	  hitcounter = index;
	  break;
	}
    }

    
  /* get supersector */
  bank = (UInt_t*) hitarray;
  supersector = (Int_t) (bank[3]+1)/2;
  /*printf("l3totphit :  now converting supersector :%d \n",(bank[3]+1)/2); */
  /*printf("\nsupsec : %d",(bank[3]+1)/2); */

  /********************************************************************/
  /* loop over receiverboards : 6 in each sector = 12 per supersector */
  /********************************************************************/
  for(rbindex = 1 ;rbindex <=12;rbindex++)
    {
      // get offset
      rboffset = (Int_t) bank[10+2*(rbindex-1)];
      // get sector number 
      if (rbindex<=6) 
	{ 
	  sector=2*supersector-1; 
	}
      else if ( rbindex>6 && rbindex <=12 ) 
	{ 
	  sector=2*supersector;
	};
      
      /*printf("l3totphit :  now converting rb:%d   ",rbindex); */
      /*printf("\nrb:%d   ",rbindex); */
      /* printf("offset:%d   length:%d\n",rboffset,bank[11+2*(rbindex-1)]); */

      /***********************/
      /* loop over mezzanine */
      /***********************/
      for(mzindex = 1 ; mzindex <= 3 ; mzindex++)
	{
	 
	  mzoffset = (Int_t) bank[rboffset+10+2*(mzindex-1)];
	  mzlength = (Int_t) bank[rboffset+11+2*(mzindex-1)];

	  /* printf("offset:%d   length:%d\n",mzoffset,mzlength); */
	  /*printf("\t mz:%d",mzindex); */ 

	  /* is mz empty ? */
	  if ( mzlength == 11 ) 
	    {
	      /*    printf("\t\t 0 clusters on this mz \n"); */
	      /* printf("\t 0 "); */
	    } 
	  else /* mz is not empty ! */
	    { 
	      Int_t nrows,row,clusternumb,rowindex,rowoffset;
	      /******************/
	      /* loop over rows */
	      /******************/
	      nrows = (Int_t) bank[rboffset+mzoffset+10];
	      rowoffset = 0;
	      for ( rowindex = 1 ; rowindex <= nrows ; rowindex++ )
		{ 
		  Int_t clusindex;
		  /* get row number */ 
		  row = bank[rboffset+mzoffset+10+1+rowoffset];
		  /* get cluster number  */ 
		  clusternumb = bank[rboffset+mzoffset+10+2+rowoffset];

		  /* printf("\t\t %d clusters on row : %d\n",clusternumb,row); */
		  /* printf("\t%d/%d",row,clusternumb); */
		  
		  /**********************/
		  /* loop over clusters */
		  /**********************/
		  for ( clusindex = 1 ; clusindex <= clusternumb ; clusindex++)
		    {
		      Double_t pad,time;
		      UInt_t padinfo;
		      UInt_t flaginfo;
		      UShort_t flag;
		      UShort_t charge;
		      Double_t xyz[3];

		      struct dataword 
		      {
			unsigned short info1;
			unsigned short info2;
		      } *dword;

		      /* extract the pad & time center of gravity */
		      padinfo = (UInt_t) bank[rboffset+mzoffset+10+2+rowoffset+(2*clusindex-1)];
		      dword = (struct dataword*) &padinfo;
		      pad  = (Double_t) (dword->info1)/64;
		      time = (Double_t) (dword->info2)/64;

		      /* etract flag & time */
		      flaginfo = (UInt_t) bank[rboffset+mzoffset+10+2+rowoffset+(2*clusindex-1)+1];
		      dword= (struct dataword*) &flaginfo;
		      flag   = (UShort_t) (dword->info1);
		      charge = (UShort_t) (dword->info2);

		      /* coordinate transformation */
		      rawToGlobal(sector,row,pad,time,&xyz[0],&xyz[1],&xyz[2]);
		      /*printf("sec:%d  row:%d  pad:%f time:%f  ",sector,row,(float)pad,(float)time); */
		      /*printf("x:%f  y:%f  z:%f\n",(float)xyz[0],(float)xyz[1],(float)xyz[2]); */

		      /* filling of the structs */
		      tpHit[hitcounter].x =  (Float_t) xyz[0];
		      tpHit[hitcounter].y =  (Float_t) xyz[1];
		      tpHit[hitcounter].z =  (Float_t) xyz[2];
		      tpHit[hitcounter].q   =  (Float_t) charge;
		      tpHit[hitcounter].row =  (Short_t) 100 * sector + row; // to store the sector
		      tpHit[hitcounter].id  =  (Long_t)  1 + hitcounter;  // to start with 1

		      /* couting */
		      hitcounter++;
		      cluspersec++;
		    }
		  /* set offset for next row */
		  rowoffset += ((2 * clusternumb) + 2); 
		}
	    }
	  /* printf("\t\t # clusters on this mz : %d\n",clusternumb); */
	}
    }

  printf("Clusters in supsector %d: %d  \n",supersector,cluspersec);
  /*for (index =0;index<34;index++)
    {
    printf("%d\t",bank[index]); 
    };
    sector = 1;
    row = 1;
    pad =1;
    time =1;
   
    rawToGlobal(sector,row,pad,time,&xyz[0],&xyz[1],&xyz[2]);
    printf("\nx: %f  y:%f  z:%f\n",xyz[0],xyz[1],xyz[2]);
  */

  return 1;
}




