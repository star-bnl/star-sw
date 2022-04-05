#include <stdio.h>
#include <string.h>
#include <math.h>
/*********************************************************************
 * $Id: L2Histo.cxx,v 1.6 2010/04/18 06:05:32 pibero Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 *  primitive 1-D or 2-D histograming I/O
 *  all indexes run [0,nb-1]
 *********************************************************************
 */

#include "L2Histo.h"

//=====================================
//=====================================
void
L2Histo::set( int idx, const char *tit, int nbx, int nby) {
  if (!(idx>0 && nbx >0 && nby >0 && tit))
    {
      head.hid=1;
      head.nBin=1;
      head.nBinX=1;
      head.nBinY=1;
      head.dataSize=head.nBin*sizeof(int);
      data=new int [head.nBin];
      memset(data,0,head.dataSize);
      head.title[0]=0;
      strncpy(head.title,"set(): HISTOGRAM IS BROKEN IN. CONTACT EXPERT.",mxTx);
      return;
    }

  head.nOver=head.nUnder=0;
  head.ver=version;
  head.hid=idx;
  head.nBin=nbx*nby;
  head.nBinX=nbx;
  head.nBinY=nby;
  head.dataSize=head.nBin*sizeof(int);
  data=new int [head.nBin];
  memset(data,0,head.dataSize);
  head.title[0]=0;
  strncpy(head.title,tit,mxTx);
}

//=====================================
//=====================================
void
L2Histo::setTitle(const char *tit){
  head.title[0]=0;
  if (!tit)
    {
      strncpy(head.title,"setTitle():  HISTOGRAM IS BROKEN. CONTACT EXPERT.",mxTx);
      return;
    }
  strncpy(head.title,tit,mxTx);
}

//=====================================
//=====================================
L2Histo::L2Histo(){
  head.nOver=head.nUnder=-1;
  head.ver=version;
  head.hid=0;
  head.nBin=0;
  head.nBinX=head.nBin;
  head.nBinY=1;
  head.dataSize=0;
  data=0;
  head.title[0]=0;
}

//=====================================
//=====================================
void
L2Histo::reset() {
  head.nOver=head.nUnder=0;
  if(head.dataSize>0) memset(data,0,head.dataSize);
}

//=====================================
//=====================================
void
L2Histo::print( int flag, FILE *fd){
  int dim=1;
  if(head.nBinY>1) dim=2;
  fprintf(fd," L2Histo::print h%d, %dD, tit='%s' head.nBin=%d nBinX=%d ver=%d\n", head.hid,  dim,  head.title,  head.nBin,  head.nBinX,head.ver);

  int i=0;
  int sum=0;
  for(i=0;i< head.nBin;i++) {
    sum+= data[i];
    if(flag) {
      if(dim==1) {
	fprintf(fd," bin=%2d value=%4d sum=%d\n",i, data[i],sum);
      }	else {
	int ix=i%head.nBinX;
	int iy=i/head.nBinX;
	fprintf(fd," bin=%d  ix=%d iy=%d value=%d \n",i,ix,iy, data[i]);
      }
    }
  }
  fprintf(fd,"   Counts: inRange=%d  nUnder=%d, nOver=%d\n",sum,  head.nUnder,  head.nOver);
}


//=====================================
//=====================================
void
L2Histo::printCSV(  FILE *fd){
  if (!fd) {
    printf("printCSV() passed bad output file!\n");
    return;
  }
  fprintf(fd,"#L2H%d,%d,%s,", head.hid,  head.nBin,  head.title);
  if(head.nBinY>1) {
    fprintf(fd,"ny=%d,CVS format not supported for 2D histos,\n",head.nBinY);
    return;
  }
  int i=0;

  for(i=0;i< head.nBin;i++)  fprintf(fd,"%d,",data[i]);
  fprintf(fd,"\n");
  return;
}


//=====================================
//=====================================
void
L2Histo::fill( int bin){
  /* should be:   assert(head.nBinY==1) 
     but it is an online code - proceed, good luck
  */ 
  if(bin<0) { head.nUnder++; return;}
  if(bin>=head.nBin) { head.nOver++; return;}
  data[bin]++;
}


//=====================================
//=====================================
void
L2Histo::fillW( int bin, int w){
  //Fill data[bin] with weight w.
  /* should be:   assert(head.nBinY==1) 
     but it is an online code - proceed, good luck
  */ 
  if(bin<0) { head.nUnder+=w; return;}
  if(bin>=head.nBin) { head.nOver+=w; return;}
  data[bin]+=w;
}

//=====================================
//=====================================
void
L2Histo::fill( int binX, int binY){
  /* will work fine for 1-D, unless binY is negative;
     just computes too much
     inverse is not true !
  */ 
  if(binX<0 || binY <0) { head.nUnder++; return;}
  if(binX>=head.nBinX || binY>=head.nBinY) { head.nOver++; return;}
  data[binY*head.nBinX+binX]++;
}

//=====================================
//=====================================
void
L2Histo::fillW( int binX, int binY, int w){
  /* will work fine for 1-D, unless binY is negative;
     just computes too much
     inverse is not true !
  */ 
  if(binX<0 || binY <0) { head.nUnder+=w; return;}
  if(binX>=head.nBinX || binY>=head.nBinY) { head.nOver+=w; return;}
  data[binY*head.nBinX+binX]+=w;
}

//=====================================
//=====================================
void
L2Histo::write(FILE *fd, int dbg) {
  if(fd==0) return;
  if( head.nBin<=0) return;
  if(dbg) print(dbg-1);
  char *c;
  unsigned int i;
  //.... save histo header
  c=(char *)&head;
  for(i=0;i<sizeof(L2Histo::Head);i++)  fputc(c[i],fd);
 
   //.... save histo data
  c=(char *) data;
  for(i=0;i< head.dataSize;i++)  fputc(c[i],fd);

} 


//=====================================
//=====================================
int
L2Histo::read(FILE *fd, int dbg) {
  if (!fd)
    {
      printf("L2Histo::read called with no file.  Aborting\n");
      return -1;
    }
  char *c;
  unsigned int i;
  
  // printf("aa=%d\n",sizeof(*h));
  //.... read histo header
  c=(char *)&head;
  for(i=0;i<sizeof(L2Histo::Head);i++) {
    int val=fgetc(fd);
    if(i==0 && val==EOF) return 1;
    if(!(val!=EOF))
      {
	printf("L2Histo::read  val==EOF.  Aborting\n");
	return -1;
      }
    c[i]=val;
  } 
  //  printf("rd ver %d %d \n", head.ver,version);
  if (!(  head.ver==version)) // change it in the future if header ever changes
    {
      printf("L2Histo::read head.ver!=version.  Aborting\n");
      return -1;
    }
  if (!( head.nBin>0))
    {
      printf("L2Histo::read  head.nBin<=0.  Aborting\n");
      return -1;
    }
  if (!( head.dataSize== head.nBin*sizeof(int)))
    {
      printf("L2Histo::read  head.dataSize!= head.nBin*sizeof(int).  Aborting\n");
      return -1;
    }

  data=new int [head.dataSize];
  memset( data,0, head.dataSize); 
  
  //.... read histo data
  c=(char *) data;
  for(i=0;i< head.dataSize;i++) {
    int val=fgetc(fd);
    if(dbg)printf("i=%d val=%d\n",i,val);
    if(i==0 && val==EOF) return 1;
    if(!(val!=EOF))
    {
      printf("L2Histo::read  val==EOF.  Aborting\n");
      return -1;
    }
    c[i]=val;
  }

 if(dbg) printf("got histo hid=%d  , head.nBin=%d, tit='%s'\n", head.hid,  head.nBin, head.title);

  if(dbg) print(1);
  return 0;
}


/*========================================
  ======================================== */
bool 
L2Histo::findMax( int *iMax, int *iFWHM) {
  // returns true if both max & FWHM make sense
  *iMax=*iFWHM=-1;  

  /* finds pedestal & FWHM */
  int maxVal=-1, maxJ=-1;
  int j;
  for(j=0; j<head.nBin;j++) {
    // printf("j=%d  maxVal=%d maxJ=%d  yield=%d\n",j,maxVal, maxJ,data[j]);
    if(data[j]<=maxVal) continue;
    maxVal=data[j];
    maxJ=j;
  }

  if(maxJ<0) return false; /* no maximum found */

  *iMax=maxJ; // maximum was found

  if(head.nBinY>1) { // FWHM makes no sense for 2D plots
   *iMax=maxJ;
   *iFWHM=999;
   return true;
  }


  // search for FWHM
  int halfMax=maxVal/2;
   int j1=-1, j2=-1;
   for(j=maxJ+1; j<head.nBin;j++) {// forward
     if(data[j]>halfMax) continue;
     j2=j;
     break;
   }

   for(j=maxJ-1; j>=0;j--) { // backward
     if(data[j]>halfMax) continue;
     j1=j;
     break;
   }
   if(j1<0 || j2<0) return false; /* FWHM is 0 ? */


   *iFWHM=(j2-j1);
 
   return true;
}

/*========================================
  ======================================== */
bool 
L2Histo::findMean( int *iMean, int *iRMS) {
  // returns true if both mean & RMS make sense

  *iMean=*iRMS=-1;  
  if (head.nBinY>1)
    return false; //this method doesn't work for 2d plots
  /* finds mean & RMS without counting overflows */
  int totalVal=0, weightedVal=0, meanJ=-1, rms=-1;
  int j;
  for(j=0; j<head.nBin;j++) {
    // printf("j=%d  maxVal=%d maxJ=%d  yield=%d\n",j,maxVal, maxJ,data[j]);
    weightedVal+=data[j]*j;
    totalVal+=data[j];
  }
  if (totalVal>0) meanJ=weightedVal/totalVal;
  if(meanJ<0) return false; /* no maximum found */

  *iMean=meanJ; // mean was found

  // search for RMS
  weightedVal=0;
  for(j=0; j<head.nBin;j++) {
    // printf("j=%d  maxVal=%d maxJ=%d  yield=%d\n",j,maxVal, maxJ,data[j]);
    weightedVal+=data[j]*(j-meanJ)*(j-meanJ);
  }
  rms=weightedVal/totalVal;

  *iRMS=rms; 
   return true;
}

//=====================================
//=====================================
void
L2Histo::printPed( FILE *fd, int x0, int x1,char term){
  if(head.nBinY>1) {
    fprintf(fd," printped NOT implemented for 2D histos: hid=%d tit=%s\n",head.hid, head.title);
    return;
  }
     int j;
      for(j=0; j<head.nBin;j++) {
	int adc=x0+j;
	if(adc>=x1) break;
	float val=data[j];
	if(val<=0) { // print grid for empty bins 
	  if(adc==0) 
	    fprintf(fd,"*");
	  else if(adc==10 ) 
	    fprintf(fd,">");
	  else if(adc==-10 ) 
	    fprintf(fd,"<");
	  else if(adc%10==0) 
	    fprintf(fd,",");
	  else
	    fprintf(fd,".");
	  continue;
	}
	// print yield for populated bins

	fprintf(fd,"%c",y2c(val));
      }

      // sum higher bins t print overflow
      int sum=head.nOver;
      for(j=x1; j<head.nBin;j++) sum+=data[j];
      
      fprintf(fd,"ov=%d %c",sum,term);
}

//=====================================
//=====================================
char
L2Histo::y2c(float val){
  float valLog=log(val);
  char k='?';
  if(valLog<0.) 
    k='-';
  else if(valLog<9) 
    k='1'+(int)valLog;
  else  
    k='a'+(int)(valLog-9);
  return k;
}



/*
*********************************************************************
  $Log: L2Histo.cxx,v $
  Revision 1.6  2010/04/18 06:05:32  pibero
  Address compiler warnings.

  Revision 1.5  2010/04/17 16:42:09  pibero
  *** empty log message ***

  Revision 1.4  2008/01/16 23:32:33  balewski
  toward token dependent compute()

  Revision 1.3  2008/01/10 22:45:39  balewski
  reduce memory allocation for every histo

  Revision 1.2  2007/11/14 03:58:07  balewski
  cleanup of common timing measurement

  Revision 1.1  2007/10/11 00:33:14  balewski
  L2algo added

  Revision 1.3  2006/03/28 19:33:23  balewski
  ver16b , in L2new

  Revision 1.2  2006/03/11 17:08:32  balewski
  now CVS comments should work

*/

