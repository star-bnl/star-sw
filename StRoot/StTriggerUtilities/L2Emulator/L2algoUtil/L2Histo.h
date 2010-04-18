#ifndef L2HISTO_h
#define L2HISTO_h
/*********************************************************************
 * $Id: L2Histo.h,v 1.4 2010/04/18 06:05:32 pibero Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 *  primitive 1-D or 2-D histograming I/O
 *  all indexes run [0,nb-1]
 *********************************************************************
 */


class L2Histo {
 private:
  enum {mxTx=200, version=12}; // max length of text lable
  struct Head {// keep it divisible by 4
    int ver; // to trace evolution of the header 
    char title[mxTx];
    int hid,nBin,nBinX,nBinY;  // nBinY=1 for 1-D, for 2D: nBin=nBinX*nBinY
    int  nUnder,nOver; // simplified for 2-D
    unsigned int dataSize;
  } head;
  int *data; // bin container
  char y2c(float val);// yield -->character
  void set( int id, const char *tit, int mxBinX, int mxBinY ); // generic histo init

 public:
  L2Histo();
  // all methods w/o  '2' ignore value of 'nBinY'

  L2Histo( int id, const char *tit, int mxBinX, int mxBinY ) { // 2D histo
    set(id, tit,mxBinX ,mxBinY); }
  L2Histo( int id, const char *tit, int mxBin) {  // 1D histo
    set(id, tit, mxBin,1 ); }
  void setTitle(const char *tit); // change title

  void print( int flag, FILE *fd=stdout);
  void printCSV( FILE *fd=stdout);
  void fill ( int binX); // 1D histo
  void fill ( int binX, int binY);  // 2D histo
  void fillW( int binX, int w); // weight !=1
  void fillW( int binX, int binY, int w);
  void write(FILE *fd, int dbg=0);
  int  read(FILE *fd, int dbg=0);
  void reset(); // erase all entries, retain definition
  int getNbin(){return head.nBin;}
  int getNbinX(){return head.nBinX;}
  bool is1D(){return head.nBinY==1;}
  bool is2D(){return head.nBinY>1;}
  int getId(){return head.hid;}
  int getNunder(){return head.nUnder;}
  int getNover(){return head.nOver;}
  bool findMax( int *iMax, int *iFWHM);
  bool findMean( int *iMean, int *iRMS);

  void printPed( FILE *fd, int x0, int maxBin,char term='\n');
  const int *getData(){ return data; }; 
  const char *getTitle(){ return head.title; } 
};


#endif 


/*
*********************************************************************
  $Log: L2Histo.h,v $
  Revision 1.4  2010/04/18 06:05:32  pibero
  Address compiler warnings.

  Revision 1.3  2010/04/17 16:42:09  pibero
  *** empty log message ***

  Revision 1.2  2007/11/02 03:03:41  balewski
  modified L2VirtualAlgo

  Revision 1.1  2007/10/11 00:33:15  balewski
  L2algo added

  Revision 1.3  2006/03/28 19:33:23  balewski
  ver16b , in L2new

  Revision 1.2  2006/03/11 17:08:32  balewski
  now CVS comments should work

*/

