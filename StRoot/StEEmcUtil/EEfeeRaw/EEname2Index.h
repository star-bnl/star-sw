#ifndef EEname2Index_h
#define EEname2Index_h

/*********************************************************************
 * $Id: EEname2Index.h,v 1.5 2004/08/05 18:43:09 balewski Exp $
 * $Log: EEname2Index.h,v $
 * Revision 1.5  2004/08/05 18:43:09  balewski
 * reduce memory consumption by factor 10
 *
 * Revision 1.4  2003/12/02 17:22:08  balewski
 * fix after version mixup
 *
 * Revision 1.2  2003/11/20 16:01:47  balewski
 * towars run 4
 *
 * Revision 1.1  2003/01/28 23:17:14  balewski
 * start
 *
 * Revision 1.1  2002/11/30 20:04:37  balewski
 * start
 *
 *********************************************************************/

 /* Descripion:

  1) Lets define a unique 'name' for any active element in EEMC
         name="ssTann" for towers, ss=sector 01-12, a=subsector='A' -'E'
                        nn=tower in eta =01-12
              "ssxkkk"  for SMD  x='U' or 'V' plain orientation,
                                 kkk=001-288 strip ID
              "ssPann"  for Pres1
              "ssQann"  for Pres2
              "ssRann"  for Post
              "ssLann"  for LED pixel

  2) Lets define sector oriented index for any   PMT/MAPMT pixel
       Index within sector:

         Tower   1- 60  =(sub-1)*5+eta, where sub=1-5, eta=1-12	  
          Pre1 101-160  =  the same
          Pre2 201-260  =  the same
          Post 301-360  =  the same
         smd-U 401-688   couting from shortest at eta=2
         smd-V 701-988   couting from shortest at eta=2

       Index jumps by 1000 from sector to sector
       --> add (sectorID-1)*1000

       Index total range [0-11999], spare ~15% or ~180/sector
	      
  */

//*********************************************************************

#define EEindexMax 12000 // use it to decalare any local array
#define EEindexVersion 100 // use assert(EEindexVersion==100) if you depend on particular indexing, but try not to.

int EEname2Index(const char *name);
void EEindex2Name(int index, char * name);
void  EEindexRange(int secID,int &ix1, int &ix2);

#endif

