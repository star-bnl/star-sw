#ifndef BsmdRawData_h
#define BsmdRawData_h
/*********************************************************************
 * $Id: BsmdRawData.h,v 1.1 2004/06/18 01:46:35 balewski Exp $
 *********************************************************************
 * container for STAR BEMC SMD  data
 */

#include <TObject.h>
class TArrayS ;
class TArrayC ;

class BsmdRawData : public TObject {
 public:
  // data containers
  TArrayS *used; // used data blocks from DAQ are marked with 1
  TArrayC *caps; // capacitors for all data blocks
  int nBlock;// number of nonzero blocks
  TArrayS *head; // stores only nonzero header blocks
  TArrayS *data; // stores only nonzero data blocks

  // methods
  BsmdRawData();
  virtual ~BsmdRawData();
  void  print(int k=0, FILE *fd=stdout) const;
  void  clear();
  int headSize() const;
  int dataBlockSize() const;

  ClassDef(BsmdRawData,1) 

};
#endif


/*
 * $Log: BsmdRawData.h,v $
 * Revision 1.1  2004/06/18 01:46:35  balewski
 * added BSMD to ezTree
 *
 *********************************************************************/
