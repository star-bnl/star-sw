/* Hey Emacs this is -*-C++-*- */
#ifndef EEfeeDataBlock_h
#define EEfeeDataBlock_h
/*********************************************************************
 * $Id: EEfeeDataBlock.h,v 1.9 2004/03/25 16:54:59 balewski Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Raw FEE Data Block
 *********************************************************************
 */
#include "TObject.h"

class EEfeeDataBlock :public TObject {

public:
  enum { EVTYPE=0,WRDCNT=1,TOKEN=2,CRATE=3};
  static const int DefaultMaxHead;
  static const int DefaultMaxData;
 
private:
  int      MaxHead;
  int      MaxData;
  UShort_t *head;//[MaxHead];
  UShort_t *data;//[MaxData]
  
public:  
  EEfeeDataBlock();
  EEfeeDataBlock(const EEfeeDataBlock *b);

  virtual ~EEfeeDataBlock();
  void print(int flag=1);
  void clear();
  void set         (const EEfeeDataBlock *b);
  void setHead     (const UShort_t* h );
  void setData     (int chan, UShort_t d);
  void setDataArray(const UShort_t *d,  int size);


  UShort_t* getData() const { return data; };
  UShort_t* getHead() const { return head; };
  
  int       getDataLen() const { return MaxData; }
  int       getValidDataLen() const {return getDataLen(); } //bckwd compat.  
  int       getHeadLen() const { return MaxHead; }

  UShort_t  getToken()    const { return  head[TOKEN];           }
  UChar_t   getTrigType() const { return  head[CRATE] / 0x0100 ; }
  UChar_t   getCrateID()  const { return  head[CRATE] & 0x00FF ; }
  int       getNData(int thres) const;
  void      maskCrate() {head[CRATE]=0xFFFF;}
  void      setCrateID(UShort_t id ) { head[CRATE]= (head[CRATE]&0xFF00) + ( id& 0x00FF);}
  int       isValid();
  
  ClassDef(EEfeeDataBlock,1) // Endcap Emc event
};
#endif

/*
 * $Log: EEfeeDataBlock.h,v $
 * Revision 1.9  2004/03/25 16:54:59  balewski
 * cleanup of arguments
 *
 * Revision 1.8  2004/03/20 20:25:55  balewski
 * *** empty log message ***
 *
 * Revision 1.7  2004/01/27 15:13:57  balewski
 * it is tricky with BTOW
 *
 * Revision 1.6  2003/12/04 18:29:25  balewski
 * I forgot
 *
 * Revision 1.5  2003/12/02 17:22:08  balewski
 * fix after version mixup
 *
 * Revision 1.3  2003/11/24 05:40:55  balewski
 * new stuff for miniDaq
 *
 * Revision 1.2  2003/11/20 16:01:46  balewski
 * towars run 4
 *
 * Revision 1.1  2003/01/28 23:17:14  balewski
 * start
 *
 * Revision 1.5  2002/12/19 22:22:56  zolnie
 * fixed trig type bug
 *
 * Revision 1.4  2002/12/19 21:54:32  zolnie
 * updates for real fee data
 *
 * Revision 1.3  2002/12/04 19:13:16  zolnie
 * fixed bug in setData
 *
 * Revision 1.2  2002/12/03 23:48:52  zolnie
 * changed back to var length
 *
 * Revision 1.1  2002/11/30 20:04:37  balewski
 * start
 *
 *
 *********************************************************************/
