/* Hey Emacs this is -*-C++-*- */
#ifndef EEfeeDataBlock_h
#define EEfeeDataBlock_h
/*********************************************************************
 * $Id: EEfeeDataBlock.h,v 1.12 2004/06/21 19:50:21 balewski Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Raw FEE Data Block
 *********************************************************************
 */
#include "TObject.h"

class EEfeeDataBlock :public TObject {

public:
  enum { WRDCNT=0,ERRFLG=1,TOKEN=2,CRATE=3};
  static const int DefaultMaxHead;
  static const int DefaultMaxData;
 
private:
  int      MaxHead;
  int      MaxData;
  UShort_t *head;//[MaxHead];
  UShort_t *data;//[MaxData]
  UChar_t  sanity;// encodes all corruptions, filled in fly

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

  UShort_t  getErrFlag() const { return  head[ERRFLG] & 0x0FFF; }
  UShort_t  getLenCount() const { return  head[WRDCNT] & 0x0FFF; }
  UShort_t  getToken()    const { return  head[TOKEN] & 0x0FFF; }
  UChar_t   getTrigComm() const { return  (head[CRATE] / 0x0100) &0x000F ; }
  UChar_t   getCrateID()  const { return  head[CRATE] & 0x00FF ; }
  UChar_t   getSanity()  const { return sanity;}
  int       getNData(int thres) const;
  void      maskCrate() { sanity|=0x80;}// mark 7th (not used) bit 
  void      setCrateID(UShort_t id ) { head[CRATE]= (head[CRATE]&0xFF00) + ( id& 0x00FF);}
  int       isValid() const  {return !sanity;}
  UChar_t   isHeadValid(int token, int crId, int len, int trigComm, int errFlag);

  ClassDef(EEfeeDataBlock,3) // Endcap Emc event
};
#endif

/*
Date: Fri, 02 Apr 2004 00:10:36 -0500
From: Gerard Visser <gvisser@iucf.indiana.edu>
To: Jan Balewski <balewski@iucf.indiana.edu>
Subject: Re: header

Hi Jan,
        First of all ALL numbers reported by ETOW and ESMD are 12-bits. DAQ
reads them as 16, but only the 12 least significant come over the fiber.
Tonko fills in the upper nibble with 0 but I don't know about any
guarantees.
        The 0th word is the length count. ESMD has 192+4 words, length count is
196=0x0c4. ETOW has 128+4 words, length count is 132=0xa4.
        The 1th word is the "error flags".
        The 2nd word is the token.
        The 3rd word is the trigger command nibble put together with the "RDO
ID" or "crate ID".
        The 4th and following words are the data (ADC values).
        See
http://www.iucf.indiana.edu/U/gvisser/STAR_EEMC/STAR_EEMC_DAQ_Data_Formats.pdf
(page 2 for instance).

        - Gerard

*/


/*
 * $Log: EEfeeDataBlock.h,v $
 * Revision 1.12  2004/06/21 19:50:21  balewski
 * mre detailed monitoring of data corruption
 *
 * Revision 1.11  2004/04/20 21:43:53  balewski
 * small change in data block header get now tagged
 *
 * Revision 1.10  2004/04/02 06:38:52  balewski
 * *** empty log message ***
 *
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
