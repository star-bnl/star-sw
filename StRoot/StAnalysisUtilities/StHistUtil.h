// $Id: StHistUtil.h,v 2.5 2006/05/18 16:38:03 genevb Exp $
// $Log: StHistUtil.h,v $
// Revision 2.5  2006/05/18 16:38:03  genevb
// Introduce StHistUtil::GetRunYear()
//
// Revision 2.4  2006/03/28 21:35:32  genevb
// Single page output capability for eps,jpg,png,gif,tiff,etc. [see TPad::Print()]
//
// Revision 2.3  2006/03/28 01:58:39  genevb
// Allow PDF (and other) output formats (was only PostScript)
//
// Revision 2.2  2002/09/06 02:51:34  genevb
// Remove limit on maximum number of histograms that can be copied
//
// Revision 2.1  2000/08/25 22:06:50  genevb
// Added histo descriptor in top right
//
// Revision 2.0  2000/08/25 15:47:38  genevb
// New revision: cleaned up, multiple PS files
//
//
///////////////////////////////////////////////////////////////////////////////
// Histogram Utility methods for use with star makers and bfc output
///////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StHistUtil
#define STAR_StHistUtil


#ifndef ROOT_TH1
#include "TH1.h"
#endif

#ifndef ROOT_TH2
#include "TH2.h"
#endif

#include "TList.h"
#include "TString.h"


//  - if not using the methods of the class, then can just put class TCanvas;
//   -  however, if we are using the methods of TCanvas, then put include "TCanvas.h"
class TCanvas;
class StMaker;
class TPaveLabel;
class TPaveText;
class TDatime;

class StHistUtil {

 private:
  // Data-members to make up the output Canvases and Postscript files
  TCanvas* m_HistCanvas; //!
  Int_t   m_PadColumns;  // Number of the columns (TPad's) on the single Canvas
  Int_t   m_PadRows;     // Number of the columns (TPad's) on the single Canvas
  Int_t   m_PaperWidth;  // Paper size in cm
  Int_t   m_PaperHeight; // Paper size in cm
  TString m_FirstHistName;
  TString m_LastHistName;
  TString m_OutFileName; // Base name of the output file to plot hists out
  TString m_CurFileName; // Name of the current output file
  TString m_OutType;    // Output file type
  Bool_t  m_OutMultiPage; // Output file is multipage
  Int_t   m_CurPrefix;
  TString m_GlobalTitle; // Title at top of each page of output
  TPaveLabel* Ltitle;   //! PaveLabel with title for pages
  TPaveText*  Ldesc;    //! PaveLabel with descriptor for hists
  TList*  m_ListOfLogY; //! list of histogram names that will be drawn with logY scale
  TList*  m_ListOfLogX; //! list of histogram names that will be drawn with logX scale
  TList*  m_ListOfPrint;//! list of histogram names that will be drawn,printed
  StMaker* m_PntrToMaker;//! pointer to an St_Maker, so can find histograms
  Int_t   maxHistCopy;  //! size of array of new histograms
  TH1**   newHist;      //! array of new histograms that other will be copied into
  Bool_t  debug;
  Int_t   m_RunYear;    // Run year


 protected:
  virtual void    CloseOutFile();
  virtual Bool_t  CheckOutFile(const Char_t* histName);


 public: 
  StHistUtil();
  virtual        ~StHistUtil();
  virtual void    SetDebug(Bool_t dbg=kTRUE) { debug=dbg; }
  virtual Bool_t  Debug() { return debug; }
  virtual Int_t   DrawHists(Char_t *dirName="QA");
  virtual Int_t   ListHists(Char_t *dirName="QA");
  virtual TList*  FindHists(Char_t *dirName="QA");
  virtual Int_t   CopyHists(TList  *dirList);
  virtual Int_t   AddHists(TList  *dirList, Int_t nHistCopy=-1);
  virtual Int_t   PrintInfoHists(TList  *dirList,  const Char_t *fname="printinfo.out");

  virtual void    SetDefaultLogYList(Char_t *dirName="QA");
  virtual Int_t   AddToLogYList(const Char_t *HistName="");
  virtual Int_t   RemoveFromLogYList(const Char_t *HistName="");
  virtual Int_t   ExamineLogYList();

  virtual void    SetDefaultLogXList(Char_t *dirName="QA");
  virtual Int_t   AddToLogXList(const Char_t *HistName="");
  virtual Int_t   RemoveFromLogXList(const Char_t *HistName="");
  virtual Int_t   ExamineLogXList();

  virtual void    SetDefaultPrintList(Char_t *dirName="QA",Char_t *analType="FullTable");
  virtual Int_t   AddToPrintList(const Char_t *HistName="");
  virtual Int_t   RemoveFromPrintList(const Char_t *HistName="");
  virtual Int_t   ExaminePrintList();

  virtual Int_t   Overlay1D(Char_t *dirName,Char_t *inHist1,Char_t *inHist2);
  virtual Int_t   Overlay2D(Char_t *dirName,Char_t *inHist1,Char_t *inHist2);

  virtual Int_t   GetRunYear(const Char_t *filename);

// Inline methods
  void SetHistsNamesDraw(const Char_t *firstName="*", const Char_t *lastName="*");
  void SetZones(Int_t columns=2, Int_t rows=3);   
  // SetPaperSize -->  A4 is 20,26  US letter is 20,24
  void SetPaperSize(Int_t width=20, Int_t height=24);

  // SetOutFile -->  type="ps","pdf","jpg",etc...see TPad::Print()
  void SetOutFile(const Char_t *fileName="", const Char_t* type=0);
  void SetPostScriptFile(const Char_t *psFileName="");
  void SetPDFFile(const Char_t *pdfFileName="");

  void SetPntrToMaker(StMaker *m1);
  void SetGlobalTitle(const Char_t *globalTitle="");
  TH1** getNewHist();
  Int_t getNewHistSize();
  
// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHistUtil.h,v 2.5 2006/05/18 16:38:03 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StHistUtil, 1)   //needed for all code that will be used in CINT
    };
        
inline void StHistUtil::SetHistsNamesDraw(const Char_t *firstName, const Char_t *lastName)
  { m_FirstHistName = firstName;  m_LastHistName  = lastName; }

inline void StHistUtil::SetZones(Int_t columns, Int_t rows)
  { m_PadColumns =columns; m_PadRows = rows;}

inline void StHistUtil::SetPaperSize(Int_t width, Int_t height)
  { m_PaperWidth = width; m_PaperHeight = height;}

inline void StHistUtil::SetPostScriptFile(const Char_t *psFileName)
  { SetOutFile(psFileName,"ps"); }

inline void StHistUtil::SetPDFFile(const Char_t *pdfFileName)
  { SetOutFile(pdfFileName,"pdf"); }

inline void StHistUtil::SetPntrToMaker(StMaker *m1) 
  {m_PntrToMaker = m1;}

inline void StHistUtil::SetGlobalTitle(const Char_t *globalTitle)
  { m_GlobalTitle = globalTitle;}

inline TH1**  StHistUtil::getNewHist()
  {return newHist;}

inline Int_t StHistUtil::getNewHistSize()
  {return maxHistCopy; }


#endif
