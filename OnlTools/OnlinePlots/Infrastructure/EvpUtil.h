#ifndef EvpUtil_h
#define EvpUtil_h



#ifndef __PROJECTPATH__
#define __PROJECTPATH__  PROJECTDIR
#endif

#define MAX_TABS 18
#define MAX_SUBTABS 18
#define MAX_PADS 64
#define MAX_NAME 32
#define MAX_GARBAGE 256

#include "TFile.h"
#include "TMapFile.h"
class TPad;
class TH1;
class TH2;

#include "TString.h"
#include "TObjArray.h"
#include "TObjString.h"

#include "GenericFile.h"
#include "GroupCollection.h"

class GroupCollection;

typedef GenericFile FileType;

class EvpUtil {
 public:

  static const char* GetInputPath();
  static const char* GetOutputPath();
  static const char* GetProjectPath();
  static int mCanvasWidth;
  static int mCanvasHeight;
  static int mDebugLevel;
  static int mSharedMemorySize;
  static char* mMapFilePath;
  static const char* mInputPath;
  static const char* mOutputPath;
  static const char* mProjectPath;
  static const char* mReference;
  static char* mCanvasDescriptionFile;
  static char* mListOfHistogamsFile;


  static char* cat(const char*, const char*);

  static int mNumberOfTabs;
  static int mNumberOfSubTabs[MAX_TABS];
  static int nx[MAX_TABS][MAX_SUBTABS];
  static int ny[MAX_TABS][MAX_SUBTABS];
  static int nHist[MAX_TABS][MAX_SUBTABS];  
  static unsigned int canvasTriggerBits[MAX_TABS][MAX_SUBTABS];  
  static unsigned int canvasDetectorBits[MAX_TABS][MAX_SUBTABS];  
  static TString hNames[MAX_TABS][MAX_SUBTABS][MAX_PADS]; 
  static TString hGroupName[MAX_TABS][MAX_SUBTABS]; 
  static TH1* hHist[MAX_TABS][MAX_SUBTABS][MAX_PADS]; 

  static int GetLogY(const int i,const int j);
  static int GetLogZ(const int i,const int j);
  static bool DisplayOneCanvas(TMapFile* mfile , TPad* gcc, const int i, const int j, bool doClear=false);    // return true if canvas should be printed
  static bool DisplayOneCanvas(GenericFile* gFile , TPad* gcc, const int i, const int j, bool doClear=false, GroupCollection* hGroups=NULL); // return true if canvas should be printed
  static bool HasEntries(GenericFile* gFile , int i, int j);
  static int ParseString (const TString &tChain, TObjArray &Opt);
  
  static int ReadCanvasDefinitions(char* file = mCanvasDescriptionFile);
  static int ReadCanvasDefinition(TString line);
  static int ReadCanvasCondition(TString line,unsigned int& trig, unsigned int& det);
  static void CheckCanvasDefinitions(TMapFile* mfile) ;

  // map file sanity
  static int EvpUtil::GetSizeOfMappedObjects(TMapFile *);

  // retrieving histograms
  static TObject* EvpUtil::GetObjectFromMapFile(TMapFile *,const char* name, TObject* o=0);
  static TH1*     EvpUtil::GetHistoFromMapFile(TMapFile *,const char* name, TH1* h=0);
  static TObject* EvpUtil::GetObjectFromGenericFile(GenericFile *,const char* name, TObject* o=0);
  static TH1*     EvpUtil::GetHistoFromGenericFile(GenericFile*,const char* name, TH1* h=0);
  static TH1* hUnknown; // default histogram if histo is not found by name in the map file
  // converting file types
  static int Map2Root(TMapFile* mFile, const char* root);
  static int Map2Root(const char* map, const char* root);
  static int Root2Map(const char* root, const char* map);

  static unsigned int evpgroupmask( char* );
  static unsigned int detmask( char* );

  static void EvpUtil::Draw(TH1*h, const char* options="");

};




#endif





/***************************************************************************
 *
 * $Id: EvpUtil.h,v 1.7 2009/04/30 01:23:33 dkettler Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: EvpUtil.h,v $
 * Revision 1.7  2009/04/30 01:23:33  dkettler
 * Histogram group printing improvements
 *
 * Revision 1.6  2009/04/06 18:49:21  dkettler
 * Histogram groups can be added to the main tabs by editing CanvasDescriptions.txt
 *
 * Revision 1.5  2009/03/05 00:03:16  dkettler
 * EMC Updates
 *
 * Revision 1.4  2009/02/27 22:30:17  dkettler
 * TOF Updates
 *
 * Revision 1.3  2009/02/04 01:25:51  dkettler
 * Remove ONLINEPLOTSDIR reference
 *
 * Revision 1.2  2009/01/29 19:04:35  dkettler
 * BEMC changes
 *
 * Revision 1.1  2009/01/23 16:10:55  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.4  2009/01/13 00:44:02  fine
 * Add rootrc parameters
 *
 * Revision 1.3  2009/01/13 00:39:07  fine
 * Add rootrc parameters
 *
 * Revision 1.2  2007/03/21 17:11:45  laue
 * Moved individual HistogramGroups to their own folder
 * Modified Makefile and server to allow profiling with gprof (i.e. must not exot with exit(...) function)
 *
 * Revision 1.1  2007/02/27 15:23:37  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/

