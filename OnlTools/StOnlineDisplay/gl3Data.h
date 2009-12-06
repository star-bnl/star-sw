#ifndef GL3DATA_H
#define GL3DATA_H

#include <qobject.h>
#include <stdio.h>

#include "StEvpReader.h"

// everything from L3
#include "daqFormats.h"
#include "l3CoordinateTransformer.h"
#include "l3EmcCalibration.h"
#include "FtfSl3.h"
#include "gl3Event.h"
#include "gl3EMC.h"

class daqReader;

//#include "St_l3_Coordinate_Transformer.h"

class Gl3Data : public QObject
{
  Q_OBJECT

public:
  Gl3Data(QObject* parent, const char* name);
  ~Gl3Data();
public slots:
  void Init();                                     
  void ReadNextEvent(daqReader *evp, char *mem);    
  void CloseFile();
  void setBField(float field);
  void setEmcTowerAdcCut(float adc);
 public: 
  float getBField() { return bfield; }
  float getEmcTowerAdcCut() { return emcTowerAdcCut;}
  int  GetNTracks() const { return event ? event->getNTracks(): 0; }
  int  GetNHits()  const { return event ? event->getNHits() : 0; }
signals:
  void InitFailed(int);
  void InitSuccess();
  void OpenFileFailed(int,char*);
  void OpenFileSuccess();
  void BuildNewTpcClusterList(Gl3Data *myEventReader);
  void BuildNewTpcTrackList(Gl3Data *myEventReader);
  void BuildNewBEmcTowerList(Gl3Data *myEventReader);
  void BuildNewEEmcTowerList(Gl3Data *myEventReader);
  void NewNumberOfTracks(int);
  void NewNumberOfHits(int);
  void NewL3Decision(bool);

 public:
  // are called by viewer to display hits...
  void resetTpcHits();
  int getNextTpcHit();
  int getTpcHitPos(float *x, float *y, float *z);
  int getTpcHitColor(float *r, float *g, float *b);

  // are called by viewer to display tracks...
  void resetTpcTracks();
  int getNextTpcTrack();
  void resetTpcTrackPos();
  int getNextTpcTrackPos(float *x, float *y, float *z);
  void getTrackColor(float *r, float *g, float *b);
  
  void resetBEmcTower();
  void resetEEmcTower();
  int getNextBEmcTower(float *eta, float *phi, int type = 0);
  int getNextEEmcTower(float *eta1, float *eta2,  float *phi, int type = 0);
  int getBEmcTowerAdc(float *adc);
  int getEEmcTowerAdc(float *adc);
  int getBEmcTowerEnergy(float *energy);
  int getEEmcTowerEnergy(float *energy);
  void getBEmcTowerColor(float *r, float *g, float *b, int type = 0);
  void getEEmcTowerColor(float *r, float *g, float *b, int type = 0);

private:
  char filename[512];
  bool isFileOpen;
  FILE *datafile;

  int init();
  void setFileName(char *name);
  bool openFile();
  bool readEvent();
  bool closeFile();

  // L3 stuff...
  int maxEventBuffer;
  char *evbuff;

  float bfield;

  FtfSl3 *tracker;
  l3CoordinateTransformer *transformer;
  l3EmcCalibration *bemcCalibration;
  l3EmcCalibration *eemcCalibration;
  gl3Event *event;
  bool retrack;
  void retrackEvent();
  int trackUnit(int iSector, L3_SECP *l3secp, char **trackDataPointer,
		char **endTrackBuffer, L3_P *gl3Header);

  int currentHit;
  gl3Hit *getCurrentHit();

  int currentTrack;

 public:
  gl3Track *getFirstTrack();
  gl3Track *getNextTrack();
  gl3Track *getCurrentTrack();

  int currentBEmcTower;
  int currentEEmcTower;
  float emcTowerAdcCut;

  float mTPCTrackStepSize;
  float mCurrentRadius;
  float mTPCTrackStopRadius;
  
  bool accept(gl3Track *track) { if (track->nHits < 10) {return false;} return true; }

  void val2col(float val,float mn,float mx,float *newR,float *newG,float *B);

private slots:

};

#endif
