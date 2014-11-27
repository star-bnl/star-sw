#ifndef STAR_TSimageMovie
#define STAR_TSimageMovie
// @(#)root/g3d:$Name:  $:$Id: TSimageMovie.h,v 1.6 2013/08/30 16:00:15 perev Exp $
// Author: Valery Fine      24/11/06

/****************************************************************************
** $Id: TSimageMovie.h,v 1.6 2013/08/30 16:00:15 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include <simage.h>
#include "qstring.h"

//struct s_params;
//struct s_movie;
//struct s_image;

class  TSimageMovie {
  private:
     int fWidth;            // pixels
     int fHeight;           // pixels
     int fClocktime;        // length of movie in seconds
     int fConstraintslevel;  
     int fNumberOfFrames;
     QString fParamFile;
     QString fMovieFile;
     s_params *fParams;   // simage object
     s_movie  *fMovie;
     s_image  *fImage;
     static bool fgPluginLoaded;

protected:
    static s_image  *CreateImage(int w, int h, unsigned char *prealloc=0, int components=3);
    static s_movie  *CreateMovie(const QString &movieFileName, s_params *params);
    static s_params *CreateParams();
    virtual void DefineParamFile();
    static void SetImage(s_image *image, int w, int h, unsigned char *buffer,
                         int components=3,int copydata=0);
    void SetParamFile(const QString &paramFile);
    static int PutImage(s_movie  *movie, s_image  *image,  s_params *params=0);

    int  PutImage();
    void SetImage(unsigned char *buffer);
    void SetParams();
public:
   TSimageMovie(int width=640,int height=480, int nrFrames=1000, int clocktime=1,int constraintslevel=8);
   virtual ~TSimageMovie();
   void Open(int width=640,int height=480, int nrFrames=1000, int clocktime=1,int constraintslevel=8);
   const QString &ParamFileName() const { return fParamFile;}
   void AddFrame(unsigned char *frameBuffer);
   int Width()  const { return fWidth; }
   int Height() const { return fHeight;}
   void Close();
   void SetMovie(const QString &movieFileName="");
};


#endif
