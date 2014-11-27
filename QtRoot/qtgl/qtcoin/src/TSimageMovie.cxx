// @(#)root/g3d:$Name:  $:$Id: TSimageMovie.cxx,v 1.7 2013/08/30 16:00:16 perev Exp $
// Author: Valery Fine      24/11/06

/****************************************************************************
** $Id: TSimageMovie.cxx,v 1.7 2013/08/30 16:00:16 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TSimageMovie.h"
#include "TEnv.h"
#include "TSystem.h"
#include "TString.h"
#include "qfileinfo.h"
#include <cmath>

bool TSimageMovie::fgPluginLoaded=false;
//______________________________________________________________________________
static void
error_cb(void * /*userdata*/, const char *text)
{
  (void)fprintf(stderr, "Error: %s\n", text);
  (void)fflush(stderr);
}


//______________________________________________________________________________
static void
warning_cb(void * /*userdata*/, const char *text)
{
  (void)fprintf(stderr, "Warning: %s\n", text);
  (void)fflush(stderr);
}


//______________________________________________________________________________
static int
progress_cb(void * /*userdata*/ , float sub, int current_frame, int num_frames)
{
  char buffer[256];

  int logframes = (int)log10(float(num_frames)) + 1;
  (void)sprintf(buffer, "\rwriting frame: %%%dd / %%%dd  -- %%03.1f%%%%  ",
                logframes, logframes);

  (void)fprintf(stdout, buffer, current_frame + 1, num_frames, sub * 100.0);
  (void)fflush(stdout);
  return 1;
}

//______________________________________________________________________________
TSimageMovie::TSimageMovie(int width,int height, int nrFrames,int clocktime,int constraintslevel)
: fWidth(width), fHeight(height), fClocktime(clocktime), fConstraintslevel(constraintslevel)
, fNumberOfFrames(nrFrames), fParams(0), fMovie(0), fImage(0)
{
   if (!fgPluginLoaded) { fgPluginLoaded = !gSystem->Load("libsimage"); }
   DefineParamFile();
   Open(width,height,nrFrames,clocktime,constraintslevel);
   SetParams();
}
//______________________________________________________________________________
void TSimageMovie::Open(int width,int height, int nrFrames, int clocktime,int constraintslevel)
{
  Close();
  if (fgPluginLoaded) {
    fWidth           = width;
    fHeight          = height; 
    fClocktime       = clocktime;
    fConstraintslevel= constraintslevel; 
    fNumberOfFrames  = nrFrames;
  }
}

//______________________________________________________________________________
TSimageMovie::~TSimageMovie()
{
  if (fParams) { s_params_destroy(fParams); fParams = 0; }
  Close();
}

//______________________________________________________________________________
void TSimageMovie::Close()
{
   if (fImage ) { s_image_destroy (fImage) ; fImage  = 0; }
   if (fMovie ) { s_movie_close(fMovie);  
                  s_movie_destroy(fMovie)  ; fMovie  = 0; }
}

//______________________________________________________________________________
s_params *TSimageMovie::CreateParams()
{
   return s_params_create();
}
//______________________________________________________________________________
s_image  *TSimageMovie::CreateImage(int w, int h, unsigned char * prealloc, int components)
{
   return s_image_create(w,h,components,prealloc);
}

//______________________________________________________________________________
s_movie *TSimageMovie::CreateMovie(const QString &movieFileName, s_params *params)
{
   s_movie *m = s_movie_create((const char*)movieFileName, params);
   printf("TSimageMovie::CreateMovie   %s %p %p\n", (const char*)movieFileName,m,params);
   if (m == NULL) {
      error_cb(NULL, "could not create movie file");
      if (simage_get_last_error()) { error_cb(NULL, simage_get_last_error()); }
   }
   return m;
}

//______________________________________________________________________________
void TSimageMovie::SetImage(s_image *image, int w, int h, unsigned char *buffer,
                         int components,int copydata)
{
   s_image_set(image, w, h, components, buffer, copydata);
}

//______________________________________________________________________________
int TSimageMovie::PutImage(s_movie *movie, s_image *image, s_params * params)
{
   return s_movie_put_image(movie, image,params);
}
//______________________________________________________________________________
int TSimageMovie::PutImage()
{
  return PutImage(fMovie,fImage);
}
//______________________________________________________________________________
void TSimageMovie::AddFrame(unsigned char *frameBuffer)
{
   if (frameBuffer && fMovie)
   {
      SetImage(frameBuffer);
      PutImage();
   }
}

//______________________________________________________________________________
void TSimageMovie::SetImage(unsigned char *buffer)
{
  if (fMovie) {
     if (!fImage) fImage = CreateImage(fWidth,fHeight,buffer);
     else SetImage(fImage, fWidth,fHeight,buffer);
  }
}
//______________________________________________________________________________
void TSimageMovie::SetMovie(const QString &movieFileName)
{ 
   if (!movieFileName.isEmpty())  fMovieFile = movieFileName;
   if (!fMovie && fParams) fMovie = CreateMovie(fMovieFile,fParams);
   printf(" TSimageMovie::SetMovie() %s movie %p params = %p\n",(const char *)movieFileName, fMovie,fParams);
}
//______________________________________________________________________________
void TSimageMovie::SetParams()
{
  if (!fParams) {
     fParams = CreateParams();
     s_params_set(fParams, 
               "mime-type"     , S_STRING_PARAM_TYPE , "video/mpeg",
               "width"         , S_INTEGER_PARAM_TYPE, fWidth,
               "height"        , S_INTEGER_PARAM_TYPE, fHeight,

               "num frames"    , S_INTEGER_PARAM_TYPE, fNumberOfFrames,

               "error callback", S_FUNCTION_PARAM_TYPE, error_cb,
               "warning callback", S_FUNCTION_PARAM_TYPE, warning_cb,
               "progress callback", S_FUNCTION_PARAM_TYPE, progress_cb,
               /* use to specify userdata for all callbacks */
               "callback userdata", S_POINTER_PARAM_TYPE, NULL,

               /* use to encode as mpeg1 instead of mpeg2 */
               "mpeg1"         , S_BOOL_PARAM_TYPE   , 0,

               /* use to specify a parameter file */
               "parameter file", S_STRING_PARAM_TYPE, (const char *)ParamFileName(),

               /* use to specify constraints coded parameter constraints for mpeg2 files, 
                  such as bitrate, sample rate, and maximum allowed motion vector range.

                  Value Meaning         Typical use
                  ----  --------------- -----------------------------------------------
                  4     High Level      HDTV production rates: e.g. 1920 x 1080 x 30 Hz
                  6     High 1440 Level HDTV consumer rates: e.g. 1440 x 960 x 30 Hz
                  8     Main Level      CCIR 601 rates: e.g. 720 x 480 x 30 Hz
                  10    Low Level       SIF video rate: e.g. 352 x 240 x 30 Hz
               */
               "level", S_INTEGER_PARAM_TYPE, fConstraintslevel,

               /* NULL means no more params */
               NULL);              
   }
}
//______________________________________________________________________________
void TSimageMovie::SetParamFile(const QString &paramFile) {
    fParamFile = paramFile;
}
//______________________________________________________________________________
void TSimageMovie::DefineParamFile() 
{
   TString paramFile = gEnv->GetValue("Movie.Parameters","$STAR/QtRoot/qtgl/qtcoin/data/ntsc_coin.par");
   gSystem->ExpandPathName(paramFile);
   fParamFile = (const char *)paramFile;
   if (!QFileInfo(fParamFile).isReadable() )  fParamFile = "";
}

