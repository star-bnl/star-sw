/*:>-------------------------------------------------------------------
**: FILE:       gl3.cxx
**: HISTORY:    7/06/99  ppy shell created
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "gl3.h"

extern "C" long type_of_call gl3_(
  TABLE_HEAD_ST     *paraH,       GL3PARA_ST    *para,
  TABLE_HEAD_ST     *l3TrackH,    SL3BUFFER_ST  *l3Track,  
  TABLE_HEAD_ST     *tpTrackH,    TPT_TRACK_ST  *tpTrack )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    gl3
**: DESCRIPTION: Collects tracks from SL3 processors and makes
**:              trigger decision
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@rice.edu  
**: ARGUMENTS:
**:       IN:
**:        para          - gl3 control parameters
**:        l3Track       - Track buffer from sl3  
**:      OUT:
**:        tpTrack       - tracks in offline format
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
//
   return STAFCV_OK ;
}
