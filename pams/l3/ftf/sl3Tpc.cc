/*:>-------------------------------------------------------------------
**: FILE:       sl3tTpc.cxx
**: HISTORY:    07/05/99 ppy version with input/output buffer implemented
**:             07/06/99 replace SL3TRACK and SL3HIT with SL3BUFFER
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "sl3Tpc.h"
#include "FtfSl3.h"


void sl3TpcSetParameters ( FtfSl3* tracker, SL3TPCPARA_ST* para ) ;

extern "C" long type_of_call sl3Tpc_(
  TABLE_HEAD_ST     *paraH,       SL3TPCPARA_ST  *para,
  TABLE_HEAD_ST     *hitH,        SL3BUFFER_ST   *hit,        
  TABLE_HEAD_ST     *l3TrackH,    SL3BUFFER_ST   *l3Track )   
{
/*:>--------------------------------------------------------------------
**: ROUTINE:     sl3Tpc
**: DESCRIPTION: Calls sl3 Tracking   
**:
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@rice.edu  
**: ARGUMENTS:
**:       IN:
**:        sl3TpcPara   - Level 3 TPC tracking parameters
**:        hit          - L3 Hit Buffer
**:      OUT:
**:        l3Track      - L3 Track Buffer
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
//
//
//   Decode hit buffer
//
   return STAFCV_OK ;
}
