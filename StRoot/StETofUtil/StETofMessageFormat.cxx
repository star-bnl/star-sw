/***************************************************************************
 *
 * $Id: StETofMessageFormat.cxx,v 1.2 2019/02/19 20:18:27 fseck Exp $
 *
 * Author: Pierre-Alain Loizeau, January 2018
 ***************************************************************************
 *
 * Description: DAQ stream eTOF message format 
 * for 2018 taken from
 * https://lxcbmredmine01.gsi.de/projects/cbmroot/repository/entry/trunk/fles/star2018/unpacker/rocMess_wGet4v2.cxx
 * message format is contained in the namespaces gdbp and get4v2x
 *
 * for 2019 (and later) taken from
 * https://lxcbmredmine01.gsi.de/projects/cbmroot/repository/entry/trunk/fles/mcbm2018/dataformat/gDpbMessv100.cxx
 * message format is contained in the namespace gdbpv100
 *
 ***************************************************************************
 *
 * $Log: StETofMessageFormat.cxx,v $
 * Revision 1.2  2019/02/19 20:18:27  fseck
 * update to include new message format for 2019+
 *
 * Revision 1.1  2018/07/25 14:34:40  jeromel
 * First version, reviewed Raghav+Jerome
 *
 *
 ***************************************************************************/
#include "StETofMessageFormat.h"

// Specific headers

// C++11 headers
#include <cmath>

// std C++ lib headers
#include <stdio.h>
#include <string.h>

//#include <iostream>
#include <iomanip>

//----------------------------------------------------------------------------
// 
// ***** format for year 2019 and later: namespace gdpbv100 *****
//
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
//! strict weak ordering operator, assumes same epoch for both messages
bool gdpbv100::Message::operator<(const gdpbv100::Message& other) const
{
   uint64_t uThisTs  = 0;
   uint64_t uOtherTs = 0;

   uint32_t uThisType  = this->getMessageType();
   uint32_t uOtherType = other.getMessageType();

   // if both GET4 hit messages, use the full timestamp info
   if( MSG_HIT == uThisType && MSG_HIT == uOtherType )
   {
      uThisTs  = this->getGdpbHitFullTs();
      uOtherTs = other.getGdpbHitFullTs();
      return uThisTs < uOtherTs;
   } // both GET4 hit (32b or 24b)

   // First find the timestamp of the current message
   if( MSG_HIT == uThisType )
   {
      uThisTs = ( this->getGdpbHitFullTs() );
   } // if Hit GET4 message (24 or 32b)
      else uThisTs = 0;

   // Then find the timestamp of the current message
   if( MSG_HIT == uOtherType )
   {
      uOtherTs = ( this->getGdpbHitFullTs() );
   } // if Hit GET4 message (24 or 32b)
      else uOtherTs = 0;

   return uThisTs < uOtherTs;
}
//----------------------------------------------------------------------------
//! Returns expanded and adjusted time of message (in ns)
uint64_t gdpbv100::Message::getMsgFullTime(uint64_t epoch) const
{
   return std::round( getMsgFullTimeD( epoch ) );
}
//----------------------------------------------------------------------------
//! Returns expanded and adjusted time of message in double (in ns)
double gdpbv100::Message::getMsgFullTimeD(uint64_t epoch) const
{
   switch( getMessageType() )
   {
      case MSG_HIT:
      {
         if( getGdpbHitIs24b() )
            return ( static_cast<double_t>(FullTimeStamp(epoch, (getGdpbHitCoarse() << 7)))
                    + ( static_cast<double_t>(getGdpbHitFineTs() - 8. ) * gdpbv100::kdFtSize /gdpbv100::kdFtBinsNb) )
                   * (gdpbv100::kdClockCycleSizeNs / gdpbv100::kdFtSize);
            else return ( gdpbv100::kdEpochInNs * static_cast<double_t>( epoch )
                         + static_cast<double_t>( getGdpbHitFullTs() ) * gdpbv100::kdClockCycleSizeNs / gdpbv100::kdFtBinsNb );
      } // case MSG_HIT:
      case MSG_EPOCH:
         return gdpbv100::kdEpochInNs * static_cast<double_t>( getGdpbEpEpochNb() );
      case MSG_SLOWC:
      case MSG_SYST:
      case MSG_STAR_TRI_A:
      case MSG_STAR_TRI_B:
      case MSG_STAR_TRI_C:
      case MSG_STAR_TRI_D:
         return gdpbv100::kdEpochInNs * static_cast<double_t>( epoch );
      default:
         return 0.0;
   } // switch( getMessageType() )

   // If not already dealt with => unknown type
   return 0.0;
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//! Returns the time difference between two expanded time stamps

uint64_t gdpbv100::Message::CalcDistance(uint64_t start, uint64_t stop)
{
   if (start>stop) {
      stop += 0x3FFFFFFFFFFFLLU;
      if (start>stop) {
         printf("Epochs overflow error in CalcDistance\n");
         return 0;
      }
   }

   return stop - start;
}


//----------------------------------------------------------------------------
//! Returns the time difference between two expanded time stamps

double gdpbv100::Message::CalcDistanceD(double start, double stop)
{
   if (start>stop) {
      stop += 0x3FFFFFFFFFFFLLU;
      if (start>stop) {
         printf("Epochs overflow error in CalcDistanceD\n");
         return 0.;
      }
   }

   return stop - start;
}

//----------------------------------------------------------------------------
//! Print message in human readable format to \a cout.
/*!
 * Prints a one line representation of the message in to \a cout.
 * See printData(std::ostream&, unsigned, uint32_t) const for full
 * documentation.
 */

void gdpbv100::Message::printDataCout(unsigned kind, uint32_t epoch) const
{
  printData(msg_print_Cout, kind, epoch);
}

//----------------------------------------------------------------------------
//! Print message in human readable format to the Fairroot logger.
/*!
 * Prints a one line representation of the message in to the Fairroot logger.
 * TODO: Add coloring of possible
 * See printData(std::ostream&, unsigned, uint32_t) const for full
 * documentation.
 */

void gdpbv100::Message::printDataLog(unsigned kind, uint32_t epoch) const
{
  printData(msg_print_FairLog, kind, epoch);
}

//----------------------------------------------------------------------------
//! Print message in binary or human readable format to a stream.
/*!
 * Prints a one line representation of the message in to a stream, selected by \a outType.
 * The stream is \a cout if \a outType is kFALSE and \a FairLogger if \a outType is kTRUE.
 * The parameter \a kind is mask with 4 bits
 * \li gdpbv100::msg_print_Prefix (1) - message type
 * \li gdpbv100::msg_print_Data   (2) - print all message specific data fields
 * \li gdpbv100::msg_print_Hex    (4) - print data as hex dump
 * \li gdpbv100::msg_print_Human  (8) - print in human readable format
 *
 * If bit msg_print_Human in \a kind is not set, raw format
 * output is generated. All data fields are shown in hexadecimal.
 * This is the format of choice when chasing hardware problems at the bit level.
 *
 * If bit msg_print_Human is set, a more human readable output is generated.
 * The timestamp is shown as fully extended and adjusted time as
 * returned by the getMsgFullTime(uint32_t) const method.
 * All data fields are represented in decimal.
 *
 * \param os output stream
 * \param kind mask determing output format
 * \param epoch current epoch number (from last epoch message)
 *
 */

//void gdpbv100::Message::printData(std::ostream& os, unsigned kind, uint32_t epoch) const
void gdpbv100::Message::printData(unsigned outType, unsigned kind, uint32_t epoch, std::ostream& os) const
{
   char buf[256];
   if (kind & msg_print_Hex) {
      const uint8_t* arr = reinterpret_cast<const uint8_t*> ( &data );
      snprintf(buf, sizeof(buf), "BE= %02X:%02X:%02X:%02X:%02X:%02X:%02X:%02X LE= %02X:%02X:%02X:%02X:%02X:%02X:%02X:%02X ",
               arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6], arr[7],
               arr[7], arr[6], arr[5], arr[4], arr[3], arr[2], arr[1], arr[0] );

      if( msg_print_Cout == outType)
         std::cout << buf;
      else if( msg_print_File == outType )
         os << buf;

      snprintf(buf, sizeof(buf), " ");
   }

   if (kind & msg_print_Human) {
      double timeInSec = getMsgFullTimeD(epoch)/1.e9;
      //int fifoFill = 0;

      switch (getMessageType()) {
         case MSG_EPOCH:
            snprintf(buf, sizeof(buf),
                  "Msg:%u ", getMessageType());

            if( msg_print_Cout == outType)
               std::cout << buf;
            else if( msg_print_File == outType )
               os << buf;

            snprintf(buf, sizeof(buf),
                  "EPOCH @%17.11f Get4:%2d Epoche2:%10u 0x%08x Sync:%x Dataloss:%x Epochloss:%x Epochmissmatch:%x",
                        timeInSec, getGdpbGenChipId(), getGdpbEpEpochNb(), getGdpbEpEpochNb(),
                        getGdpbEpSync(), getGdpbEpDataLoss(), getGdpbEpEpochLoss(), getGdpbEpMissmatch());

            if( msg_print_Cout == outType)
               std::cout << buf << std::endl;
            else if( msg_print_File == outType )
               os << buf << std::endl;
            break;
         case MSG_HIT:
            snprintf(buf, sizeof(buf),
                  "Msg:%u ", getMessageType());

            if( msg_print_Cout == outType)
               std::cout << buf;
            else if( msg_print_File == outType )
               os << buf;

            if( getGdpbHitIs24b() )
            {
               snprintf(buf, sizeof(buf),
                     "Get4 24b @%17.11f Get4:%2d Chn:%3d Edge:%1d Ts:%7d",
                     timeInSec, getGdpbGenChipId(), getGdpbHitChanId(), getGdpbHit24Edge(), getGdpbHitFullTs() );
            } // if( getGdpbHitIs24b() )
               else
               {
                  snprintf(buf, sizeof(buf),
                     "Get4 24b @%17.11f Get4:%2d Chn:%3d Dll:%1d Ts:%7d",
                     timeInSec, getGdpbGenChipId(), getGdpbHitChanId(), getGdpbHit32DllLck(), getGdpbHitFullTs() );
               } // else of if( getGdpbHitIs24b() )

            if( msg_print_Cout == outType)
               std::cout << buf << std::endl;
            else if( msg_print_File == outType )
               os << buf << std::endl;
            break;
         default:
            kind = kind & ~msg_print_Human;
            if (kind==0) kind = msg_print_Prefix | msg_print_Data;
      }

      // return, if message was correctly printed in human-readable form
      if (kind & msg_print_Human) return;
   }

   if (kind & msg_print_Prefix) {
      snprintf(buf, sizeof(buf), "Msg:%2u ", getMessageType() );

      if( msg_print_Cout == outType)
         std::cout << buf;
      else if( msg_print_File == outType )
         os << buf;
   }

   if (kind & msg_print_Data) {
      //const uint8_t* arr = reinterpret_cast<const uint8_t*> ( &data );
      switch (getMessageType()) {
         case MSG_HIT:
         {
            if( getGdpbHitIs24b() )
            {
               snprintf(buf, sizeof(buf), "Get4 24 bits, Get4:0x%04x Chn:%1x Edge:%1x Ts:0x%03x",
                     getGdpbGenChipId(), getGdpbHitChanId(), getGdpbHit24Edge(), getGdpbHitFullTs() );
            } // if( getGdpbHitIs24b() )
               else
               {
                  snprintf(buf, sizeof(buf),
                    "Get4 32 bits, Get4:0x%04x Channel %1d Ts:0x%03x Ft:0x%02x Tot:0x%02x  Dll %1d",
                    getGdpbGenChipId(), getGdpbHitChanId(), getGdpbHitCoarse(),
                    getGdpbHitFineTs(), getGdpbHit32Tot(), getGdpbHit32DllLck() );
               } // else of if( getGdpbHitIs24b() )
            break;
         } // case MSG_HIT:
         case MSG_EPOCH:
         {
            snprintf(buf, sizeof(buf), "Get4:0x%04x Link: %1u Epoch:0x%08x Sync:%x Dataloss:%x Epochloss:%x Epochmissmatch:%x",
                     getGdpbGenChipId(), getGdpbEpLinkId(), getGdpbEpEpochNb(), getGdpbEpSync(),
                     getGdpbEpDataLoss(), getGdpbEpEpochLoss(), getGdpbEpMissmatch());
            break;
         } // case MSG_EPOCH:
         case MSG_SLOWC:
         {
            // GET4 slow control message, new "true" ROC support
            snprintf(buf, sizeof(buf),
               "Get4 Slow control, Get4:0x%04x => Chan:%01d Edge:%01d Type:%01x Data:0x%06x",
               getGdpbGenChipId(), 0x0, 0x0, 0x0, getGdpbSlcData() );
            break;
         } // case MSG_SLOWC:
         case MSG_SYST:
         {
            // GET4 system message, new "true" ROC support
            char sysbuf[256];

            switch( getGdpbSysSubType() )
            {
               case SYS_GET4_ERROR:
               {
                 snprintf(sysbuf, sizeof(sysbuf),
                   "Get4:0x%04x Ch:0x%01x Edge:%01x Unused:%06x ErrCode:0x%02x - GET4 V1 Error Event",
                   getGdpbGenChipId(), getGdpbSysErrChanId(), getGdpbSysErrEdge(), getGdpbSysErrUnused(), getGdpbSysErrData());
                  break;
               } //
               case SYS_GDPB_UNKWN:
                  snprintf(sysbuf, sizeof(sysbuf), "Unknown GET4 message, data: 0x%08x", getGdpbSysUnkwData());
                  break;
               case SYS_GET4_SYNC_MISS:
                  snprintf(sysbuf, sizeof(sysbuf), "GET4 SYNC synchronization error");
                  break;
               case SYS_PATTERN:
                  snprintf(sysbuf, sizeof(sysbuf), "Pattern message => Type %d, Index %2d, Pattern 0x%08X",
                             getGdpbSysPattType(), getGdpbSysPattIndex(), getGdpbSysPattPattern() );
                  break;
               default:
                  snprintf(sysbuf, sizeof(sysbuf), "unknown system message type %u", getGdpbSysSubType());
            } // switch( getGdpbSysSubType() )
            snprintf(buf, sizeof(buf), "%s", sysbuf);

            break;
         } // case MSG_SYST:
         case MSG_STAR_TRI_A:
         case MSG_STAR_TRI_B:
         case MSG_STAR_TRI_C:
         case MSG_STAR_TRI_D:
         {
            // STAR trigger token, spread over 4 messages
            switch( getStarTrigMsgIndex() )
            {
               case 0:
               {
                  snprintf(buf, sizeof(buf),
                    "STAR token A, gDPB TS MSB bits: 0x%010llx000000",
                    (unsigned long long) getGdpbTsMsbStarA() );
                  break;
               } // case 1st message:
               case 1:
               {
                  snprintf(buf, sizeof(buf),
                    "STAR token B, gDPB TS LSB bits: 0x0000000000%06llx, STAR TS MSB bits: 0x%04llx000000000000",
                    (unsigned long long) getGdpbTsLsbStarB(), (unsigned long long) getStarTsMsbStarB() );
                  break;
               } // case 2nd message:
               case 2:
               {
                  snprintf(buf, sizeof(buf),
                    "STAR token C,                                     , STAR TS Mid bits: 0x0000%010llx00",
                    (unsigned long long) getStarTsMidStarC() );
                  break;
               } // case 3rd message:
               case 3:
               {
                  snprintf(buf, sizeof(buf),
                    "STAR token D,                                     , STAR TS LSB bits: 0x00000000000000%02llx"
                    ", Token: %03x, DAQ: %1x; TRG:%1x",
                    (unsigned long long) getStarTsLsbStarD(), getStarTokenStarD(), getStarDaqCmdStarD(), getStarTrigCmdStarD() );
                  break;
               } // case 4th message:
            } // switch( getStarTrigMsgIndex() )

            break;
         } // case MSG_STAR_TRI_A || MSG_STAR_TRI_B || MSG_STAR_TRI_C || MSG_STAR_TRI_D:
         default:
           snprintf(buf, sizeof(buf), "Error - unexpected MessageType: %1x, full data %08X::%08X",
                                      getMessageType(), getField(32, 32), getField(0, 32) );
      }
   }

   if( msg_print_Cout == outType)
      std::cout << buf << std::endl;
   else if( msg_print_File == outType )
      os << buf << std::endl;
}
//----------------------------------------------------------------------------
//! strict weak ordering operator, including epoch for both messages
bool gdpbv100::FullMessage::operator<(const FullMessage& other) const
{
   if( other.fulExtendedEpoch == this->fulExtendedEpoch )
      // Same epoch => use Message (base) class ordering operator
      return this->Message::operator<( other );
      else return this->fulExtendedEpoch < other.fulExtendedEpoch;

}
//----------------------------------------------------------------------------
void gdpbv100::FullMessage::PrintMessage( unsigned outType, unsigned kind) const
{
   std::cout << "Full epoch = " << std::setw(9) << fulExtendedEpoch << " ";
   printDataCout( outType, kind );
}
//----------------------------------------------------------------------------








//----------------------------------------------------------------------------
// 
// ***** format for year 2018: namespace get4v2x and gdpb *****
//
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
//! strict weak ordering operator, assumes same epoch for both messages
bool gdpb::Message::operator<(const gdpb::Message& other) const
{
   uint64_t uThisTs  = 0;
   uint64_t uOtherTs = 0;

   uint32_t uThisType  = this->getMessageType();
   uint32_t uOtherType = other.getMessageType();

   // if both GET4 hit messages, use the full timestamp info
   if( (MSG_GET4_32B == uThisType  || MSG_GET4 == uThisType )&&
       (MSG_GET4_32B == uOtherType || MSG_GET4 == uOtherType ) )
   {
      uThisTs  = this->getGdpbHitFullTs();
      uOtherTs = other.getGdpbHitFullTs();
      return uThisTs < uOtherTs;
   } // both GET4 hit (32b or 24b)

   // First find the timestamp of the current message
   if( MSG_GET4_32B == uThisType  || MSG_GET4 == uThisType )
   {
      uThisTs = ( this->getGdpbHitFullTs() ) / 20 + 512;
   } // if Hit GET4 message (24 or 32b)
   else if( MSG_GET4_SLC == uThisType || MSG_GET4_SYS == uThisType )
   {
      uThisTs = 0;
   } // if SLC or SYS GET4 message
      else uThisTs = this->getMsgFullTime( 0 );

   // Then find the timestamp of the current message
   if( MSG_GET4_32B == uOtherType  || MSG_GET4 == uOtherType )
   {
      uOtherTs = ( this->getGdpbHitFullTs() ) / 20 + 512;
   } // if Hit GET4 message (24 or 32b)
   else if( MSG_GET4_SLC == uOtherType || MSG_GET4_SYS == uOtherType )
   {
      uOtherTs = 0;
   } // if SLC or SYS GET4 message
      else uOtherTs = other.getMsgFullTime( 0 );

   return uThisTs < uOtherTs;
}
//----------------------------------------------------------------------------
//! Returns expanded and adjusted time of message (in ns)
//! epoch should correspond to the message type - epoch2 for Get4, epoch for all others
//! With the DPBs, both GET4 and nXYTER systems operate at the same frequency of 160MHz.
//! When converting from GET4 to nXYTER clocks, one should take into account
//! that initial value of nDPB timestamp counter is 0x80 or 512 ns. Therefore such offset
//! should be applied during conversion => Is it really true? nXYTER or nDPB effect?
//! =====> see '///' lines
uint64_t gdpb::Message::getMsgFullTime(uint64_t epoch) const
{
   return std::round( getMsgFullTimeD( epoch ) );
}
//----------------------------------------------------------------------------
//! Returns expanded and adjusted time of message in double (in ns)
//! epoch should correspond to the message type - epoch2 for Get4, epoch for all others
//! Harcoded constants should be implemented better as soon as their value is fixed
double gdpb::Message::getMsgFullTimeD(uint64_t epoch) const
{
   switch( getMessageType() )
   {
      case MSG_EPOCH2:
         return get4v2x::kdEpochInNs * static_cast<double_t>( getEpoch2Number() );
      case MSG_GET4:
         return ( static_cast<double_t>(FullTimeStamp2(epoch, (getGdpbHitCoarse() << 7))) + ( static_cast<double_t>(getGdpbHitFineTs() - 8. ) * 128. /112.) )
                  * (6.25 / 128.); /// TODO: hardcoded -> constant values!!
      case MSG_SYS:
         if( SYSMSG_GET4_EVENT == getGdpbSysSubType() )
            return get4v2x::kdEpochInNs * static_cast<double_t>( epoch ); // Use epoch period of TOF!!
      case MSG_GET4_32B:
         return ( get4v2x::kdEpochInNs * static_cast<double_t>( epoch ) + static_cast<double_t>( getGdpbHitFullTs() ) * get4v2x::kdClockCycleSizeNs / 112. );
      case MSG_GET4_SLC:
      case MSG_GET4_SYS:
         return FullTimeStamp2(epoch, 0) * (6.25 / 128.);
      default:
         return 0.0;
   } // switch( getMessageType() )

   // If not already dealt with => unknown type
   return 0.0;
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//! Returns the time difference between two expanded time stamps

uint64_t gdpb::Message::CalcDistance(uint64_t start, uint64_t stop)
{
   if (start>stop) {
      stop += 0x3FFFFFFFFFFFLLU;
      if (start>stop) {
         printf("Epochs overflow error in CalcDistance\n");
         return 0;
      }
   }

   return stop - start;
}


//----------------------------------------------------------------------------
//! Returns the time difference between two expanded time stamps

double gdpb::Message::CalcDistanceD(double start, double stop)
{
   if (start>stop) {
      stop += 0x3FFFFFFFFFFFLLU;
      if (start>stop) {
         printf("Epochs overflow error in CalcDistanceD\n");
         return 0.;
      }
   }

   return stop - start;
}



//----------------------------------------------------------------------------
//! Print message in human readable format to \a cout.
/*!
 * Prints a one line representation of the message in to \a cout.
 * See printData(std::ostream&, unsigned, uint32_t) const for full
 * documentation.
 */

void gdpb::Message::printDataCout(unsigned kind, uint32_t epoch) const
{
  printData(msg_print_Cout, kind, epoch);
}

//----------------------------------------------------------------------------
//! Print message in human readable format to the Fairroot logger.
/*!
 * Prints a one line representation of the message in to the Fairroot logger.
 * TODO: Add coloring of possible
 * See printData(std::ostream&, unsigned, uint32_t) const for full
 * documentation.
 */

void gdpb::Message::printDataLog(unsigned kind, uint32_t epoch) const
{
  printData(msg_print_FairLog, kind, epoch);
}

//----------------------------------------------------------------------------
//! Print message in binary or human readable format to a stream.
/*!
 * Prints a one line representation of the message in to a stream, selected by \a outType.
 * The stream is \a cout if \a outType is kFALSE and \a FairLogger if \a outType is kTRUE.
 * The parameter \a kind is mask with 4 bits
 * \li gdpb::msg_print_Prefix (1) - ROC number and message type
 * \li gdpb::msg_print_Data   (2) - print all message specific data fields
 * \li gdpb::msg_print_Hex    (4) - print data as hex dump
 * \li gdpb::msg_print_Human  (8) - print in human readable format
 *
 * If bit msg_print_Human in \a kind is not set, raw format
 * output is generated. All data fields are shown in hexadecimal.
 * This is the format of choice when chasing hardware problems at the bit level.
 *
 * If bit msg_print_Human is set, a more human readable output is generated.
 * The timestamp is shown as fully extended and adjusted time as
 * returned by the getMsgFullTime(uint32_t) const method.
 * All data fields are represented in decimal.
 *
 * \param os output stream
 * \param kind mask determing output format
 * \param epoch current epoch number (from last epoch message)
 *
 * Typical message output in human format looks like
\verbatim
Msg:7 Roc:1 SysType: 1 Nx:0 Data:        0 : DAQ started
Msg:7 Roc:1 SysType: 6 Nx:0 Data:        0 : FIFO reset
Msg:2 Roc:1 EPO @    0.536870912 Epo:     32768 0x00008000 Miss:   0
Msg:0 Roc:0 NOP (raw 80:40:82:0F:00:00)
Msg:2 Roc:1 EPO @    0.646627328 Epo:     39467 0x00009a2b Miss:   0
Msg:1 Roc:1 HIT @    0.646614333 Nx:2 Chn: 12 Ts: 3389-e( 8) Adc:2726 Pu:0 Of:0
Msg:1 Roc:1 HIT @    0.646630717 Nx:2 Chn: 13 Ts: 3389  ( 0) Adc:2745 Pu:0 Of:0
Msg:2 Roc:1 EPO @    0.805306368 Epo:     49152 0x0000c000 Miss:   0
Msg:3 Roc:1 SYN @    0.805306408 Chn:2 Ts:   40   Data:   49152 0x00c000 Flag:0
Msg:7 Roc:1 SysType: 2 Nx:0 Data:        0 : DAQ finished
\endverbatim
 *
 * Typical message output in binary format looks like
\verbatim
Msg:7 Roc:1 SysType: 1 Nx:0 Data:        0 : DAQ started
Msg:7 Roc:1 SysType: 6 Nx:0 Data:        0 : FIFO reset
Msg:2 Roc:1 Epoch:00008000 Missed:00
Msg:1 Roc:1 Nx:2 Chn:0d Ts:3ec9 Last:1 Msb:7 Adc:a22 Pup:0 Oflw:0
Msg:1 Roc:1 Nx:2 Chn:0e Ts:3ec9 Last:0 Msb:7 Adc:a18 Pup:0 Oflw:0
Msg:0 Roc:0 NOP (raw 80:40:82:0F:00:00)
Msg:2 Roc:1 Epoch:00010000 Missed:00
Msg:3 Roc:1 SyncChn:2 EpochLSB:0 Ts:0028 Data:010000 Flag:0
Msg:7 Roc:1 SysType: 2 Nx:0 Data:        0 : DAQ finished
\endverbatim
 */

//void gdpb::Message::printData(std::ostream& os, unsigned kind, uint32_t epoch) const
void gdpb::Message::printData(unsigned outType, unsigned kind, uint32_t epoch, std::ostream& os) const
{
   char buf[256];
   if (kind & msg_print_Hex) {
      const uint8_t* arr = reinterpret_cast<const uint8_t*> ( &data );
      snprintf(buf, sizeof(buf), "BE= %02X:%02X:%02X:%02X:%02X:%02X:%02X:%02X LE= %02X:%02X:%02X:%02X:%02X:%02X:%02X:%02X ",
               arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6], arr[7],
               arr[7], arr[6], arr[5], arr[4], arr[3], arr[2], arr[1], arr[0] );
//      os << buf;
      if( msg_print_Cout == outType)
         std::cout << buf;
      else if( msg_print_File == outType )
         os << buf;

      snprintf(buf, sizeof(buf), " ");
   }

   if (kind & msg_print_Human) {
      double timeInSec = getMsgFullTimeD(epoch)/1.e9;
      //int fifoFill = 0;

      switch (getMessageType()) {
         case MSG_EPOCH2:
            snprintf(buf, sizeof(buf),
                  "Msg:%u Roc:%04x ", getMessageType(), getRocNumber());
//            os << buf;
            if( msg_print_Cout == outType)
               std::cout << buf;
            else if( msg_print_File == outType )
               os << buf;

            snprintf(buf, sizeof(buf),
                  "EPO2 @%17.11f Get4:%2d Epoche2:%10u 0x%08x StampTime:%2d Sync:%x Dataloss:%x Epochloss:%x Epochmissmatch:%x",
                        timeInSec, getEpoch2ChipNumber(), getEpoch2Number(), getEpoch2Number(),
                        getEpoch2StampTime(), getEpoch2Sync(), getEpoch2DataLost(), getEpoch2EpochLost(), getEpoch2EpochMissmatch());
//            os << buf << std::endl;
            if( msg_print_Cout == outType)
               std::cout << buf << std::endl;
            else if( msg_print_File == outType )
               os << buf << std::endl;
            break;
         case MSG_GET4:
            snprintf(buf, sizeof(buf),
                  "Msg:%u Roc:%04x ", getMessageType(), getRocNumber());
//            os << buf;
            if( msg_print_Cout == outType)
               std::cout << buf;
            else if( msg_print_File == outType )
               os << buf;

            snprintf(buf, sizeof(buf),
                  "Get4 @%17.11f Get4:%2d Chn:%3d Edge:%1d Ts:%7d CRC8:%3d",
                  timeInSec, getGet4Number(), getGet4ChNum(), getGet4Edge(), getGet4Ts(), getGet4CRC() );
//            os << buf << std::endl;
            if( msg_print_Cout == outType)
               std::cout << buf << std::endl;
            else if( msg_print_File == outType )
               os << buf << std::endl;
            break;
         default:
            kind = kind & ~msg_print_Human;
            if (kind==0) kind = msg_print_Prefix | msg_print_Data;
      }

      // return, if message was correctly printed in human-readable form
      if (kind & msg_print_Human) return;
   }

   if (kind & msg_print_Prefix) {
      snprintf(buf, sizeof(buf), "Msg:%2u Roc:%04x ", getMessageType(), getRocNumber());
//      os << buf;
      if( msg_print_Cout == outType)
         std::cout << buf;
      else if( msg_print_File == outType )
         os << buf;
   }

   if (kind & msg_print_Data) {
      const uint8_t* arr = reinterpret_cast<const uint8_t*> ( &data );
      switch (getMessageType()) {
        case MSG_NOP:
           snprintf(buf, sizeof(buf), "NOP (raw %02X:%02X:%02X:%02X:%02X:%02X)",
                    arr[0], arr[1], arr[2], arr[3], arr[4], arr[5]);
            break;
         case MSG_EPOCH2:
            snprintf(buf, sizeof(buf), "Get4:0x%02x Link: %1u Epoche2:0x%08x StampTime:0x%x Sync:%x Dataloss:%x Epochloss:%x Epochmissmatch:%x",
                     getGdpbGenChipId(), getGdpbEpLinkId(), getGdpbEpEpochNb(), getGdpbEpStampTs(), getGdpbEpSync(),
                     getGdpbEpDataLoss(), getGdpbEpEpochLoss(), getGdpbEpMissmatch());
            break;
         case MSG_GET4:
            snprintf(buf, sizeof(buf), "Get4:0x%02x Chn:%1x Edge:%1x Ts:0x%05x CRC8:0x%02x",
                  getGdpbGenChipId(), getGdpbHitChanId(), getGdpbHit24Edge(), getGdpbHitFullTs(), getGdpbHitCrc() );
            break;
         case MSG_SYS: {
            char sysbuf[256];

            switch (getSysMesType()) {
              case SYSMSG_DAQ_START:
                 snprintf(sysbuf, sizeof(sysbuf), "DAQ started");
                 break;
              case SYSMSG_DAQ_FINISH:
                 snprintf(sysbuf, sizeof(sysbuf), "DAQ finished");
                 break;
              case SYSMSG_SYNC_PARITY:
                 snprintf(sysbuf, sizeof(sysbuf), "SYNC parity error ");
                 break;
              case SYSMSG_DAQ_RESUME:
                 snprintf(sysbuf, sizeof(sysbuf), "DAQ resume after high/low water");
                 break;
              case SYSMSG_FIFO_RESET:
                 snprintf(sysbuf, sizeof(sysbuf), "FIFO reset");
                 break;
              case SYSMSG_USER: {
                 const char* subtyp = "";
                 if (getSysMesData()==SYSMSG_USER_CALIBR_ON) subtyp = "Calibration ON"; else
                 if (getSysMesData()==SYSMSG_USER_CALIBR_OFF) subtyp = "Calibration OFF"; else
                 if (getSysMesData()==SYSMSG_USER_RECONFIGURE) subtyp = "Reconfigure";
                 snprintf(sysbuf, sizeof(sysbuf), "User message 0x%08x %s", getSysMesData(), subtyp);
                 break;
              }
              case SYSMSG_PACKETLOST:
                 snprintf(sysbuf, sizeof(sysbuf), "Packet lost");
                 break;
              case SYSMSG_GET4_EVENT:
              {
                 break;
              } //
              case SYSMSG_CLOSYSYNC_ERROR:
                 snprintf(sysbuf, sizeof(sysbuf), "Closy synchronization error");
                 break;
              case SYSMSG_TS156_SYNC:
                 snprintf(sysbuf, sizeof(sysbuf), "156.25MHz timestamp reset");
                 break;
              case SYSMSG_GET4V1_32BIT_0:
              case SYSMSG_GET4V1_32BIT_1:
              case SYSMSG_GET4V1_32BIT_2:
              case SYSMSG_GET4V1_32BIT_3:
              case SYSMSG_GET4V1_32BIT_4:
              case SYSMSG_GET4V1_32BIT_5:
              case SYSMSG_GET4V1_32BIT_6:
              case SYSMSG_GET4V1_32BIT_7:
              case SYSMSG_GET4V1_32BIT_8:
              case SYSMSG_GET4V1_32BIT_9:
              case SYSMSG_GET4V1_32BIT_10:
              case SYSMSG_GET4V1_32BIT_11:
              case SYSMSG_GET4V1_32BIT_12:
              case SYSMSG_GET4V1_32BIT_13:
              case SYSMSG_GET4V1_32BIT_14:
              case SYSMSG_GET4V1_32BIT_15:
              {
                 break;
              } // case SYSMSG_GET4V1_32BIT + channel!
              default:
                 snprintf(sysbuf, sizeof(sysbuf), "unknown system message type ");
            } // switch (getSysMesType())

            snprintf(buf, sizeof(buf), "SysType:%2x Data:%8x : %s", getSysMesType(), getSysMesData(), sysbuf);

            break;
         } // case MSG_SYS:
         case MSG_GET4_SLC:
         {
            // GET4 slow control message, new "true" ROC support
            snprintf(buf, sizeof(buf),
               "Get4 Slow control, chip %02d => Chan:%01d Edge:%01d Type:%01x Data:0x%06x CRC:0x%02x",
               getGdpbGenChipId(), 0x0, 0x0, 0x0, getGdpbSlcData(), getGdpbSlcCrc() );
            break;
         } // case MSG_GET4_SLC:
         case MSG_GET4_32B:
         {
            // 32b GET4 data event, new "true" ROC support
            snprintf(buf, sizeof(buf),
              "Get4 32 bits, Chip:0x%02x Dll %1d Channel %1d Ts:0x%03x Ft:0x%02x Tot:0x%02x",
              getGdpbGenChipId(), getGdpbHit32DllLck(), getGdpbHitChanId(),
              getGdpbHitCoarse(), getGdpbHitFineTs(), getGdpbHit32Tot() );

            break;
         } // case MSG_GET4_32B:
         case MSG_GET4_SYS:
         {
            // GET4 system message, new "true" ROC support
            char sysbuf[256];

            switch( getGdpbSysSubType() )
            {
               case SYSMSG_GET4_EVENT:
               {
                 snprintf(sysbuf, sizeof(sysbuf),
                   "Get4:0x%02x Ch:0x%01x Edge:%01x Unused:%06x ErrCode:0x%02x - GET4 V1 Error Event",
                   getGdpbGenChipId(), getGdpbSysErrChanId(), getGdpbSysErrEdge(), getGdpbSysErrUnused(), getGdpbSysErrData());
                  break;
               } //
               case SYSMSG_CLOSYSYNC_ERROR:
                  snprintf(sysbuf, sizeof(sysbuf), "Closy synchronization error");
                  break;
               case SYSMSG_TS156_SYNC:
                  snprintf(sysbuf, sizeof(sysbuf), "156.25MHz timestamp reset");
                  break;
               case SYSMSG_GDPB_UNKWN:
                  snprintf(sysbuf, sizeof(sysbuf), "Unknown GET4 message, data: 0x%08x", getGdpbSysUnkwData());
                  break;
               default:
                  snprintf(sysbuf, sizeof(sysbuf), "unknown system message type %u", getGdpbSysSubType());
            } // switch( getGdpbSysSubType() )
            snprintf(buf, sizeof(buf), "%s", sysbuf);

            break;
         } // case MSG_GET4_SYS:
         case MSG_STAR_TRI:
         {
            // STAR trigger token, spread over 4 messages
            switch( getStarTrigMsgIndex() )
            {
               case 0:
               {
                  snprintf(buf, sizeof(buf),
                    "STAR token A, gDPB TS MSB bits: 0x%010llx000000",
                    (unsigned long long) getGdpbTsMsbStarA() );
                  break;
               } // case 1st message:
               case 1:
               {
                  snprintf(buf, sizeof(buf),
                    "STAR token B, gDPB TS LSB bits: 0x0000000000%06llx, STAR TS MSB bits: 0x%04llx000000000000",
                    (unsigned long long) getGdpbTsLsbStarB(), (unsigned long long) getStarTsMsbStarB() );
                  break;
               } // case 2nd message:
               case 2:
               {
                  snprintf(buf, sizeof(buf),
                    "STAR token C,                                     , STAR TS Mid bits: 0x0000%010llx00",
                    (unsigned long long) getStarTsMidStarC() );
                  break;
               } // case 3rd message:
               case 3:
               {
                  snprintf(buf, sizeof(buf),
                    "STAR token D,                                     , STAR TS LSB bits: 0x00000000000000%02llx"
                    ", Token: %03x, DAQ: %1x; TRG:%1x",
                    (unsigned long long) getStarTsLsbStarD(), getStarTokenStarD(), getStarDaqCmdStarD(), getStarTrigCmdStarD() );
                  break;
               } // case 4th message:
            } // switch( getStarTrigMsgIndex() )

            break;
         } // case MSG_STAR_TRI:
         default:
           snprintf(buf, sizeof(buf), "Error - unexpected MessageType: %1x, full data %08X::%08X",
                                      getMessageType(), getField(32, 32), getField(0, 32) );
      }
   }

//   os << buf << std::endl;
   if( msg_print_Cout == outType)
      std::cout << buf << std::endl;
   else if( msg_print_File == outType )
      os << buf << std::endl;
//   else LOG(INFO) << sLogBuff << buf << FairLogger::endl;
}

uint32_t gdpb::Message::RawSize(int fmt)
{
   switch (fmt) {
      case formatNormal: return 8;
      default:
         std::cerr << "gdpb::Message::RawSize => "
                   << " Deprecated format, nothing done!!"
                   << std::endl;
         return 0;
   }
   return 8;
}

bool gdpb::Message::assign(void* src, int fmt)
{
   switch (fmt) {
      case formatNormal:
         memcpy(&data, src, 8);
         return true;
      default:
         std::cerr << "gdpb::Message::assign => "
                   << " Deprecated format, nothing done!!"
                   << std::endl;
         return false;
   }

   return false;
}

bool gdpb::Message::copyto(void* tgt, int fmt)
{
   switch (fmt) {
      case formatNormal:
         memcpy(tgt, &data, 8);
         return true;
      default:
         std::cerr << "gdpb::Message::assign => "
                   << " Deprecated format, nothing done!!"
                   << std::endl;
         return false;
   }

   return false;

}

//----------------------------------------------------------------------------
//! strict weak ordering operator, including epoch for both messages
bool gdpb::FullMessage::operator<(const FullMessage& other) const
{
   if( other.fulExtendedEpoch == this->fulExtendedEpoch )
      // Same epoch => use Message (base) class ordering operator
      return this->Message::operator<( other );
      else return this->fulExtendedEpoch < other.fulExtendedEpoch;

}
//----------------------------------------------------------------------------
void gdpb::FullMessage::PrintMessage( unsigned outType, unsigned kind) const
{
   std::cout << "Full epoch = " << std::setw(9) << fulExtendedEpoch << " ";
   printDataCout( outType, kind );
}