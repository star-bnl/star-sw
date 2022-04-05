#ifndef TRG_CONFNUM
#define TRG_CONFNUM

/******
*
*     Header defining crate configuration numbers
*
*     J.M. Nelson        November 2008
*
*     JMN 04Oct16  All ConfNum are defined in trgConfig.h
*                  A separate file is not needed as duplication is bad
*     JMN 29Aug17  In order to keep RTS/trg/include in step with $DEV
*                  re-place ConfNum from trgConfig.h to this file and
*                  #include trgConfnum.h in trgConfig.h
***********************************************************************/


#define  MAX_CONF_NUM      20

#define  RCC_CONF_NUM       0
#define   L1_CONF_NUM       1
#define  BC1_CONF_NUM       2
#define  MXQ_CONF_NUM       3
#define  MIX_CONF_NUM       4
#define  BCW_CONF_NUM       5
#define  BCE_CONF_NUM       6
#define  EQ3_CONF_NUM       7
#define  BBC_CONF_NUM       8
#define  BBQ_CONF_NUM       9
#define  FMS_CONF_NUM      10
#define  QT1_CONF_NUM      11
#define  QT2_CONF_NUM      12
#define  QT3_CONF_NUM      13
#define  QT4_CONF_NUM      14
#define  EQ1_CONF_NUM      15
#define  EQ2_CONF_NUM      16
#define  INF_CONF_NUM      20

#endif
