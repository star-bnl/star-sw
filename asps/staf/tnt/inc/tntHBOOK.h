#ifdef __cplusplus
#define CC_P "C"
#endif

/*    INTEGER*4 FUNCTION initPAWC()                            */
extern CC_P long initpawc_();

/*    CHARACTER*80 FUNCTION TNT_NT_TITLE(HID)                   */
extern CC_P char *tnt_nt_title_(long hid);

/*    INTEGER*4 FUNCTION TNT_NT_COLUMN_COUNT(HID)               */
extern CC_P long tnt_nt_column_count_(long hid);

/*    INTEGER*4 FUNCTION TNT_NT_ENTRY_COUNT(HID)                */
extern CC_P long tnt_nt_entry_count_(long hid);

/*    CHARACTER*8 FUNCTION TNT_NT_TAG(HID,NCOL)                 */
extern CC_P char *tnt_nt_tag_(long hid,long ncol);

/*    INTEGER*4 FUNCTION TNT_START_PAW()                       */
extern CC_P long tnt_start_paw_();

/*    INTEGER*4 FUNCTION TNT_START_SHARE()                     */
extern CC_P long tnt_start_share_();

