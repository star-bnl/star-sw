#ifndef __SOCDL_H__
#define __SOCDL_H__

#ifdef __cplusplus
extern "C" {
#endif

int soc_dl_load (const char *pkgName);
int soc_dl_init (const char *pkgName);
int soc_dl_start(const char *pkgName);
int soc_dl_stop (const char *pkgName);

#ifdef __cplusplus
}
#endif

#endif /* __SOCDL_H__ */
