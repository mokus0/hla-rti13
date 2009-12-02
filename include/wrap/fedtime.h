#ifndef ___n_fedtime_h__
#define ___n_fedtime_h__

#include "wrap/common.h"

ccall void  *wrap_new_RTIfedTime(double t, void **out_exc);
ccall void   wrap_delete_RTIfedTime(void *rtiFedTime, void **out_exc);
ccall double wrap_getTime(void *rtiFedTime, void **out_exc);
ccall void   wrap_setTime(void *rtiFedTime, double t, void **out_exc);

#endif /* ___n_fedtime_h__ */
