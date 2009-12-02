#ifndef ___n_BaseTypes_h__
#define ___n_BaseTypes_h__

#include "wrap/common.h"

ccall void delete_Exception(void *exc);
ccall void dissect_Exception(void *exc, RTI_ULong *serial, const char **reason, const char **name);

#endif /* ___n_BaseTypes_h__ */
