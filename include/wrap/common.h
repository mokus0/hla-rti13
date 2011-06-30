#ifndef ___n_common_h__
#define ___n_common_h__

#ifdef __cplusplus
#   define ccall extern "C"
    extern "C" {
#       include "HsFFI.h"
        typedef void (*FunPtrFn)(HsFunPtr fn);
    }
#else
#   define ccall 
#   include "HsFFI.h"
#endif

// need to declare this because the C headers need it
#if defined(__alpha) || (defined(__sgi) && _MIPS_SZLONG == 64) || (defined(__sparcv9))  || (defined(__x86_64))
#ifndef RTI_64_BIT_LONG
#define RTI_64_BIT_LONG 1
#endif
typedef unsigned int RTI_ULong;
#else
typedef unsigned long RTI_ULong;
#endif

#define wrap(action) {                  \
    try {                               \
        action;                         \
    } catch (rti13::Exception &e) {     \
        *out_exc = e.cloneSelf();       \
    } catch (...) {                     \
        *out_exc = (void *) -1;         \
    }                                   \
}

#endif /* ___n_common_h__ */
