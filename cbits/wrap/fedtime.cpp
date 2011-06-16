#include "wrap/fedtime.h"

#include "wrap/rti.h"

#define invoke(method)  wrap(return ((RTIfedTime *)rtiFedTime)->method)


ccall void *wrap_new_RTIfedTime(double t, void **out_exc) {
    wrap (return new RTIfedTime(t))
}

ccall void wrap_delete_RTIfedTime(void *rtiFedTime, void **out_exc) {
    wrap(delete ((RTIfedTime *)rtiFedTime))
}

ccall double wrap_RTIfedTime_getTime(void *rtiFedTime, void **out_exc) {
    invoke(getTime())
}

ccall void wrap_RTIfedTime_setTime(void *rtiFedTime, double t, void **out_exc) {
    wrap( 
        *((RTIfedTime *)rtiFedTime) = t;
    )
}

