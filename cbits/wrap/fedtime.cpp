#include "wrap/fedtime.h"

#include "wrap/rti.h"

#ifdef DLC_API
#   define q_RTIfedTime     rti13::RTIfedTime
#else
#   define q_RTIfedTime     RTIfedTime
#endif

#define invoke(method)  wrap(return ((q_RTIfedTime *)rtiFedTime)->method)


ccall void *wrap_new_RTIfedTime(double t, void **out_exc) {
    wrap (return new q_RTIfedTime(t))
}

ccall void wrap_delete_RTIfedTime(void *rtiFedTime, void **out_exc) {
    wrap(delete ((q_RTIfedTime *)rtiFedTime))
}

ccall double wrap_RTIfedTime_getTime(void *rtiFedTime, void **out_exc) {
    invoke(getTime())
}

ccall void wrap_RTIfedTime_setTime(void *rtiFedTime, double t, void **out_exc) {
    wrap( 
        *((q_RTIfedTime *)rtiFedTime) = t;
    )
}

