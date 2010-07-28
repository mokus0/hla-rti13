#ifndef ___n_hsFederateAmb_h__
#define ___n_hsFederateAmb_h__

#include "wrap/common.h"

typedef void (*VoidFunc)();
typedef void (*ConstPtr_to_Void)(const void *);
typedef void (*ConstPtrX2_to_Void)(const void *, const void *);
typedef void (*ULong_to_Void)(RTI_ULong);
typedef void (*ULong_to_ConstPtr_to_Void)(RTI_ULong, const void *);
typedef void (*ULong_to_ULong_to_ConstPtr_to_Void)(RTI_ULong, RTI_ULong, const void *);
typedef void (*ULong_to_ConstPtr_to_ConstPtr_to_ULong_to_Void)(RTI_ULong, const void *, const void *, RTI_ULong);
typedef void (*ULong_to_ConstPtrX2_to_ULongX2_to_Void)(RTI_ULong, const void *, const void *, RTI_ULong, RTI_ULong);
typedef void (*ULong_to_ConstPtrX3_to_ULongX2_to_Void)(RTI_ULong, const void *, const void *, const void *, RTI_ULong, RTI_ULong);


ccall void *wrap_new_HsFederateAmbassador(FunPtrFn releaseFunPtr, void **out_exc);
ccall void *wrap_delete_HsFederateAmbassador(void *fedAmb, void **out_exc);

// TODO: eventually make proper declarations so compiling via C will work
// ccall void hsfa_set_turnUpdatesOnForObjectInstance(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr);
// ccall void hsfa_set_timeRegulationEnabled(void *fedAmb, ConstPtr_to_Void theFunPtr);

#endif /* ___n_hsFederateAmb_h__ */
